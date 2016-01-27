{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
import qualified Control.Monad.State.Strict as State
import Control.Monad.State.Strict (State)
import qualified Control.Monad.Random as Random
import System.Random (mkStdGen)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Sequence as Sequence
import Data.Sequence (Seq, (|>), ViewL ((:<)))

type ModelState v = Map String v

-- Model monad
type ModelAction v = State.StateT (ModelState v) (Random.Rand Random.StdGen)

getModelState :: ModelAction v (ModelState v)
getModelState = State.get

getValue :: String -> ModelAction v v
getValue name =
  do ms <- State.get
     let (Just v) = Map.lookup name ms
     return v

setValue :: String -> v -> ModelAction v ()
setValue name value =
  do ms <- State.get
     State.put (Map.insert name value ms)

modifyValue :: String -> (v -> v) -> ModelAction v ()
modifyValue name f =
  State.modify (\ ms -> Map.adjust (\ v -> f  v) name ms)
  
data Model v = Model {
  modelName :: String,
  startEvent :: Event v
  }

type Condition v = ModelAction v Bool

trueCondition :: Condition v
trueCondition = return True

largerThanValueCondition :: Ord a => String -> a -> Condition a
largerThanValueCondition name value =
  do value' <- getValue name
     return (value' > value)

type Delay v = ModelAction v Integer

zeroDelay :: Delay v
zeroDelay = return 0
constantDelay :: Integer -> Delay Integer
constantDelay v = return v

exponentialDelay :: Double -> Delay v
exponentialDelay mean =
  do u <- Random.getRandom
     return (round (-mean * log u))
  
data Transition v = Transition { targetEvent :: Event v,
                                 condition :: Condition v,
                                 delay :: Delay v,
                                 inhibiting :: Bool }

transition = Transition { targetEvent = undefined, condition = trueCondition, delay = zeroDelay, inhibiting = False }

type StateChange v = ModelAction v ()

incrementValue :: Num a => String -> a -> ModelAction a ()
incrementValue name inc =
  modifyValue name ((+) inc)
  

data Event v = Event { name :: String,
                       priority :: Int,
                       transitions :: [Transition v],
                       stateChanges :: [StateChange v] }

instance Eq (Event v) where
  e1 == e2 = (name e1) == (name e2)

event = Event { name = "UNKNOWN", priority = 0, transitions = [], stateChanges = [] }

minimalModel =
  let queue = "Queue"
      serverCapacity = "ServerCapacity"
      serviceTime = constantDelay 6
      interarrivalTime = constantDelay 5
      runEvent = event { name = "Run",
                         transitions = [runToEnter],
                         stateChanges = [setValue serverCapacity 1,
                                         setValue queue 0] }
      enterEvent = event { name = "ENTER",
                           transitions = [enterToEnter, enterToStart],
                           stateChanges = [incrementValue queue 1] }
      startEvent = event { name = "START",
                           priority = -1,
                           transitions = [startToLeave],
                           stateChanges = [setValue serverCapacity 0,
                                           incrementValue queue 1] }
      leaveEvent = event { name = "LEAVE",
                           transitions = [leaveToStart],
                           stateChanges = [setValue serverCapacity 1] }
      runToEnter = transition { targetEvent = startEvent,
                                condition = largerThanValueCondition serverCapacity 0 }
      enterToEnter = transition { targetEvent = enterEvent,
                                  delay = serviceTime }
      enterToStart = transition { targetEvent = startEvent,
                                  condition = largerThanValueCondition serverCapacity 0 }
      startToLeave = transition { targetEvent = leaveEvent,
                                  delay = serviceTime }
      leaveToStart = transition { targetEvent = startEvent,
                                  condition = largerThanValueCondition queue 0 }
      
  in Model "MinimalModel" runEvent

type Time = Integer

data EventInstance v = EventInstance Time (Event v)

  deriving Eq

instance Ord (EventInstance v) where
  compare (EventInstance t1 e1) (EventInstance t2 e2) =
    case compare t1 t2 of
      EQ -> compare (priority e1) (priority e2)
      x -> x

class ReportGenerator r v | r -> v where
  update :: r -> Time -> ModelState v -> r
  writeReport :: r -> IO ()

newtype Clock = Clock { getCurrentTime :: Time }

type Simulation r v = State.StateT (Clock, Seq (EventInstance v), r) (ModelAction v)

setCurrentTime :: Time -> Simulation r v ()
setCurrentTime t = State.modify (\ (_, evs, r) -> (Clock t, evs, r))

getNextEvent :: Simulation r v (EventInstance v)
getNextEvent =
  do (clock, evs, r) <- State.get
     let ev :< evs' = Sequence.viewl evs
     State.put (clock, evs', r)
     return ev

updateSystemState :: EventInstance v -> Simulation r v ()
updateSystemState (EventInstance _ ev) = State.lift (sequence_ (stateChanges ev))

updateStatisticalCounters :: ReportGenerator r v => EventInstance v -> Simulation r v ()
updateStatisticalCounters (EventInstance t _) =
  do (clock, evs, r) <- State.get
     ms <- State.lift State.get
     State.put (clock, evs, update r t ms)

generateEvents (EventInstance _ ev) =
  mapM_ (\ tr ->
          do t <- State.lift (condition tr)
             (clock, evs, r) <- State.get
             d <- State.lift (delay tr)
             let evi = EventInstance ((getCurrentTime clock) + d) (targetEvent tr)
             let evs' = evs |> evi
             State.put (clock, evs', r))
        (transitions ev)

timingRoutine :: Simulation r v (EventInstance v)
timingRoutine =
  do result <- getNextEvent
     let (EventInstance t e) = result
     setCurrentTime t
     return result

runSimulation :: ReportGenerator r v => Model v -> Time -> r -> IO ()
runSimulation model endTime reportGenerator =
  let clock = Clock 0
      modelState = Map.empty
      initialEvent = EventInstance (getCurrentTime clock) (startEvent model)
      eventList = Sequence.singleton initialEvent
      state0 = (clock, eventList)
      loop =
        do (clock, evs, r) <- State.get
           if ((getCurrentTime clock) <= endTime) && not (Sequence.null evs) then
             do currentEvent <- timingRoutine
                generateEvents currentEvent
                loop
           else
             return ()
  in let ma = State.execStateT loop (clock, eventList, reportGenerator)
         (_, _, r) = Random.evalRand (State.evalStateT ma Map.empty) (mkStdGen 0)
      in writeReport r
