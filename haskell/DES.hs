{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.State.Strict as State
import Control.Monad.State.Strict (State)
import Control.Monad.Random as Random
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Sequence as Sequence

type ModelState v = Map String v

-- Simulation monad
type Simulation' v = Random.RandT StdGen (State (ModelState v))

-- v1
type Simulation v = State.StateT (ModelState v) (Rand StdGen)

getModelState :: Simulation v (ModelState v)
getModelState = State.get

getValue :: String -> Simulation v v
getValue name =
  do ms <- State.get
     let (Just v) = Map.lookup name ms
     return v

setValue :: String -> v -> Simulation v ()
setValue name value =
  do ms <- State.get
     State.put (Map.insert name value ms)

modifyValue :: String -> (v -> v) -> Simulation v ()
modifyValue name f =
  State.modify (\ ms -> Map.adjust (\ v -> f  v) name ms)
  
data Model v = Model {
  modelName :: String,
  startEvent :: Event v
  }

type Condition v = Simulation v Bool

trueCondition :: Condition v
trueCondition = return True

largerThanValueCondition :: Ord a => String -> a -> Condition a
largerThanValueCondition name value =
  do value' <- getValue name
     return (value' > value)

type Delay v = Simulation v Integer

zeroDelay :: Delay v
zeroDelay = return 0
constantDelay :: Integer -> Delay Integer
constantDelay v = return v

exponentialDelay :: Double -> Delay v
exponentialDelay mean =
  do u <- getRandom
     return (round (-mean * log u))
  
data Transition v = Transition { targetEvent :: Event v,
                                 condition :: Condition v,
                                 delay :: Delay v,
                                 inhibiting :: Bool }

transition = Transition { targetEvent = undefined, condition = trueCondition, delay = zeroDelay, inhibiting = False }

type StateChange v = Simulation v ()

incrementValue :: Num a => String -> a -> Simulation a ()
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

class ReportGenerator r v where
  update :: r -> Time -> ModelState v -> r
  writeReport :: r -> IO ()

newtype Clock = Clock { getCurrentTime :: Time }

runSimulation model endTime reportGenerator =
  let clock = Clock 0
      modelState = Map.empty
      initialEvent = EventInstance (getCurrentTime clock) (startEvent model)
      eventList = Sequence.singleton initialEvent
  in undefined
