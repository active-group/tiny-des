{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import qualified Control.Monad.State.Strict as State
import Control.Monad.State.Strict (State)
import qualified Control.Monad.Random as Random
import System.Random (mkStdGen)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Sequence as Sequence
import Data.Sequence (Seq, (|>))
import qualified Data.Heap as Heap
import Data.Heap (MinHeap)
import System.IO.Unsafe (unsafePerformIO)

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

type Condition v = ModelState v -> Bool

trueCondition :: Condition v
trueCondition = \ _ -> True

largerThanValueCondition :: Ord a => String -> a -> Condition a
largerThanValueCondition name value ms =
  let (Just value') = Map.lookup name ms
  in  value' > value
  
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
                                 delay :: Delay v }

transition = Transition { targetEvent = undefined, condition = trueCondition, delay = zeroDelay }

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

instance Show (Event v) where
  show e = "Event { name = " ++ (name e) ++ " }"

event = Event { name = "UNKNOWN", priority = 0, transitions = [], stateChanges = [] }

minimalModel =
  let queue = "Queue"
      serverCapacity = "ServerCapacity"
      serviceTime = constantDelay 6
      interarrivalTime = constantDelay 5
      runEvent = event { name = "RUN",
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
                                           incrementValue queue (-1)] }
      leaveEvent = event { name = "LEAVE",
                           transitions = [leaveToStart],
                           stateChanges = [setValue serverCapacity 1] }
      runToEnter = transition { targetEvent = enterEvent }
      enterToEnter = transition { targetEvent = enterEvent,
                                  delay = interarrivalTime }
      enterToStart = transition { targetEvent = startEvent,
                                  condition = largerThanValueCondition serverCapacity 0 }
      startToLeave = transition { targetEvent = leaveEvent,
                                  delay = serviceTime }
      leaveToStart = transition { targetEvent = startEvent,
                                  condition = largerThanValueCondition queue 0 }
      
  in Model "MinimalModel" runEvent

type Time = Integer

data EventInstance v = EventInstance Time (Event v)
  deriving (Show, Eq)

instance Ord (EventInstance v) where
  compare (EventInstance t1 e1) (EventInstance t2 e2) =
    case compare t1 t2 of
      EQ -> compare (priority e1) (priority e2)
      x -> x

class ReportGenerator r v | r -> v where
  update :: r -> Time -> ModelState v -> r
  writeReport :: r -> IO ()

newtype Clock = Clock { getCurrentTime :: Time }
  deriving Show

type Simulation r v = State.StateT (Clock, MinHeap (EventInstance v), r) (ModelAction v)

setCurrentTime :: Time -> Simulation r v ()
setCurrentTime t = State.modify (\ (_, evs, r) -> (Clock t, evs, r))

getNextEvent :: Simulation r v (EventInstance v)
getNextEvent =
  do (clock, evs, r) <- State.get
     case Heap.view evs of
       Just (ev, evs') ->
         do State.put (clock, evs', r)
            return ev
       Nothing -> fail "can't happen"

updateSystemState :: EventInstance v -> Simulation r v ()
updateSystemState (EventInstance _ ev) = State.lift (sequence_ (stateChanges ev))

-- FIXME: Don't update incrementally, instead do everything based on consistent old state.

updateStatisticalCounters :: ReportGenerator r v => EventInstance v -> Simulation r v ()
updateStatisticalCounters (EventInstance t _) =
  do (clock, evs, r) <- State.get
     ms <- State.lift State.get
     State.put (clock, evs, update r t ms)

generateEvents (EventInstance _ ev) =
  mapM_ (\ tr ->
          do ms <- State.lift getModelState
             if condition tr ms then
               do (clock, evs, r) <- State.get
                  d <- State.lift (delay tr)
                  let evi = EventInstance ((getCurrentTime clock) + d) (targetEvent tr)
                  let evs' = Heap.insert evi evs
                  State.put (clock, evs', r)
             else
               return ())
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
      initialEvent = EventInstance (getCurrentTime clock) (startEvent model)
      eventList = Heap.singleton initialEvent
      loop =
        do (clock, evs, r) <- State.get
           if ((getCurrentTime clock) <= endTime) && not (Heap.null evs) then
             do currentEvent <- timingRoutine
                -- seq (unsafePerformIO (putStrLn (show currentEvent))) (updateSystemState currentEvent)
                updateSystemState currentEvent
                updateStatisticalCounters currentEvent
                generateEvents currentEvent
                loop
           else
             return ()
  in let ma = State.execStateT loop (clock, eventList, reportGenerator)
         (cl', evs, r) = Random.evalRand (State.evalStateT ma Map.empty) (mkStdGen 0)
      in do writeReport r

-- Report generator

data Value = Value {
  valueTime :: Time,
  valueValue :: Maybe Integer,
  valueMin :: Maybe Integer,
  valueMax :: Maybe Integer,
  valueAverage :: Double
}

defaultValue = Value {
  valueMin = Nothing,
  valueMax = Nothing,
  valueAverage = 0.0,
  valueValue = Nothing,
  valueTime = 0
  }
  
updateAvg :: Value -> Time -> Integer -> Double
updateAvg value currentTime currentValue =
  case valueValue value of
    Just v ->
      if currentTime /= 0 then
        ((valueAverage value) * (fromInteger (valueTime value))
         + ((fromInteger currentTime) - (fromInteger (valueTime value))) * (fromInteger v))
        / (fromInteger currentTime)
      else
        valueAverage value
    Nothing -> valueAverage value

valueWithCurrent :: Value -> Time -> Integer -> Value
valueWithCurrent value t v =
  value { valueMin = valueNewMin value v,
          valueMax = valueNewMax value v,
          valueAverage = updateAvg value t v,
          valueTime = t,
          valueValue = Just v }

valueNewMin value current =
  case valueMin value of
    Just m -> Just (min current m)
    Nothing -> Just current

valueNewMax value current =
  max (Just current) (valueMax value)

separator = ";\t"

valueHeader :: String
valueHeader =
  "Time" ++ separator ++ "Value" ++ separator ++ "Min" ++ separator ++ "Max" ++ separator ++ "Avg" ++ separator

instance Show Value where
  show value =
    let sm x = case x of
          Just v -> show v
          Nothing -> "<>"
    in      
      (show (valueTime value)) ++ separator ++
      (sm (valueValue value)) ++ separator ++
      (sm (valueMin value)) ++ separator ++
      (sm (valueMax value)) ++ separator ++
      (show (valueAverage value)) ++ separator
         
data FullReportGenerator = FullReportGenerator {
  reportValues :: Map String Value,
  reportHistory :: Seq (Map String Value)
  } deriving Show

fullReportGenerator = FullReportGenerator { reportValues = Map.empty, reportHistory = Sequence.empty }

instance ReportGenerator FullReportGenerator Integer where
  update frg t ms =
    let vals =
          Map.foldlWithKey (\ vals k v ->
                             case Map.lookup k ms of
                               Nothing -> vals
                               Just current ->
                                 let val = case Map.lookup k vals of
                                             Just value -> value
                                             Nothing -> defaultValue
                                     val' = valueWithCurrent val t current
                                 in Map.insert k val' vals)
             (reportValues frg)
             ms
    in frg { reportValues = vals,
             reportHistory = (reportHistory frg) |> vals }
       
  writeReport frg =
    do putStrLn ("name" ++ separator ++ valueHeader)
       let putValues vs =
             sequence (Map.mapWithKey (\ k v -> putStrLn (k ++ separator ++ (show v))) vs)
       
       mapM_ putValues (reportHistory frg)

