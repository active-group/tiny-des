import qualified Control.Monad.State.Strict as State
import Control.Monad.State.Strict (State)
import qualified Control.Monad.Random as Random
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Sequence as Sequence
import Data.Sequence (Seq, (|>))
import qualified Data.Heap as Heap
import Data.Heap (MinHeap)
import System.IO.Unsafe (unsafePerformIO)

type ModelState v = Map String v

-- Model monad
type ModelAction v = State.State (ModelState v)

getModelState :: ModelAction v (ModelState v)
getModelState = State.get

getValue :: String -> ModelAction v v
getValue name =
  do ms <- State.get
     let (Just v) = Map.lookup name ms
     return v

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

type Random = Random.Rand Random.StdGen
      
type Delay = Random Integer

zeroDelay :: Delay
zeroDelay = return 0
constantDelay :: Integer -> Delay
constantDelay v = return v

exponentialDelay :: Double -> Delay
exponentialDelay mean =
  do u <- Random.getRandom
     return (round (-mean * log u))
  
data Transition v = Transition { targetEvent :: Event v,
                                 condition :: Condition v,
                                 delay :: Delay }

transition = Transition { targetEvent = undefined, condition = trueCondition, delay = zeroDelay }

type StateChange v = ModelAction v ()

setValue :: String -> v -> StateChange v
setValue name value =
  do ms <- State.get
     State.put (Map.insert name value ms)

modifyValue :: String -> (v -> v) -> StateChange v
modifyValue name f =
  State.modify (\ ms -> Map.adjust (\ v -> f  v) name ms)

incrementValue :: Num v => String -> v -> StateChange v
incrementValue name inc =
  do ms <- State.get
     let (Just v) = Map.lookup name ms
     setValue name (v + inc)

data Event v = Event { name :: String,
                       priority :: Int,
                       transitions :: [Transition v],
                       stateChanges :: [StateChange v] }

instance Eq (Event v) where
  e1 == e2 = (name e1) == (name e2)

instance Show (Event v) where
  show e = "Event { name = " ++ (name e) ++ " }"

event = Event { name = "UNKNOWN", priority = 0, transitions = [], stateChanges = [] }

minimalModel () = -- () is because we're parameterized over monad
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
