{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.State.Strict as State
import Control.Monad.State.Strict (State)
import Control.Monad.Random as Random
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Queue as Queue

type ModelState v = Map String v

-- Simulation monad
type Simulation' v = Random.RandT StdGen (State (ModelState v))

-- v1
type Simulation v r = State.StateT (ModelState v, r) (Rand StdGen)

-- v2

getValue :: String -> Simulation v r v
getValue name =
  do (ms, _) <- State.get
     let (Just v) = Map.lookup name ms
     return v

setValue :: String -> v -> Simulation v r ()
setValue name value =
  do (ms, r) <- State.get
     State.put (Map.insert name value ms, r)

modifyValue :: String -> (v -> v) -> Simulation v r ()
modifyValue name f =
  State.modify (\ (ms, r) -> (Map.adjust (\ v -> f  v) name ms, r))
  
data Model v r = Model String (Event v r)

type Condition v r = Simulation v r Bool

trueCondition :: Condition v r
trueCondition = return True

largerThanValueCondition :: Ord a => String -> a -> Condition a r
largerThanValueCondition name value =
  do value' <- getValue name
     return (value' > value)

type Delay v r = Simulation v r Integer

zeroDelay :: Delay v r
zeroDelay = return 0
constantDelay :: Integer -> Delay v r
constantDelay v = return v

exponentialDelay :: Double -> Delay v r
exponentialDelay mean =
  do u <- getRandom
     return (round (-mean * log u))
  
data Transition v r = Transition { targetEvent :: Event v r,
                                   condition :: Condition v r,
                                   delay :: Delay v r,
                                   inhibiting :: Bool }

transition = Transition { targetEvent = undefined, condition = trueCondition, delay = zeroDelay, inhibiting = False }

type StateChange v = Simulation v ()

incrementValue :: Num a => String -> a -> Simulation a r ()
incrementValue name inc =
  modifyValue name ((+) inc)
  

data Event v r = Event { name :: String,
                         priority :: Int,
                         transitions :: [Transition v r],
                         stateChanges :: [StateChange v r] }

instance Eq (Event v r) where
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

data EventInstance v r = EventInstance (Event v r) Time
  deriving Eq

instance Ord (EventInstance v r) where
  compare (EventInstance e1 t1) (EventInstance e2 t2) =
    case compare t1 t2 of
      EQ -> compare (priority e1) (priority e2)
      x -> x
