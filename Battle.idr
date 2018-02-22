%default total

--------------------------------------------------------------------------------
-- BATTLE!!!
--------------------------------------------------------------------------------

data Input = STUPID

data BattleState : Type where
  Dead : BattleState
  NotDead : (health : Nat) -> BattleState
  
  Ready : BattleState
  Done : BattleState
  ReadError : BattleState
  
data BattleCmd : (ty : Type) -> BattleState -> (ty -> BattleState) -> Type where
  Start : BattleCmd () Dead (const (NotDead 10))
  Die : BattleCmd () (NotDead 0) (const Dead)
  Stupid : BattleCmd () (NotDead (S k)) (const (NotDead k))
  
  ReadState : BattleCmd (Maybe BattleState) Ready
    (\res => case res of
                  Just loadedState => loadedState
                  Nothing => ReadError)
  WriteState : BattleCmd () inState (const Done)
  GiveUp : BattleCmd () ReadError (const Done)
  
  Log : String -> BattleCmd () state (const state)
  
  Pure : (res : ty) -> BattleCmd ty (state_fn res) state_fn
  (>>=) : BattleCmd a state1 state2_fn ->
          ((res : a) -> BattleCmd b (state2_fn res) state3_fn) ->
          BattleCmd b state1 state3_fn
          
--------------------------------------------------------------------------------
-- foreign stuff
--------------------------------------------------------------------------------

log : String -> JS_IO ()
log = putStrLn'

newState : JS_IO ()
newState = foreign FFI_JS "window.badboy = { state: 'DEAD' }" (JS_IO ())

readState : JS_IO (Maybe BattleState)
readState = do stateStr <- foreign FFI_JS "window.badboy.state" (JS_IO String)
               case stateStr of
                 "DEAD" => pure (Just Dead)
                 "NOTDEAD" => do health <- foreign FFI_JS "window.badboy.health" (JS_IO Int)
                                 pure (Just (NotDead (cast health)))
                 _ => pure Nothing
               
writeStateStr : String -> JS_IO ()
writeStateStr = foreign FFI_JS "window.badboy.state = %0" (String -> JS_IO ())

writeState : BattleState -> JS_IO ()
writeState Dead = writeStateStr "DEAD"
writeState (NotDead health) =
  do writeStateStr "NOTDEAD"
     foreign FFI_JS "window.badboy.health = %0" (Int -> JS_IO()) (toIntNat health)
writeState _ = log "nobody knows the trouble I've seen" -- this is fucked up

partial onClick : String -> JS_IO () -> JS_IO ()
onClick selector callback =
  foreign FFI_JS 
    "document.querySelector(%0).addEventListener('click', %1)"
    (String -> JsFn (() -> JS_IO ()) -> JS_IO ())
    selector (MkJsFn (\_ => callback))

--------------------------------------------------------------------------------
          
runCmd : BattleCmd () Ready (const Done) -> JS_IO ()
runCmd = runCmd'
  where
    runCmd' : BattleCmd res inState outState_fn -> JS_IO res
    runCmd' Start = do log "starting..."
    runCmd' Die = log "you died"
    runCmd' Stupid {outState_fn = const (NotDead health)} =
      log ("you did something stupid, health: " ++ show health)
    runCmd' ReadState = readState
    runCmd' WriteState {inState} = writeState inState
    runCmd' GiveUp = log "something went wrong reading the state"
    runCmd' (Log msg) = log msg
    runCmd' (Pure res) = pure res
    runCmd' (x >>= f) = do x' <- runCmd' x
                           runCmd' (f x')

doStupid : BattleCmd () Ready (const Done)
doStupid = do Just state <- ReadState | Nothing => GiveUp 
              case state of
                NotDead (S health) => do Stupid
                                         case health of
                                           Z => do Die
                                                   WriteState
                                           _ => WriteState
                _ => WriteState
                
doStart : BattleCmd () Ready (const Done)
doStart = do Just state <- ReadState | Nothing => GiveUp
             case state of
               Dead => do Start
                          WriteState
               _ => WriteState

partial main : JS_IO ()
main = do newState 
          onClick "#start" (runCmd doStart)
          onClick "#stupid" (runCmd doStupid)
