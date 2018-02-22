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
  
data BattleCmd : (ty : Type) -> BattleState -> (ty -> BattleState) -> Type where
  Start : BattleCmd () Dead (const (NotDead 10))
  Die : BattleCmd () (NotDead 0) (const Dead)
  Stupid : BattleCmd () (NotDead (S k)) (const (NotDead k))
  
  ReadState : BattleCmd BattleState Ready (\x => x) -- moves to the loaded state
  WriteState : BattleCmd () inState (const Done)
  
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
newState = foreign FFI_JS "window.badboy = { value: 12 }" (JS_IO ())

readState : JS_IO BattleState
readState = do i <- foreign FFI_JS "(function () { return window.badboy.value; } )()" (JS_IO Int)
               pure (NotDead (cast i))
               
writeState : BattleState -> JS_IO ()
writeState (NotDead health) =
  foreign FFI_JS "window.badboy.value = %0" (Int -> JS_IO ()) (toIntNat health)
writeState _ = log "nobody knows the trouble I've seen" -- this is fucked up

partial onClick : String -> JS_IO () -> JS_IO ()
onClick selector callback =
  foreign FFI_JS 
    "document.querySelector(%0).addEventListener('click', %1)"
    (String -> JsFn (() -> JS_IO ()) -> JS_IO ())
    selector (MkJsFn (\_ => callback))

--------------------------------------------------------------------------------
          
testCmd : BattleCmd () Ready (const Done)
testCmd = do state <- ReadState
             case state of
               NotDead (S health) => do Stupid
                                        WriteState
               _ => WriteState

runCmd : BattleCmd () Ready (const Done) -> JS_IO ()
runCmd = runCmd'
  where
    runCmd' : BattleCmd res inState outState_fn -> JS_IO res
    runCmd' Start = do log "starting..."
    runCmd' Die = log "you died"
    runCmd' Stupid = log "you did something stupid"
    runCmd' ReadState = readState
    runCmd' WriteState {inState} = writeState inState
    runCmd' (Log msg) = log msg
    runCmd' (Pure res) = pure res
    runCmd' (x >>= f) = do x' <- runCmd' x
                           runCmd' (f x')

doStupid : BattleCmd () Ready (const Done)
doStupid = do state <- ReadState
              case state of
                NotDead (S health) => do Stupid
                                         WriteState
                _ => WriteState

partial main : JS_IO ()
main = do newState 
          onClick "#dangermouse" (runCmd doStupid)
