%default total

--------------------------------------------------------------------------------
-- BATTLE!!!
--------------------------------------------------------------------------------

data Input = KICK | PUNCH | MAGIC

namespace Enemy
  data EnemyResponse = Kick | Punch | Magic

data BattleState : Type where
  Dead : BattleState
  NotDead : (health : Nat) -> (enemy : Nat) -> BattleState
  Won : BattleState
  Safe : BattleState
  
  Ready : BattleState
  Done : BattleState
  ReadError : BattleState
  
data Writable : BattleState -> Type where
  WDead : Writable Dead
  WNotDead : Writable (NotDead health enemy)
  WWon : Writable Won
  WSafe : Writable Safe
  
Show (Writable state) where
  show WDead = "you are dead"
  show (WNotDead {health} {enemy}) = "you are alive, health: " ++ show health ++ ", enemy: " ++ show enemy
  show WWon = "you won!"
  show WSafe = "you are safe"
  
data BattleCmd : (ty : Type) -> BattleState -> (ty -> BattleState) -> Type where
  Start : BattleCmd () Safe (const (NotDead 10 10))
  Die : BattleCmd () (NotDead 0 _) (const Dead)
  Win : BattleCmd () (NotDead (S health) 0) (const Won)
  
  Kick : BattleCmd EnemyResponse (NotDead (S health) (S enemy))
    (\res => case res of
                  Kick => NotDead (S health) (S enemy)
                  Punch => NotDead (S health) enemy
                  Magic => NotDead health (S enemy)) 
                  
  Punch : BattleCmd EnemyResponse (NotDead (S health) (S enemy))
    (\res => case res of
                  Kick => NotDead health (S enemy)
                  Punch => NotDead (S health) (S enemy)
                  Magic => NotDead (S health) enemy)
                  
  Magic : BattleCmd EnemyResponse (NotDead (S health) (S enemy))
    (\res => case res of
                  Kick => NotDead (S health) enemy 
                  Punch => NotDead health (S enemy)
                  Magic => NotDead (S health) (S enemy))
                  
  ReadState : BattleCmd (Maybe (a : BattleState ** Writable a)) Ready
    (\res => case res of
                  Just (MkDPair loadedState _) => loadedState
                  Nothing => ReadError)
  WriteState : (w : Writable inState) -> BattleCmd () inState (const Done)
  GiveUp : BattleCmd () ReadError (const Done)
  ShowStatus : (w : Writable state) -> BattleCmd () state (const state)
  
  Log : String -> BattleCmd () state (const state)
  Pure : (res : ty) -> BattleCmd ty (state_fn res) state_fn
  (>>=) : BattleCmd a state1 state2_fn ->
          ((res : a) -> BattleCmd b (state2_fn res) state3_fn) ->
          BattleCmd b state1 state3_fn
          
--------------------------------------------------------------------------------
-- foreign stuff
--------------------------------------------------------------------------------

newState : JS_IO ()
newState = foreign FFI_JS "gameState.newState()" (JS_IO ())

readState : JS_IO (Maybe (a : BattleState ** Writable a))
readState = do stateStr <- foreign FFI_JS "gameState.readState()" (JS_IO String)
               case stateStr of
                 "DEAD" => pure (Just (Dead ** WDead))
                 "WON" => pure (Just (Won ** WWon))
                 "SAFE" => pure (Just (Safe ** WSafe))
                 "NOTDEAD" => do health <- foreign FFI_JS "gameState.readHealth()" (JS_IO Int)
                                 enemy <- foreign FFI_JS "gameState.readEnemy()" (JS_IO Int)
                                 pure (Just (NotDead (cast health) (cast enemy) ** WNotDead))
                 _ => pure Nothing
               
writeStateStr : String -> JS_IO ()
writeStateStr = foreign FFI_JS "gameState.writeStateStr(%0)" (String -> JS_IO ())

writeState : Writable state -> JS_IO ()
writeState w {state = Dead} = writeStateStr "DEAD"
writeState w {state = Won} = writeStateStr "WON"
writeState w {state = Safe} = writeStateStr "SAFE"
writeState w {state = (NotDead health enemy)} =
  do writeStateStr "NOTDEAD"
     foreign FFI_JS "gameState.writeHealth(%0)" (Int -> JS_IO()) (toIntNat health)
     foreign FFI_JS "gameState.writeEnemy(%0)" (Int -> JS_IO()) (toIntNat enemy)

partial onClick : String -> JS_IO () -> JS_IO ()
onClick selector callback =
  foreign FFI_JS 
    "onClick(%0, %1)"
    (String -> JsFn (() -> JS_IO ()) -> JS_IO ())
    selector (MkJsFn (\_ => callback))
    
partial onInit : JS_IO () -> JS_IO ()
onInit callback =
  foreign FFI_JS
    "onInit(%0)"
    ((JsFn (() -> JS_IO ())) -> JS_IO ())
    (MkJsFn (\_ => callback))
    
enemyResponse : JS_IO EnemyResponse
enemyResponse = 
  do i <- foreign FFI_JS "enemyResponse()" (JS_IO Int)
     case i of
       1 => pure Punch
       2 => pure Magic
       _ => pure Kick
       
showStatus : Writable state -> JS_IO ()
showStatus w = 
  foreign FFI_JS 
    "showStatus(%0)"
    (String -> JS_IO ())
    (show w)
       
log : String -> JS_IO ()
log = foreign FFI_JS "log(%0)" (String -> JS_IO ())
--------------------------------------------------------------------------------
          
runCmd : BattleCmd () Ready (const Done) -> JS_IO ()
runCmd = runCmd'
  where
    runCmd' : BattleCmd ty inState outState -> JS_IO ty
    runCmd' Start = pure ()
    runCmd' Die = pure ()
    runCmd' Win = pure ()
    
    runCmd' Punch = enemyResponse
    runCmd' Kick = enemyResponse
    runCmd' Magic = enemyResponse
    
    runCmd' ReadState = readState
    runCmd' (WriteState w) = writeState w
    runCmd' GiveUp = putStrLn' "something went wrong reading the state"
    runCmd' (ShowStatus w) = showStatus w
    runCmd' (Log msg) = log msg
    runCmd' (Pure res) = pure res
    runCmd' (x >>= f) = do x' <- runCmd' x
                           runCmd' (f x')

doStart : BattleCmd () Ready (const Done)
doStart = do Just (state ** w) <- ReadState | Nothing => GiveUp
             case state of
               Safe => do Log "An enemy appeared! Battle start!"
                          Start
                          ShowStatus WNotDead
                          WriteState WNotDead
               _ => WriteState w

doKick : BattleCmd () Ready (const Done)
doKick =
  do Just (state ** w) <- ReadState | Nothing => GiveUp
     case state of
          NotDead (S health) (S enemy) => do Log "you kicked!"
                                             res <- Kick
                                             case res of
                                                  Kick => do Log "enemy kicked!"
                                                             ShowStatus WNotDead 
                                                             WriteState WNotDead
                                                  Punch => do Log "enemy punched!"
                                                              case enemy of
                                                                   Z => do Win 
                                                                           Log "enemy kicked to death"
                                                                           ShowStatus WWon
                                                                           WriteState WWon
                                                                   _ => do ShowStatus WNotDead
                                                                           WriteState WNotDead 
                                                  Magic => do Log "enemy used magic!"
                                                              case health of
                                                                   Z => do Die
                                                                           Log "you died :("
                                                                           Log "RIP player, fatally magicked"
                                                                           ShowStatus WDead
                                                                           WriteState WDead
                                                                   _ => do ShowStatus WNotDead
                                                                           WriteState WNotDead
          _ => WriteState w

doPunch : BattleCmd () Ready (const Done)
doPunch =
  do Just (state ** w) <- ReadState | Nothing => GiveUp
     case state of
          NotDead (S health) (S enemy) => do Log "you punched!"
                                             res <- Punch
                                             case res of
                                                  Punch => do Log "enemy punched!"
                                                              ShowStatus WNotDead 
                                                              WriteState WNotDead
                                                  Magic => do Log "enemy used magic!"
                                                              case enemy of
                                                                   Z => do Win 
                                                                           Log "enemy punched to death"
                                                                           ShowStatus WWon
                                                                           WriteState WWon
                                                                   _ => do ShowStatus WNotDead
                                                                           WriteState WNotDead 
                                                  Kick => do Log "enemy kicked!"
                                                             case health of
                                                                  Z => do Die
                                                                          Log "you died :("
                                                                          Log "RIP player, received an absolute kicking" 
                                                                          ShowStatus WDead
                                                                          WriteState WDead
                                                                  _ => do ShowStatus WNotDead
                                                                          WriteState WNotDead
          _ => WriteState w
          
doMagic : BattleCmd () Ready (const Done)
doMagic =
  do Just (state ** w) <- ReadState | Nothing => GiveUp
     case state of
          NotDead (S health) (S enemy) => do Log "you used magic!"
                                             res <- Magic
                                             case res of
                                                  Magic => do Log "enemy used magic!"
                                                              ShowStatus WNotDead 
                                                              WriteState WNotDead
                                                  Kick => do Log "enemy kicked!"
                                                             case enemy of
                                                                  Z => do Win 
                                                                          Log "enemy magicked to death"
                                                                          ShowStatus WWon
                                                                          WriteState WWon
                                                                  _ => do ShowStatus WNotDead
                                                                          WriteState WNotDead 
                                                  Punch => do Log "enemy punched!"
                                                              case health of
                                                                   Z => do Die
                                                                           Log "you died :("
                                                                           Log "RIP player, punched to death" 
                                                                           ShowStatus WDead
                                                                           WriteState WDead
                                                                   _ => do ShowStatus WNotDead
                                                                           WriteState WNotDead
          _ => WriteState w

actions : Input -> BattleCmd () Ready (const Done)
actions KICK = doKick
actions PUNCH = doPunch
actions MAGIC = doMagic
 
doAction : Input -> JS_IO ()
doAction = runCmd . actions

partial setUp : JS_IO ()
setUp = do newState
           runCmd doStart
           onClick "#kick" (doAction KICK)
           onClick "#punch" (doAction PUNCH) 
           onClick "#magic" (doAction MAGIC) 

partial main : JS_IO ()
main = onInit setUp
