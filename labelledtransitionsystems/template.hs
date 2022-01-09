import Data.List
import Data.Maybe

type Id = String

type State = Int

type Transition = ((State, State), Id)

type LTS = [Transition]

type Alphabet = [Id]

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process] 
             deriving (Eq, Show)

type ProcessDef = (Id, Process)

type StateMap = [((State, State), State)]

------------------------------------------------------
-- PART I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: The item is in the table
lookUp
  = (fromJust .) . lookup

states :: LTS -> [State]
states lts
  = nub $ states' lts
  where
    states' :: LTS -> [State]
    states' []
      = []
    states' (((s, s'), _) : xs)
      = s : s' : (states' xs)

transitions :: State -> LTS -> [Transition]
transitions s 
  = filter ((==s) . fst . fst) 

alphabet :: LTS -> Alphabet
alphabet 
  = nub . map snd

------------------------------------------------------
-- PART II

actions :: Process -> [Id]
actions STOP
  = []
actions (Ref p)
  = []
actions (Prefix a p)
  = nub $ a : actions p
actions (Choice ps)
  = nub $ concatMap actions ps

accepts :: [Id] -> [ProcessDef] -> Bool
--Pre: The first item in the list of process definitions is
--     that of the start process.
accepts ids pdefs@((_, p) : _) 
  = accepts' ids p
  where
    accepts' :: [Id] -> Process -> Bool
    accepts' [] _
      = True
    accepts' xs STOP
      = False
    accepts' xs (Ref p)
      = accepts' xs $ lookUp p pdefs
    accepts' (x : xs) (Prefix a p)
      | a == x    = accepts' xs p
      | otherwise = False
    accepts' xs (Choice ps)
      = any (accepts' xs) ps

------------------------------------------------------
-- PART III

composeTransitions :: Transition -> Transition 
                   -> Alphabet -> Alphabet 
                   -> StateMap 
                   -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions ((s, t), a) ((s', t'), a') k k' m 
  | a == a'                    = [((iss, itt), a)] 
  | (elem a k') && (elem a' k) = []
  | (elem a' k)                = [((iss, its), a)]
  | (elem a k')                = [((iss, ist), a')]
  | otherwise                  = [((iss, its), a), ((iss, ist), a')]
  where
    iss = lookUp (s, s') m
    itt = lookUp (t, t') m
    its = lookUp (t, s') m
    ist = lookUp (s, t') m

pruneTransitions :: [Transition] -> LTS
pruneTransitions ts
  = filter ((flip elem) (visit 0 [])) ts
  where
    visit :: State -> [State] -> [Transition]
    visit s xs
      | (elem s xs) = []
      | otherwise = concatMap (\t@((from, to), a) -> t : visit to (from : xs)) ts' 
      where
        ts' = transitions s ts

------------------------------------------------------
-- PART IV

compose :: LTS -> LTS -> LTS
compose 
  = undefined

------------------------------------------------------
-- PART V

buildLTS :: [ProcessDef] -> LTS
-- Pre: All process references (Ref constructor) have a corresponding
--      definition in the list of ProcessDefs.
buildLTS 
  = undefined

------------------------------------------------------
-- Sample process definitions...

vendor, clock, play, maker, user, p, q, switch, off, on :: ProcessDef

vendor 
  = ("VENDOR", Choice [Prefix "red"  (Prefix "coffee" (Ref "VENDOR")),
                       Prefix "blue" (Prefix "tea" (Ref "VENDOR")),
                       Prefix "off" STOP])

clock 
  = ("CLOCK", Prefix "tick" (Prefix "tock" (Ref "CLOCK")))

play 
  = ("PLAY", Choice [Prefix "think" (Prefix "move" (Ref "PLAY")), 
                     Prefix "end" STOP])

maker 
  = ("MAKER", Prefix "make" (Prefix "ready" (Ref "MAKER")))

user  
  = ("USER",  Prefix "ready" (Prefix "use" (Ref "USER")))

p = ("P", Prefix "a" (Prefix "b" (Prefix "c" STOP)))

q = ("Q",  Prefix "d" (Prefix "c" (Prefix "b" (Ref "Q"))))

switch 
  = ("SWITCH", Ref "OFF")

off 
  = ("OFF", Choice [Prefix "on" (Ref "ON")])

on  
  = ("ON",  Choice [Prefix "off" (Ref "OFF")])

------------------------------------------------------
-- Sample LTSs...

vendorLTS, clockLTS, playLTS, clockPlayLTS, makerLTS, userLTS, makerUserLTS, 
  pLTS, qLTS, pqLTS, switchLTS :: LTS

vendorLTS 
  = [((0,1),"off"),((0,2),"blue"),((0,3),"red"),((2,0),"tea"),((3,0),"coffee")]

clockLTS 
  = [((0,1),"tick"),((1,0),"tock")]

playLTS 
  = [((0,1),"end"),((0,2),"think"),((2,0),"move")]

clockPlayLTS 
  = [((0,1),"end"),((1,4),"tick"),((4,1),"tock"),((0,3),"tick"),
     ((3,4),"end"),((3,0),"tock"),((3,5),"think"),((5,3),"move"),
     ((5,2),"tock"),((2,0),"move"),((2,5),"tick"),((0,2),"think")]

makerLTS 
  = [((0,1),"make"),((1,0),"ready")]

userLTS 
  = [((0,1),"ready"),((1,0),"use")]

makerUserLTS 
  = [((0,2),"make"),((2,1),"ready"),((1,0),"use"),((1,3),"make"),((3,2),"use")]

pLTS 
  = [((0,1),"a"),((1,2),"b"),((2,3),"c")]

qLTS 
  = [((0,1),"d"),((1,2),"c"),((2,0),"b")]

pqLTS 
  = [((0,1),"d"),((1,4),"a"),((0,3),"a"),((3,4),"d")]

switchLTS 
  = [((0,1),"on"),((1,0),"off")]

