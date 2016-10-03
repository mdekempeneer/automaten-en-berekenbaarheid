
-- Declare datas for Machine
data Action = L | S | R deriving (Show, Read)

-- Declare types for Machine
type State = Int
type States = Int
type Alphabet = [Char]
type DecisionTable = [(State, Char, State, Char, Action)]

type Input = String
data Acceptance = Accept | Reject deriving (Show, Read)

-- Declare important States
type Start = Int
type Accept = Int
type Refuse = Int

-- Declare a Machine
data Machine = Machine DecisionTable Start Accept Refuse
                       deriving (Show, Read)

action :: DecisionTable -> State -> Char -> (State, Char, Action)
action d s c = head [(s', c', a)|(os, oc, s', c', a) <- d, (==) os s, (==) oc c]

execute :: String -> String -> Action -> Char -> (String, String)
execute s1 s2 action ch = case action of L -> ((init s1), (last s1):ch:(tail s2))
                                         S -> (s1, ch:(tail s2))
                                         R -> ((s1++[ch]), (tail s2))

process :: Machine -> State -> Input -> Input -> Acceptance
process m s "" i2 = process m s "#" i2
process m s i1 "" = process m s i1 "#"
process (Machine decisions start accept reject) state a b
  | (==) state accept = Accept
  | (==) state reject = Reject
  | otherwise = process (Machine decisions start accept reject) s' a' b'
                where  (s', c, act) = action decisions state (head b)
                       (a', b')     = execute a b act c

determine :: Machine -> Input -> Acceptance
determine (Machine decisions start accept reject) input =
  process (Machine decisions start accept reject) start ("") input

-- Gives a basic DT to decide {0^n1^n|n>=0}. The start state is 0, acceptance
-- state is 6 and the trash state is 5. See "Automaten en Berekenbaarheid"
-- page 88 for a graphical representation of this Turing machine.
decisions :: DecisionTable
decisions =  [  (0, '#', 6, '#', S),
                (0, '1', 5, '1', S),
                (0, '0', 1, '#', R),
                (1, '#', 5, '#', S),
                (1, '1', 2, '1', R),
                (1, '0', 1, '0', R),
                (2, '#', 3, '#', L),
                (2, '1', 2, '1', R),
                (2, '0', 5, '0', S),
                (3, '#', 5, '#', S),
                (3, '1', 4, '#', L),
                (3, '0', 5, '0', R),
                (4, '#', 0, '1', R),
                (4, '1', 4, '1', L),
                (4, '0', 4, '0', L)   ]

-- Gives a basic Turing machine with the decision table above to decide {0^n1^n|n>=0}.
-- The start state is 0, acceptance state is 6 and the trash state is 5. See
-- "Automaten en Berekenbaarheid" page 99 for a graphical reprentation of this
-- Turing machine.
basicMachine :: Machine
basicMachine = Machine decisions 0 6 5
