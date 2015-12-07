
-- Don't look at it! it's still ugly and just an idea

-- Declare datas for Machine
data Action = L | S | R deriving (Show, Read)

-- Declare types for Machine
type State = Int
type States = Int
type Alphabet = [Char]
type DecisionTable = [(State, Char, State, Char, Action)]

-- Declare important States
type Start = Int
type Accept = Int
type Refuse = Int

-- Declare a Machine
data Machine = Machine States Alphabet DecisionTable Start Accept Refuse
                       deriving (Show, Read)












-- Given DT, State and Read-char, return new State, Write-char and LSR Action.
action :: DecisionTable -> State -> Char -> (State, Char, Action)
action d s c = head [(s', c', a)|(os, oc, s', c', a) <- d, (==) os s, (==) oc c]

-- Execute the write operation followed by a LSR Action.
execute :: String -> String -> Action -> Char -> (String, String)
execute s1 s2 action ch = case action of L -> ((init s1), (last s1):ch:(tail s2))
                                         S -> (s1, ch:(tail s2))
                                         R -> ((s1++[ch]), (tail s2))

-- Given DT, State, Left- and Rightstring, Return new State and Strings.
process :: DecisionTable -> State -> String -> String -> (String, String, State)
process d s s1 s2 = (s1', s2', s') where (s', c, a) = action d s (head s2)
                                         (s1', s2') = execute s1 s2 a c

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
-- "Automaten en Berekenbaarheid" page 99 for a geaphical reprentation of this
-- Turing machine.
basicMachine :: Machine
basicMachine :: Machine 7 ['0','1'] decisions 0 6 5
