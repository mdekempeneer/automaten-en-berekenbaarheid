
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



action :: DecisionTable -> State -> Char -> (State, Char, Action)
action d s c = head [(s', c', a)|(os, oc, s', c', a) <- d, (==) os s, (==) oc c]

execute :: String -> String -> Action -> Char -> (String, String)
execute s1 s2 a c = case a of L -> ((init s1), (last s1):c:(tail s2))
                              S -> (s1, c:(tail s2))
                              R -> ((s1++[c]), (tail s2))

process :: DecisionTable -> State -> String -> String -> (String, String, State)
process d s s1 s2 = (s1', s2', s') where (s', c, a) = action d s (head s2)
                                         (s1', s2') = execute s1 s2 a c

exampleDT :: DecisionTable
exampleDT = [(1,'a',2,'b',L)]
