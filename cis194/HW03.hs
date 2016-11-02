module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend state name value = \key -> if key == name then value else state key

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

applyBinOp :: State -> (Int -> Int -> a) -> Expression -> Expression -> a
applyBinOp s op left right = (evalE s left) `op` (evalE s right)

applyBinBoolOp :: State -> (Int -> Int -> Bool) -> Expression -> Expression -> Int
applyBinBoolOp s op l r = boolToInt $ applyBinOp s op l r

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

intToBool :: Int -> Bool
intToBool 0 = False
intToBool _ = True

evalE :: State -> Expression -> Int
evalE _ (Val x) = x
evalE s (Var v) = s v
evalE s (Op l Plus r) = applyBinOp s (+) l r
evalE s (Op l Minus r) = applyBinOp s (-) l r
evalE s (Op l Times r) = applyBinOp s (*) l r
evalE s (Op l Divide r) = applyBinOp s (div) l r
evalE s (Op l Gt r) = applyBinBoolOp s (>) l r
evalE s (Op l Ge r) = applyBinBoolOp s (>=) l r
evalE s (Op l Lt r) = applyBinBoolOp s (<) l r
evalE s (Op l Le r) = applyBinBoolOp s (<=) l r
evalE s (Op l Eql r) = applyBinBoolOp s (==) l r

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign s e) = DAssign s e
desugar (Incr s) = DAssign s (Op (Var s) Plus (Val 1))
desugar (If e sTrue sFalse) = DIf e (desugar sTrue) (desugar sFalse)
desugar (While e s) = DWhile e (desugar s)
desugar (For initialization condition update body) =
    DSequence (desugar initialization)
              (DWhile condition (DSequence (desugar body) (desugar update)))
desugar (Sequence s1 s2) = DSequence (desugar s1) (desugar s2)
desugar Skip = DSkip


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple s (DAssign name expr) = extend s name (evalE s expr)

evalSimple s (DIf condition trueStatement falseStatement) =
    if (intToBool $ evalE s condition)
    then evalSimple s trueStatement
    else evalSimple s falseStatement

evalSimple s whileStatement@(DWhile condition body) =
    if (intToBool $ evalE s condition)
    then evalSimple s (DSequence body whileStatement)
    else s

evalSimple s (DSequence first second) = evalSimple (evalSimple s first) second
evalSimple s DSkip = s

run :: State -> Statement -> State
run state statement = evalSimple state (desugar statement)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
