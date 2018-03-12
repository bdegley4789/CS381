-- | Team Members: Bryce Egley ONID: egleyb, Kenneth Price ONID: pricek, Kenneth Thompson ONID: thomkenn
module StackLang where


import Prelude hiding (Num)


--
-- * Syntax of StackLang
--

-- Grammar for StackLang:
--
--    num ::= (any integer)
--   bool ::= `true`  |  `false`
--   prog ::= cmd*
--    cmd ::= num                         push a number on the stack
--         |  bool                        push a boolean on the stack
--         |  `add`                       add the top two numbers the stack
--         |  `equ`                       check whether the top two elements are equal
--         |  `if` prog `else` prog `end` if the value on the top is true
--                                        then run the first program, else run the second


-- 1. Encode the above grammar as a set of Haskell data types

type Num = Int

type Prog = [Cmd]

data Cmd = PushN Num
         | PushB Bool
         | Add
         | Equ
         | IfThenElse Prog Prog
  deriving (Eq,Show)


-- 2. Write the following StackLang program as a Haskell value:
--
--   3 4 add 5 equ
--
ex1 :: Prog
ex1 = [PushN 3, PushN 4, Add, PushN 5, Equ]


-- 3. Write a StackLang program that:
--     * checks whether 3 and 4 are equal
--     * if so, returns the result of adding 5 and 6
--     * if not, returns the value false
--    First write it in concrete syntax, then in abstract syntax as a Haskell value.
--
--    3 4 equ if 5 6 add else false end
--
ex2 :: Prog
ex2 = [PushN 3, PushN 4, Equ, IfThenElse [PushN 5, PushN 6, Add] [PushB False]]


-- 4. Write a Haskell function that takes two arguments x and y
--    and generates a StackLang program that adds both x and y to
--    the number on the top of the stack
genAdd2 :: Num -> Num -> Prog
genAdd2 x y = [PushN x, PushN y, Add, Add]


-- 5. Write a Haskell function that takes a list of integers and
--    generates a StackLang program that sums them all up.
genSum :: [Int] -> Prog
genSum []     = [PushN 0]
genSum (x:xs) = genSum xs ++ [PushN x, Add]



--
-- * Semantics of StackLang (now!)
--


-- 6. Identify/define a semantics domain for Cmd and for Prog.
--    Things we need:
--      * stack
--        * numbers
--        * booleans
--      * errors
--        * type error
--        * stack underflow

-- data Either a b = Left a | Right b

type Stack = [Either Num Bool]
-- type Domain = Stack -> Maybe Stack


-- 7. Define the semantics of a StackLang command (ignore If at first).
cmd :: Cmd -> Stack -> Maybe Stack
-- cmd (PushN n) s = Just (Left n : s)
cmd (PushN n) = \s -> Just (Left n : s)
cmd (PushB b) = \s -> Just (Right b : s)
cmd Add = \s -> case s of
                  (Left n : Left m : s') -> Just (Left (n+m) : s')
                  _ -> Nothing
cmd Equ = \s -> case s of
                  (Left n : Left m : s') -> Just (Right (n==m) : s')
                  (Right b : Right c : s') -> Just (Right (b==c) : s')
                  _ -> Nothing
cmd (IfThenElse t e) = \s -> case s of
                               (Right True : s')  -> prog t s'
                               (Right False : s') -> prog e s'
                               _ -> Nothing


-- 8. Define the semantics of a StackLang program.
prog :: Prog -> Stack -> Maybe Stack
prog []     = \s -> Just s
prog (c:cs) = \s -> case cmd c s of
                      Just s' -> prog cs s'
                      Nothing -> Nothing


-- | Run a program on an initially empty stack.
--
--   >>> run ex2
--   Just [Right False]
--
--   >>> run (genSum [1..10])
--   Just [Left 55]
--
--   >>> run [PushN 3, Add, PushN 4]
--   Nothing
--
run :: Prog -> Maybe Stack
run p = prog p []
