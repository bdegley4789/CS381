-- | Team Members: Bryce Egley ONID: egleyb, Kenneth Price ONID: pricek, Kenneth Thompson ONID: thomkenn
module MiniLogo where

import Prelude hiding (Num)

type Num = Int

type Var = String

type Macro = String

type Prog = [Cmd]

data Mode = Down | Up
  deriving (Eq,Show)

data Expr = LitV Var
          | LitN Num
          | Add Expr Expr
  deriving (Eq,Show)

data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
  deriving (Eq,Show)


         
-- Define line (x1, y1, x2, y2) {
-- Pen Up;
-- Move (x1, y1);
-- Pen Down;
-- Move (x2, y2)
-- }

line :: Cmd
line = Define "line" ["x1", "y1", "x2", "y2"] [(Pen Up), Move (LitV "x1") (LitV "y1"), Pen Down, Move (LitV "x2") (LitV "y2")]

--line (x, y, x+w, y+h)
--line (x+w, y, x , y+h)

nix :: Cmd
nix = Define "nix" ["x", "y", "w", "h"] [Call "line" [LitV "x", LitV "y", Add (LitV "x") (LitV "w"), Add (LitV "y") (LitV "h")], Call "line" [Add (LitV "x") (LitV "w"), LitV "y", LitV "x", Add (LitV "y") (LitV "h")]]


singleStep :: Int -> Prog
singleStep xy = [Call "line" [LitN (xy+1), LitN (xy+1), LitN xy, LitN (xy+1)], Call "line" [LitN xy, LitN xy, LitN xy, LitN (xy+1)]]

steps :: Int -> Prog
steps 0 = [line];
steps x =  steps (x-1) ++ singleStep x

macros :: Prog -> [Macro]
macros [] = []
macros (cmd:prog) = case cmd of
                    Define m _ _ -> m : (macros prog)
                    _ -> macros prog

prettyExpr :: Expr -> String
prettyExpr expr = case expr of
                    LitV v -> v
                    LitN n -> show n
                    Add e1 e2 -> prettyExpr e1 ++ "+" ++ prettyExpr e2

prettyListE :: [Expr] -> String
prettyListE [] = ""
prettyListE [e] = prettyExpr e
prettyListE (e:t) = prettyExpr e ++ "," ++ prettyListE t

prettyListV :: [Var] -> String
prettyListV [] = ""
prettyListV [v] = v
prettyListV (v:t) = v ++ "," ++ prettyListV t

pretty :: Prog -> String
pretty [] = ""
pretty (cmd:prog)  = case cmd of
                      Pen m -> "pen " ++ (if m == Up then "up" else "down") ++ ";" ++ "\n"
                      Move e1 e2 -> "move (" ++ prettyExpr e1 ++ "," ++ prettyExpr e2 ++ ");" ++ "\n"
                      Define m vs p -> "define " ++ m ++ " (" ++ prettyListV vs ++ ") " ++ "{" ++ "\n" ++ pretty p ++ "}" ++ "\n"
                      Call m es -> m ++ " (" ++ prettyListE es ++ ");" ++ "\n"
                      ++ pretty prog

optE :: Expr -> Expr
optE (Add x y) = case (optE x, optE y) of
                   (LitN i, LitN j) -> LitN (i+j)
                   (ox, oy) -> Add ox oy
optE x = x

optP :: Prog -> Prog
optP [] = []
optP (cmd:cmds) = case cmd of
                    (Move e1 e2) -> Move (optE e1) (optE e2)
                    (Define m vs p) -> Define m vs (optP p)
                    Call m es -> Call m ((map optE) es)
                    x -> x
                  : optP cmds