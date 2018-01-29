-- | Team Members: Bryce Egley ONID: egleyb, Kenneth Price ONID: pricek, Kenneth Thompson ONID: thomkenn
module MiniLogo where

import Prelude hiding (Num)

type Num = Int

type Var = String

type Macro = String

type Prog = [Cmd]

data Mode = Down | Up

data Expr = LitV Var
          | LitN Num
          | Add Expr Expr

data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
--Pen Up;
--Move (x1, y1);
--Pen Down;
--Move (x2, y2)

line :: Cmd
line = Define "line" ["x1", "y1", "x2", "y2"] [Pen Mode Up, Move "x1" "y1", Pen Mode Down, Move "x2" "y2"]

--Pen Up;
--Move (x1, y1);
--Pen Down;
--Move ()

nix :: Prog
