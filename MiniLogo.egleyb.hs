-- | Team Members: Bryce Egley ONID: egleyb, Kenneth Price ONID: pricek, Kenneth Thompson ONID: thomkenn
module MiniLogo where

import Prelude hiding (Num)

type Num Int

type Var String

type Macro String

type Prog [Cmd]

data Mode = Down | Up

data Expr = LitV Var
          | LitN Num
          | Add Expr Expr

dara Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
