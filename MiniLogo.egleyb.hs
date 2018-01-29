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


singleStep :: Int -> Prog
singleStep xy = [Call line [LitN xy+1, LitN xy+1, LitN xy, LitN xy+1], Call line [LitN xy, LitN xy, LitN xy, LitN xy+1] 
         
steps :: Int -> Prog
steps x =  steps x-1 ++ singleStep x

macros :: Prog -> [Macro]
macros cmd : prog = case cmd of
                    Define m _ _ -> m : (macros prog)
                    _ -> macros prog
                    
