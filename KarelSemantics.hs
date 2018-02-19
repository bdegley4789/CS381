module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t) w r = not (test t w r)
test (Facing c) _ r = c == (getFacing r)
test (Clear d) w r = isClear (relativePos d r) w
test Beeper w r = hasBeeper (getPos r) w
test Empty _ r = isEmpty r

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt Move _ w r = let np = (relativePos Front r) in
                            if isClear np w 
                                then OK w (setPos (relativePos Front r) r)
                                else Error ("Blocked at: " ++ show np)
stmt PutBeeper _ w r = let p = getPos r in
                        if not (isEmpty r)
                            then OK (incBeeper p w) (decBag r)
                            else Error ("No beeper to put.")
stmt (Turn d) _ w r = OK w (updateFacing (cardTurn d) r)
stmt (Block [s]) m w r = stmt s m w r
stmt (Block (s:ss)) m w r = case (stmt s m w r) of
                            (OK wn rn) -> stmt (Block ss) m wn rn
                            res -> res
stmt (If t s1 s2) m w r = if (test t w r) then (stmt s1 m w r) else (stmt s2 m w r)
stmt (Call c) m w r = case (lookup c m) of
                        Just s -> stmt s m w r
                        Nothing -> Error ("Undefined macro: " ++ c)
stmt (Iterate 1 s) m w r = stmt s m w r
stmt (Iterate i s) m w r = case (stmt s m w r) of 
                            (OK wn rn) -> stmt (Iterate (i-1) s) m wn rn
                            res -> res
stmt (While t s) m w r = if (test t w r)
                            then case (stmt s m w r) of
                                  (OK wn rn) -> stmt (While t s) m wn rn
                                  res -> res
                            else
                                  (OK w r)
stmt _ _ _ _ = Error ("Invalid command");


    
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
