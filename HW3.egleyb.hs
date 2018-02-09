module HW3 where

import MiniMiniLogo
import Render
import Prelude hiding ((/))


--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--   * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--     functions for generating MiniMiniLogo programs. It contains the type
--     definitions for Mode, Cmd, and Prog.
--   * Render.hs contains code for rendering the output of a MiniMiniLogo
--     program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen Up) (_, p) = (((Up), p), Nothing)
cmd (Pen Down) (_, p) = (((Down), p), Nothing)
cmd (Move x y) (Up, _) = (((Up), (x,y)), Nothing)
cmd (Move x y) (Down, (x0,y0)) = (((Down), (x,y)), (Just ((x0,y0),(x,y))))  




-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line])
prog  [] s = (s, [])
prog (h:t) s = case (cmd h s) of
                (s1, Nothing) -> prog t s1
                (s1, Just (l)) -> case (prog t s1) of
                                 (s2, ls) -> (s2, l:ls)


--
-- * Extra credit
--

(/) :: Int -> Int -> Int
(/) a b = a `div` b

boxl :: Int -> Int -> Int -> Prog
boxl x y l = [Pen Up, Move x y, Pen Down, Move (x+l) y, Move (x+l) (y+l), Move x (y+l), Move x y]

grid :: Int -> Int -> Int -> Prog
grid x y l = [Pen  Up, Move (x+(l/3)) y, Pen Down, Move (x+(l/3)) (y+l), 
                Pen  Up, Move (x+(2*(l/3))) y, Pen Down, Move (x+(2*(l/3))) (y+l),
                Pen  Up, Move x (y+(l/3)), Pen Down, Move (x+l) (y+(l/3)),
                Pen  Up, Move x (y+(2*(l/3))), Pen Down, Move (x+l) (y+(2*(l/3)))]

fract :: Int -> Int -> Int -> Int -> Prog
fract _ _ _ 0 = []
fract x y l c = (grid x y l) 
                ++ (fract (x) (y) (l/3) (c-1))
                ++ (fract (x+(l/3)) (y) (l/3) (c-1))
                ++ (fract (x+(2*(l/3))) (y) (l/3) (c-1))
                ++ (fract (x) (y+(l/3)) (l/3) (c-1))
                ++ (fract (x+(2*(l/3))) (y+(l/3)) (l/3) (c-1))
                ++ (fract (x) (y+(l/3)) (l/3) (c-1))
                ++ (fract (x+(l/3)) (y+(l/3)) (l/3) (c-1))
                ++ (fract (x+(2*(l/3))) (y+(l/3)) (l/3) (c-1))

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
amazing :: Prog
amazing = (boxl 0 0 81) ++ (fract 0 0 81 3)
