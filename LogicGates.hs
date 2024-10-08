module LogicGates where

nand' :: Bool -> Bool -> Bool
nand' True True = False
nand' _ _ = True

not' :: Bool -> Bool
not' x = nand' True x

and' :: Bool -> Bool -> Bool
and' x y = not (nand' x y)

or' :: Bool -> Bool -> Bool
or' x y = nand' (not' x) (not' y)

nor' :: Bool -> Bool -> Bool
nor' x y = not (or' x y)

xor' :: Bool -> Bool -> Bool
xor' x y = (x `and'` not' y) `or'` (not' x `and'` y)