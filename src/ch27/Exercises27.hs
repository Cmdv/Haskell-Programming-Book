module Exercises27 where

-------------------------------------------
-- Exercises: Evaluate
-- 1:
-- const 1 undefined == 1

-- 2:
-- const undefined 1 == Bottom

-- 3:
-- flip const undefined 1 == 1

-- 4:
-- flip const 1 undefined == Bottom

-- 5:
-- const undefined undefined == Bottom

-- 6:
-- foldr const 'z' ['a'..'e'] == 'a'

-- 7:
-- foldr (flip const) 'z' ['a'..'e'] == 'z'


-------------------------------------------
-- Chapter Exercises

-- What will :sprint output?

-- 1:
-- let x = 1
-- x = _

-- 2:
-- let x = ['1']
-- x = '1'

-- 3:
-- let x = [1]
-- x = _

-- 4:
-- let x = 1 :: Int
-- x = 1

-- 5:
-- let f = \x -> x
-- let x = f 1
-- x = _

-- 6:
-- let f :: Int -> Int; f = \x -> x
-- let x = f 1
-- x = _


-- Will printing this expression result in bottom?
-- 1:
-- snd (undefined, 1) == 1

-- 2:
-- let x = undefined
-- let y = x `seq` 1 in snd (x, y) == Bottom

-- 3:
-- length $ [1..5] ++ undefined == Bottom

-- 4:
-- length $ [1..5] ++ [undefined] == 6

-- 5:
-- const 1 undefined == 1

-- 6:
-- const 1 (undefined `seq` 1) == 1

-- 7:
-- const undefined 1 == Bottom


-- Make the expression bottom
x' = undefined
y' = "blah"

main' = do
       print (snd (x', y'))

mainBang = do
  let s !x y = (snd (x, y))
  print $ s x y
