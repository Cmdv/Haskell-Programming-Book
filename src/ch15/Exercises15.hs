module Exercises15 where

import Data.Monoid
import Data.Semigroup
import Test.QuickCheck

-------------------------------------------
-- Exercise: Optional Monoid

-- this needed updating since Base 4.11.1.0 was released in April of 2018
-- changed things.

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada x               = x
  (<>) x Nada               = x
  (<>) (Only x) (Only noun) = Only $ x <> noun

instance Monoid a => Monoid (Optional a) where
  mempty  = Nada
  mappend = (<>)


-------------------------------------------
-- Madness

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbing' :: Exclamation
            -> Adverb
            -> Noun
            -> Adjective
            -> String
madlibbing' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."


madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' e adv noun adj = mconcat
  [ e
  , "! he said "
  , adv
  , " as he jumped into his car "
  , noun
  , " and drove off with his "
  , adj
  ,  " wife."]

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- data Bull =
--     Fools
--   | Twoo
--   deriving (Eq, Show)

-- instance Arbitrary Bull where
--   arbitrary = frequency [ (1, return Fools)
--                         , (1, return Twoo)]

-- instance Semigroup Bull where
--   (<>) _ _ = Fools

-- instance Monoid Bull where
--   mempty = Fools
--   mappend _ _ = Fools

-- type BullMappend =
--   Bull -> Bull -> Bull -> Bool

-- main :: IO ()
-- main = do
--   let ma = monoidAssoc
--       mli = monoidLeftIdentity
--       mlr = monoidRightIdentity
--   quickCheck (ma :: BullMappend)
--   quickCheck (mli :: Bull -> Bool)
--   quickCheck (mlr :: Bull -> Bool)

-------------------------------------------
-- Exercise: Maybe Another Monoid

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return Nada), (2, genOnly)]
    where
      genOnly = do
        a <- arbitrary
        return (Only a)

newtype First' a =
  First' { getFirst' :: Optional a}
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) (First' Nada) r = r
  (<>) s _             = s

instance Monoid (First' a) where
  mempty = First' Nada
  mappend = (<>)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    return First' {getFirst' = a}

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId = First' String -> Bool

-- main :: IO ()
-- main = do
--   quickCheck (monoidAssoc :: FirstMappend)
--   quickCheck (monoidLeftIdentity :: FstId)
--   quickCheck (monoidRightIdentity :: FstId)


-------------------------------------------
-- Chapter exercises:

-- Semigroup exercises

semigroupAssoc ::
     (Eq m, Semigroup m)
  => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

-- 1:

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

-- 2:
newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3:
data Two a b =
  Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two a b) (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a ,Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc =
     Two [Product Int] String
  -> Two [Product Int] String
  -> Two [Product Int] String
  -> Bool

-- 4:
data Three a b c =
  Three a b c
  deriving (Show, Eq)

instance
     (Semigroup a, Semigroup b, Semigroup c)
  => Semigroup (Three a b c) where
  (<>) (Three a b c) (Three d e f) = Three (a <> d) (b <> e) (c <> f)

instance (Arbitrary a ,Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeAssoc =
     Three [Product Int] String String
  -> Three [Product Int] String String
  -> Three [Product Int] String String
  -> Bool

-- 4:
data Four a b c d =
  Four a b c d
  deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
  => Semigroup (Four a b c d) where
  (<>) (Four a b c d) (Four e f g h) = Four (a <> e) (b <> f) (c <> g) (d <> h)

instance (Arbitrary a ,Arbitrary b, Arbitrary c, Arbitrary d)
  => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type FourAssoc =
     Four [Product Int] String String [Sum Int]
  -> Four [Product Int] String String [Sum Int]
  -> Four [Product Int] String String [Sum Int]
  -> Bool

-- 6:
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj a) (BoolConj b) = BoolConj (a && b)

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return (BoolConj a)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7:
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj a) (BoolDisj b) = BoolDisj (a || b)

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return (BoolDisj a)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8:
data Or a b =
    Fst a
  | Snd b
  deriving (Show, Eq)

instance Semigroup (Or a b) where
  (<>) (Fst _) b = b
  (<>) x _       = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return (Fst a), return (Snd b)]

type OrAssoc = Or Char Int -> Or Char Int -> Or Char Int -> Bool


-- 9:
newtype Combine a b = Combine { unCombine :: a -> b }

instance Show (Combine a b) where
  show _ = "Combine { unCombine = <function> }"

instance (Semigroup b) => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine $ \x -> f x <> g x

-- 10:
newtype Comp a =
  Comp { unComp :: a -> a }

instance Show (Comp a) where
  show _ = "Comp { unComp = <function> }"

instance Semigroup (Comp a) where
  (<>) (Comp g) (Comp f) = Comp (g . f)

genFunc :: (CoArbitrary a, Arbitrary a) => Gen (a -> a)
genFunc = arbitrary

genComp :: (CoArbitrary a, Arbitrary a) => Gen (Comp a)
genComp = do
  f <- genFunc
  return Comp {unComp = f}


-- 11:
data Validation a b =
    Failure' a
  | Success' b
  deriving (Show, Eq)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Success' a) (Success' _) = Success' a
  (<>) (Success' _) (Failure' a) = Failure' a
  (<>) (Failure' a) _            = Failure' a

validator :: IO ()
validator = do
  let failure :: String -> Validation String Int
      failure = Failure'
      success :: Int -> Validation String Int
      success = Success'
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2

-- all the functions to run for the Semigroup exercises:
-- main :: IO ()
-- main = do
--   quickCheck (semigroupAssoc :: TrivAssoc)
--   quickCheck (semigroupAssoc :: IdentityAssoc)
--   quickCheck (semigroupAssoc :: TwoAssoc)
--   quickCheck (semigroupAssoc :: ThreeAssoc)
--   quickCheck (semigroupAssoc :: FourAssoc)
--   quickCheck (semigroupAssoc :: BoolConjAssoc)
--   quickCheck (semigroupAssoc :: OrAssoc)
--   validator

-------------------------------------------
-- Monoid exerises:


monoidAssoc' :: (Eq m, Monoid m) => m -> m-> m -> Bool
monoidAssoc' a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidIdentityL :: (Eq m, Monoid m) => m -> Bool
monoidIdentityL x = mempty <> x == x

monoidIdentityR :: (Eq m, Monoid m) => m -> Bool
monoidIdentityR x = x <> mempty == x


-- 1:
instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

type TrivialAssocM = Trivial -> Trivial -> Trivial -> Bool

-- 2:
instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)


-- 3:
instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

-- 4:
instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

-- 5:
instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

-- 6:
instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mempty = Combine $ const mempty
  mappend = (<>)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

type CSum = Combine Int (Sum Int)

combineAssoc :: CSum -> CSum -> CSum -> Int -> Bool
combineAssoc a b c x =
  unCombine (a <> (b <> c)) x == unCombine ((a <> b) <> c) x

combineLeftIdentity :: Combine Int String -> Int -> Bool
combineLeftIdentity c x = unCombine (mempty <> c) x == unCombine c x

combineRightIdentity :: Combine Int String -> Int -> Bool
combineRightIdentity c x = unCombine (c <> mempty) x == unCombine c x


-- 7:
instance Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)

instance (Arbitrary a, CoArbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return $ Comp f

type CompInt = Comp Int

compAssoc :: CompInt -> CompInt -> CompInt -> Int -> Bool
compAssoc a b c x =
  unComp (a <> (b <> c)) x == unComp ((a <> b) <> c) x

-- can't work these out

-- compLeftIdentity :: CompInt -> Bool
-- compLeftIdentity c = unComp (mempty <> c) == unComp c

-- compRightIdentity :: CompInt -> Bool
-- compRightIdentity c = unComp (c <> mempty) == unComp c

-- 8:

newtype Mem s a =
  Mem {runMem :: s -> (a, s)}

instance Show (Mem s a) where
  show _ = "Mem s a"

instance Semigroup a => Semigroup (Mem s a) where
  (<>) (Mem f) (Mem g) =
    Mem $ \s ->
      let (a1, s1) = f s
          (a2, s2) = g s1
      in
        (a1 <> a2, s2)

instance (Monoid a, Semigroup a) => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend = (<>)

instance (CoArbitrary a, Arbitrary a, Arbitrary b) => Arbitrary (Mem a b) where
  arbitrary = do
    f <- arbitrary
    return $ Mem f

type MemStrs = Mem String String

memAssoc :: MemStrs -> MemStrs -> MemStrs -> String -> Bool
memAssoc a b c x =
  runMem (a <> (b <> c)) x == runMem ((a <> b) <> c) x

memLeftIdentity :: MemStrs -> String -> Bool
memLeftIdentity c x = runMem (mempty `mappend` c) x == runMem c x

memRightIdentity :: MemStrs -> String -> Bool
memRightIdentity c x = runMem (c `mappend` mempty) x == runMem c x


main :: IO ()
main = do
  putStrLn "Trivial monoid"
  quickCheck (monoidAssoc' :: TrivialAssocM)
  quickCheck (monoidIdentityL :: Trivial -> Bool)
  quickCheck (monoidIdentityR :: Trivial -> Bool)

  putStrLn "Identity monoid"
  quickCheck (monoidIdentityL :: Identity String -> Bool)
  quickCheck (monoidIdentityR :: Identity String -> Bool)

  putStrLn "Two monoid"
  quickCheck (monoidIdentityL :: Two String [Sum Int]-> Bool)
  quickCheck (monoidIdentityR :: Two String [Sum Int] -> Bool)

  putStrLn "BoolConj monoid"
  quickCheck (monoidIdentityL :: BoolConj -> Bool)
  quickCheck (monoidIdentityR :: BoolConj -> Bool)

  putStrLn "BoolDisj monoid"
  quickCheck (monoidIdentityL :: BoolDisj -> Bool)
  quickCheck (monoidIdentityR :: BoolDisj -> Bool)

  putStrLn "Combine a b"
  quickCheck combineAssoc
  quickCheck combineLeftIdentity
  quickCheck combineRightIdentity

  putStrLn "Comp monoid"
  quickCheck compAssoc
  -- quickCheck compLeftIdentity
  -- quickCheck compRightIdentity

  putStrLn "Mem monoid"
  quickCheck memAssoc
  quickCheck memLeftIdentity
  quickCheck memRightIdentity
