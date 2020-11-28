-- CS 314, Fall 2020, Homework 8
-- Due 10:00 PM, November 30
-- (Submissions up to six hours past the deadline accepted with reduced scores.)
--
-- Replace the placeholder definition of firsts with your own, then submit the revised
-- file through Sakai.

module HW8 where

import Control.Applicative
import Data.List
import Parser
import Lec18
import Lec19

-- Discussion
-- ----------
--
-- Recall:
-- * an alphabet is a set of symbols
-- * a string over an alphabet is a sequence (finite list) of symbols from that alphabet
-- * a language over an alphabet is a set of strings over an alphabet
-- * for each regular expression R, there is a language L(R)
--
-- Given an alphabet A and some language X, we can define a set F(X) containing exactly
-- those symbols that occur first in some string in X.
-- * F(X) is a subset of A
-- * For every symbol in F(X), at least one string in X begins with that symbol
-- * For every string in X, the first symbol in that string is in F(X)
--
-- Example: if X = {ab, abc, def}, then F(X) = {a, d}
--
-- Consider: what if X contains no strings? What if X contains only the empty string?

-- Problem
-- -------
--
-- Define a function firsts that computes F(L(R)) for any regular expression R.
--
-- *HW8> firsts (RAlt (RSeq (RSym A) (RSym B)) (RSeq (RSym C) (RSym D)))
-- [A,C]
--
-- *HW8> firsts (RStar (RSeq (RSym A) (RSym B)))
-- [A]

-- Experimentating with analyzing a regex

-- getList :: RE symbol -> [symbol]
-- getList (RSym c)   = [c]
-- getList REps       = []
-- getList (RAlt a b) = interleave (getList a) (getList b)
-- getList (RSeq a b) = interleave (getList a) (getList b)
-- getList (RStar a)  = getList a
-- getList (RPlus a)  = getList a
-- getList _          = []

containsEps :: RE symbol -> Bool
containsEps REps = True
containsEps (RAlt REps _) = True
containsEps (RAlt _ REps) = True
containsEps (RAlt (RAlt a b) _) = (containsEps a) || (containsEps b)
containsEps (RAlt _ (RAlt a b)) = (containsEps a) || (containsEps b)
containsEps _ = False

containsZero :: RE symbol -> Bool
containsZero RZero = True
containsZero (RSeq RZero _) = True
containsZero (RSeq _ RZero) = True
containsZero (RSeq (RSeq a b) _) = (containsZero a) || (containsZero b)
containsZero (RSeq _ (RSeq a b)) = (containsZero a) || (containsZero b)
containsZero _ = False 

firsts :: RE symbol -> [symbol]
firsts (RSym c)   = [c]
firsts REps       = []
firsts RZero      = []
firsts (RAlt a b) = interleave (firsts a) (firsts b)
firsts (RSeq (RStar a) b) = interleave (firsts a) (firsts b)
firsts (RSeq (RSeq a c) b) | (containsZero a) == True = []
                           | (containsZero c) == True = []
                           | (containsZero a) == False = firsts b
                           | (containsZero c) == False = firsts b
firsts (RSeq (RAlt a c) b) | (containsEps a) == True = (firsts a) ++ (firsts c) ++ (firsts b)
                           | (containsEps c) == True = (firsts a) ++ (firsts c) ++ (firsts b)
                           | (containsEps a) == False = firsts b
                           | (containsEps c) == False = firsts b
firsts (RSeq b (RSeq a c)) | (containsZero a) == True = []
                           | (containsZero c) == True = []
                           | (containsZero a) == False = firsts b
                           | (containsZero c) == False = firsts b
firsts (RSeq b (RAlt a c)) | (containsEps a) == True = (firsts a) ++ (firsts c) ++ (firsts b)
                           | (containsEps c) == True = (firsts a) ++ (firsts c) ++ (firsts b)
                           | (containsEps a) == False = firsts b
                           | (containsEps c) == False = firsts b

firsts (RSeq a _) = firsts a
firsts (RStar a)  = firsts a
firsts (RPlus a)  = firsts a
-- firsts _          = []


-- Note the type of firsts: your function must work for any type of symbol, even if the
-- symbols cannot be compared for order or equality. Instead of returning a set, return
-- a list. Note that your function cannot ensure the absence of duplicates, because doing
-- so would require comparing for equality.
--
-- To be correct, your implementation of firsts must return a list, such that:
-- 1. every symbol in F(L(R)) occurs at least once in the list
-- 2. no symbol not in F(L(R)) occurs in the list
-- 3. the list is finite
--
-- That means you cannot simply generate every non-empty string in the languge and take
-- the first symbol of each string. You will need to analyze the structure of the regular
-- expression.
--
-- You may use any function from the standard library, and any of the functions imported
-- from the lecture modules, including those not already imported. (Be sure to look
-- through Lec18.hs and Lec19.hs for anything that might be useful.)

-- Starting points
-- ---------------
--
-- Begin by considering several regular expressions defining finite languages. E.g.,
-- * ABCD
-- * AB|CD
-- * (A|e)(B|e)
-- * (A|e)(B|e)CD
-- * A0B|C
--
-- (Note that we are writing 'e' for epsilon and '0' for the empty set/language.)
--
-- List every string in the languages for these regular expressions and find F. What
-- patterns do you notice? What would be the implications of adding * and +?

-- Tools
-- -----
--
-- Here are two alphabets you may find helpful for testing:

data Syma = A | B | C | D | E | F | G deriving (Show, Eq, Ord)
data Symz = U | V | W | X | Y | Z deriving (Show)

-- Note that the members of the second alphabet cannot be compared for equality. Your
-- implementation of firsts should work equally well for either of these, because it is
-- polymorphic over all types.

-- When testing, you may create RE values by hand, or use the parsers given below.
--
-- *HW8> parseA "(AB)*(EF|e)"
-- Just (RSeq (RStar (RSeq (RSym A) (RSym B))) (RAlt (RSeq (RSym E) (RSym F)) REps))
--
-- *HW8> parseZ "WX*Y|Z"
-- Just (RAlt (RSeq (RSym W) (RSeq (RStar (RSym X)) (RSym Y))) (RSym Z))
--
-- *HW8> Just r = parseA "E(e|F)"
-- *HW8> match r [E,F]
-- True


parseA :: String -> Maybe (RE Syma) 
parseA = parse (pGRE pSyma)

parseZ :: String -> Maybe (RE Symz)
parseZ = parse (pGRE pSymz)

pGRE :: Parser symbol -> Parser (RE symbol)
pGRE pSym = pRE
    where

    pRE3 = RSym <$> pSym
        <|> pure REps <* pKW "e"
        <|> pure RZero <* pKW "0"
        <|> pKW "(" *> pRE <* pKW ")"

    pStar = pure RStar <* pKW "*" 
        <|> pure RPlus <* pKW "+"

    pRE2 = foldl' (flip ($)) <$> pRE3 <*> many pStar

    pRE1 = RSeq <$> pRE2 <*> pRE1
        <|> pRE2

    pRE = RAlt <$> pRE1 <* pKW "|" <*> pRE
        <|> pRE1

pSyma = foldr ((<|>) . punshow) empty [A,B,C,D,E,F,G]
pSymz = foldr ((<|>) . punshow) empty [U,V,W,X,Y,Z]

punshow :: (Show a) => a -> Parser a
punshow a = pure a <* pKW (show a)

-- Finally, you may use this function to generate all strings in the language for
-- a regular expression. (Note that this may be infinite!)
--
-- *HW8> let Just r = parseA "AB|CD" in lang r
-- [[A,B],[C,D]]
--
-- *HW8> let Just r = parseA "A*B*" in take 10 $ lang r
-- [[],[A],[B],[A,A],[B,B],[A,B],[B,B,B],[A,A,A],[B,B,B,B],[A,B,B]]

lang :: RE symbol -> [[symbol]]
lang (RSym s) = [[s]]
lang REps = [[]]
lang RZero = []
lang (RAlt a b) = interleave (lang a) (lang b)
lang (RSeq a b) | null (lang b) = []
lang (RSeq a b) = foldr (\s -> interleave (map (s ++) (lang b))) [] (lang a)
lang (RStar a)  = [] : lang (RSeq a (RStar a))
lang (RPlus a)  = lang (RSeq a (RStar a))

-- unlike ++, interleave guarantees that every element in its input lists will eventually
-- appear in the result
interleave (x:xs) ys = x : interleave ys xs
interleave [] ys     = ys