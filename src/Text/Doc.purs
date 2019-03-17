module Text.Doc where 

import Prelude 

import Data.Array (replicate)

import Data.Foldable hiding (null)
import Data.Maybe
import Data.String as S
import Data.String.CodePoints (codePointFromChar)
import Data.Tuple 

import Data.Sequence

data DocF = Inline Int (Seq String) 
         | Block Int DocF (Seq DocF)  

inline :: String -> DocF 
inline = Inline 0 <<< singleton 

block :: Seq DocF -> DocF 
block ds = case uncons ds of 
             Just (Tuple head tail) -> Block 0 head tail 
             Nothing                -> Inline 0 mempty

-- | Returns a string of white space of length `n`. 
ws :: Int -> String 
ws n = S.fromCodePointArray (replicate n $ codePointFromChar ' ')

-- | Takes an accumulator containing the identation and a doc, and returns 
-- | a string representing that doc.  
print :: Int -> DocF -> String 
print tab (Inline pad ss)  = ws (tab + pad) <> intercalate " " ss 
print tab (Block pad d ds) = intercalate "\n"
                           $ (print $ tab + pad) <$> (cons d ds)

inlined :: DocF -> String 
inlined (Inline _ ss)  = intercalate " " ss
inlined (Block _ d ds) = S.joinWith " " <<< toUnfoldable $ inlined <$> (cons d ds)

instance showDocF :: Show DocF 
  where 
  show = print 0

indent :: Int -> DocF -> DocF 
indent tab (Inline pad s)   = Inline (tab + pad) s 
indent tab (Block pad d ds) = Block (tab + pad) d ds 

prepend :: String -> DocF -> DocF
prepend pre (Inline pad ss)  = Inline 0 (cons pre ss)
prepend pre (Block pad d ds) = block <<< cons (prepend pre d) 
                                  $ indent (S.length pre + 1) <$> ds

prefix :: String -> DocF -> DocF 
prefix pre (Inline pad ss)  = Inline 0 (cons pre ss) 
prefix pre (Block pad d ds) = block $ cons (indentBlock (S.length pre + 1) (prefix pre d)) ds

flow :: DocF -> DocF -> DocF 
flow line@(Inline pad ss) docR = indent pad $ (prepend (inlined line) docR)
flow (Block pad d ds) docR 
  = case unsnoc ds of 
      Just (Tuple init last) -> indent pad <<< block <<< cons d <<< snoc init
                              $ flow last docR 
      Nothing -> block <<< singleton $ flow d docR

infixr 4 flow as :->

indentBlock :: Int -> DocF -> DocF 
indentBlock n (Inline pad ss)  = Inline pad ss 
indentBlock n (Block pad d ds) = Block pad d (indent n <$> ds) 

between :: String -> String -> DocF -> DocF 
between open close doc = prefix open doc :-> inline close

parens :: DocF -> DocF 
parens = between "(" ")"

brackets :: DocF -> DocF 
brackets = between "[" "]"

braces :: DocF -> DocF 
braces = between "{" "}"

isEmpty :: DocF -> Boolean 
isEmpty (Inline _ ss) = null ss 
isEmpty _             = false

intermix :: String -> DocF -> DocF 
intermix sep line@(Inline _ _) = line
intermix sep (Block pad d ds)  = Block pad d $ prepend sep <$> ds