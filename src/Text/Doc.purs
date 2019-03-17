module Text.Doc where 

import Prelude 

import Data.Array (replicate)

import Data.Foldable hiding (null)
import Data.Maybe
import Data.String as S
import Data.String.CodePoints (codePointFromChar)
import Data.Tuple 

import Data.Sequence

data Doc = Inline Int (Seq String) 
         | Block Int Doc (Seq Doc)  

-- | Returns an inline document containing the given string. 
inline :: String -> Doc 
inline = Inline 0 <<< singleton 

-- | Returns a block document containing the given documents. 
block :: Seq Doc -> Doc 
block ds = case uncons ds of 
             Just (Tuple head tail) -> Block 0 head tail 
             Nothing                -> Inline 0 mempty

-- | Returns a string of white space of length `n`. 
ws :: Int -> String 
ws n = S.fromCodePointArray (replicate n $ codePointFromChar ' ')

-- | Takes an accumulator containing the identation and a doc, and returns 
-- | a string representing that doc.  
print :: Int -> Doc -> String 
print tab (Inline pad ss)  = ws (tab + pad) <> intercalate " " ss 
print tab (Block pad d ds) = intercalate "\n"
                           $ (print $ tab + pad) <$> (cons d ds)

-- | Show the enire document as a single line and return that string 
-- | representation.
inlined :: Doc -> String 
inlined (Inline _ ss)  = intercalate " " ss
inlined (Block _ d ds) = S.joinWith " " <<< toUnfoldable $ inlined <$> (cons d ds)

instance showDoc :: Show Doc 
  where 
  show = print 0

-- | Indent the entire doc `n` spaces. 
indent :: Int -> Doc -> Doc 
indent tab (Inline pad s)   = Inline (tab + pad) s 
indent tab (Block pad d ds) = Block (tab + pad) d ds 

-- | Places the given string to the left of the given doc, shifting the rest of the 
-- | doc so that the indentation is preserved. 
beside :: String -> Doc -> Doc
beside pre (Inline pad ss)  = Inline 0 (cons pre ss)
beside pre (Block pad d ds) = block <<< cons (beside pre d) 
                            $ indent (S.length pre + 1) <$> ds

-- | Prepends a string to the first line in the document and shifts lines in the 
-- | same block as that line to preserve indentation. 
prefix :: String -> Doc -> Doc 
prefix pre (Inline pad ss)  = Inline 0 (cons pre ss) 
prefix pre (Block pad d ds) = block $ cons (indentBlock (S.length pre + 1) (prefix pre d)) ds

-- | Places the second document after the last line of the first document. 
-- | The second document is shifted so that indentation is preserved. 
flow :: Doc -> Doc -> Doc 
flow line@(Inline pad ss) docR = indent pad $ (beside (inlined line) docR)
flow (Block pad d ds) docR 
  = case unsnoc ds of 
      Just (Tuple init last) -> indent pad <<< block <<< cons d <<< snoc init
                              $ flow last docR 
      Nothing -> block <<< singleton $ flow d docR

infixr 4 flow as :>

-- | Indent all but the first line of the given document by `n` spaces.
indentBlock :: Int -> Doc -> Doc 
indentBlock n (Inline pad ss)  = Inline pad ss 
indentBlock n (Block pad d ds) = Block pad d (indent n <$> ds) 

-- | Returns the given doc with surrounded with the given `open` and 
-- | `close` strings. 
between :: String -> String -> Doc -> Doc 
between open close doc = prefix open doc :> inline close

-- | Returns the given doc surrounded with parenthesis. 
parens :: Doc -> Doc 
parens = between "(" ")"

-- | Returns the given doc surrounded with square brackets.
brackets :: Doc -> Doc 
brackets = between "[" "]"

-- | Returns the given doc surrounded with curly braces. 
braces :: Doc -> Doc 
braces = between "{" "}"

-- | Returns true when given the empty doc. 
isEmpty :: Doc -> Boolean 
isEmpty (Inline _ ss) = null ss 
isEmpty _             = false

-- | Place the given string beside every line in the given doc but the 
-- | first. 
intermix :: String -> Doc -> Doc 
intermix sep line@(Inline _ _) = line
intermix sep (Block pad d ds)  = Block pad d $ beside sep <$> ds