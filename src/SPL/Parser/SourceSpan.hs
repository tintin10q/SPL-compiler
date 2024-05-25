module SPL.Parser.SourceSpan where 
import Text.Megaparsec (SourcePos)
import Text.Megaparsec.Pos (sourcePosPretty)
import SPL.Colors (green)

-- This is the type we use to keep track to what things relate in the original source code

newtype SourceSpan = SourceSpan (SourcePos, SourcePos)
  deriving (Eq)

startPos :: SourceSpan -> SourcePos
startPos (SourceSpan (start, _)) = start

showStart :: SourceSpan -> String
showStart pos =  green $ sourcePosPretty $ startPos pos

showEnd :: SourceSpan -> String
showEnd pos = green $ sourcePosPretty $ startPos pos 

endPos :: SourceSpan -> SourcePos
endPos (SourceSpan (_, end)) = end

instance Show SourceSpan where
  -- show (SourceSpan (start, end)) = sourcePosPretty start ++ "->" ++ sourcePosPretty end
  show (SourceSpan (_start, _end)) = "META" 

srcSpan :: SourcePos -> SourcePos -> SourceSpan
srcSpan start end = SourceSpan (start, end)

