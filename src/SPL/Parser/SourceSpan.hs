module SPL.Parser.SourceSpan where 
import Text.Megaparsec (SourcePos (SourcePos))
import Text.Megaparsec.Pos (sourcePosPretty, unPos)
import SPL.Colors (green)
import System.FilePath (takeFileName)

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

startCol :: SourceSpan -> Int
startCol s = case startPos s of 
                      (SourcePos _ line _) -> unPos line

startFilename :: SourceSpan -> String
startFilename s = case startPos s of 
                      (SourcePos filepath _ _) -> takeFileName filepath



startLine :: SourceSpan -> Int
startLine s = case startPos s of 
                      (SourcePos _ _ col) -> unPos col



instance Show SourceSpan where
  -- show (SourceSpan (start, end)) = sourcePosPretty start ++ "->" ++ sourcePosPretty end
  show (SourceSpan (_start, _end)) = "META" 

srcSpan :: SourcePos -> SourcePos -> SourceSpan
srcSpan start end = SourceSpan (start, end)

