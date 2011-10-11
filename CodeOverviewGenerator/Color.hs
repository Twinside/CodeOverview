module CodeOverviewGenerator.Color( ColorDef(..)
                                  , ViewColor
                                  , defaultColorDef
                                  , parseColorDef
                                  , prepareKeywords
                                  , strComment
                                  , makeEntityColorLookupTable
                                  ) where

import Data.Array
import Data.List( foldl' )
import CodeOverviewGenerator.Language
import qualified Data.Map as Map
import qualified CodeOverviewGenerator.ByteString as B

type ViewColor = (Int, Int, Int, Int)

-- | Color configuration for image generation.
data    ColorDef = ColorDef
    { commentColor   :: ViewColor
    , stringColor    :: ViewColor
    , normalColor    :: ViewColor
    , highlightColor :: ViewColor
    , majColor       :: ViewColor
    , emptyColor     :: ViewColor
    , viewColor      :: ViewColor
    , keywordColor   :: ViewColor
    , typeColor      :: ViewColor
    , constantColor  :: ViewColor

    , labelColor       :: ViewColor
    , conditionalColor :: ViewColor
    , repeatColor      :: ViewColor
    , structureColor   :: ViewColor
    , statementColor   :: ViewColor
    , preprocColor     :: ViewColor
    , macroColor       :: ViewColor
    , typedefColor     :: ViewColor
    , exceptionColor   :: ViewColor
    , operatorColor    :: ViewColor
    , includeColor     :: ViewColor
    , storageClassColor :: ViewColor
    , charColor        :: ViewColor
    , numberColor      :: ViewColor
    , floatColor       :: ViewColor
    , boolColor       :: ViewColor
    , functionColor    :: ViewColor
    , tagColor         :: ViewColor
    , attribTagColor   :: ViewColor

    , errorLineColor :: ViewColor
    , warningLineColor :: ViewColor
    , infoLineColor :: ViewColor

    , heatRamp :: Array Int ViewColor
    }
    deriving Show

-- | Small heat ramp from cold color to warm ones.
smallHeatRamp :: [ViewColor]
smallHeatRamp =
    [ (  0,   0, 255, 255)
    , (  0, 103, 255, 255)
    , (  0, 207, 255, 255)
    , (  0, 255, 198, 255)
    , (  0, 255,  94, 255)
    , (  9, 255,   0, 255)
    , (114, 255,   0, 255)
    , (217, 255,   0, 255)
    , (255, 188,   0, 255)
    , (255,  83,   0, 255)
    , (255,   0,   0, 255) ]

strComment :: String -> Maybe B.ByteString
strComment = Just . B.pack

prepareKeywords :: [([String], CodeEntity)] -> Map.Map B.ByteString CodeEntity
prepareKeywords lst = Map.fromList
    [(B.pack str, entity) | (strList, entity) <- lst
                          , str <- strList ]

defaultColorDef :: ColorDef
defaultColorDef = ColorDef
    { commentColor   = (100,155,100,255)
    , normalColor    = (128,128,128,255)
    , constantColor  = (128,128,128,255)
    , stringColor    = (100,100,155,255)
    , highlightColor = (200,200,100,255)
    , majColor       = (  0,  0,  0,255)
    , emptyColor     = (255,255,255,  0)
    , viewColor      = (200,200,255,255)
    , keywordColor   = (100,100,255,255)
    , typeColor      = (100,100,255,255)

    , labelColor       = (  0,   0, 255, 255)
    , conditionalColor = (  0,   0, 255, 255)
    , repeatColor      = (  0,   0, 255, 255)
    , structureColor   = (  0,   0, 255, 255)
    , statementColor   = (  0,   0, 255, 255)
    , preprocColor     = (  0,   0, 255, 255)
    , macroColor       = (  0,   0, 255, 255)
    , functionColor    = (  0,   0, 128, 255)
    , tagColor         = (  0,   0, 128, 255)
    , attribTagColor   = (  0,   0, 128, 255)
    , typedefColor     = (  0,   0,   0, 255)
    , includeColor     = (  0,   0,   0, 255)
    , exceptionColor   = (  0,   0,   0, 255)
    , operatorColor    = (  0,   0,   0, 255)
    , storageClassColor = (  0,   0,   0, 255)
    , charColor = (  0,   0,   0, 255)
    , numberColor = (  0,   0,   0, 255)
    , floatColor = (  0,   0,   0, 255)
    , boolColor = (  0,   0,   0, 255)

    , errorLineColor   = (255,   0,   0, 200)
    , warningLineColor = (  0, 255, 255, 200)
    , infoLineColor    = (  0,   0, 255, 200)

    , heatRamp = listArray (0, length smallHeatRamp - 1) smallHeatRamp 
    }

makeEntityColorLookupTable :: ColorDef -> Array CodeEntity ViewColor
makeEntityColorLookupTable def = array (toEnum 0, last [toEnum 0 ..]) $
    [ (CommentEntity, commentColor def)
    , (StringEntity, stringColor def)
    , (NormalEntity, normalColor def)
    , (HighlightEntity, highlightColor def)
    , (MajEntity, majColor def)
    , (EmptyEntity, emptyColor def)
    , (ViewEntity, viewColor def)
    , (KeywordEntity, keywordColor def)
    , (TypeEntity, typeColor def)
    , (ConstantEntity, constantColor def)
    , (LabelEntity, labelColor def)
    , (ConditionalEntity, conditionalColor def)
    , (RepeatEntity, repeatColor def)
    , (StructureEntity, structureColor def)
    , (StatementEntity, statementColor def)
    , (PreprocEntity, preprocColor def)
    , (MacroEntity, macroColor def)
    , (TypedefEntity, typedefColor def)
    , (ExceptionEntity, exceptionColor def)
    , (OperatorEntity, operatorColor def)
    , (IncludeEntity, includeColor def)
    , (StorageClassEntity, storageClassColor def)
    , (CharEntity, charColor def)
    , (NumberEntity, numberColor def)
    , (FloatEntity, floatColor def)
    , (BoolEntity, boolColor def)
    , (FunctionEntity, functionColor def)
    , (TagEntity, tagColor def)
    , (AttribTagEntity, attribTagColor def)
    , (ErrorEntity, errorLineColor def)
    , (WarningEntity, warningLineColor def)
    , (InfoEntity, infoLineColor def)
    ]


readHex :: Char -> Int
readHex c | 'a' <= c && c <= 'f' = fromEnum c - fromEnum 'a' + 10
          | 'A' <= c && c <= 'F' = fromEnum c - fromEnum 'A' + 10
          | '0' <= c && c <= '9' = read [c]
          | otherwise = 0

split :: Char -> String -> [String]
split _ "" =  []
-- Somehow GHC doesn't detect the selector thunks in the below code,
-- so s' keeps a reference to the first line via the pair and we havj
-- a space leak (cf. #4334).
-- So we need to make GHC see the selector thunks with a trick.
split c s = cons (case break (== c) s of
        (l, s') -> (l, case s' of
                         []     -> []
                         _:s'' -> split c s''))
  where cons ~(h, t) = h : t

-- | Parse a string of the form #rrggbb or #rrggbbaa
parseHtmlColor :: String -> Maybe ViewColor
parseHtmlColor ['#', r1, r2, g1, g2, b1, b2, a1, a2] = Just (r, g, b, a)
    where r = readHex r1 * 16 + readHex r2
          g = readHex g1 * 16 + readHex g2
          b = readHex b1 * 16 + readHex b2
          a = readHex a1 * 16 + readHex a2
parseHtmlColor ['#', r1, r2, g1, g2, b1, b2] = Just (r, g, b, 255)
    where r = readHex r1 * 16 + readHex r2
          g = readHex g1 * 16 + readHex g2
          b = readHex b1 * 16 + readHex b2
parseHtmlColor _ = Nothing

-- | Given a color configuration file, give back a color configuration
parseColorDef :: String -> ColorDef
parseColorDef txt = foldl' updateColorDef defaultColorDef vals
    where cleanSecondPart (a, []) = (a, [])
          cleanSecondPart (a, _:xs) = (a, xs)
          vals = map (cleanSecondPart . break ('=' ==)) 
               . concatMap (split ';')
               $ lines txt


-- | Function used in a fold to parse color configuration file.
updateColorDef :: ColorDef -> (String, String) -> ColorDef
updateColorDef def ("comment",val) =
    maybe def (\c -> def { commentColor = c }) $ parseHtmlColor val
updateColorDef def ("normal",val) =
    maybe def (\c -> def { normalColor = c }) $ parseHtmlColor val
updateColorDef def ("string",val) =
    maybe def (\c -> def { stringColor = c }) $ parseHtmlColor val
updateColorDef def ("highlight",val) =
    maybe def (\c -> def { highlightColor = c }) $ parseHtmlColor val
updateColorDef def ("maj",val) =
    maybe def (\c -> def { majColor = c }) $ parseHtmlColor val
updateColorDef def ("empty",val) =
    maybe def (\c -> def { emptyColor = c }) $ parseHtmlColor val
updateColorDef def ("view",val) =
    maybe def (\c -> def { viewColor = c }) $ parseHtmlColor val
updateColorDef def ("keyword",val) =
    maybe def (\c -> def { keywordColor = c }) $ parseHtmlColor val
updateColorDef def ("type",val) =
    maybe def (\c -> def { typeColor = c }) $ parseHtmlColor val
updateColorDef def ("constant", val) =
    maybe def (\c -> def { constantColor = c }) $ parseHtmlColor val

updateColorDef def ("label", val) =
    maybe def (\c -> def { labelColor = c }) $ parseHtmlColor val
updateColorDef def ("conditional", val) =
    maybe def (\c -> def { conditionalColor = c }) $ parseHtmlColor val
updateColorDef def ("repeat", val) =
    maybe def (\c -> def { repeatColor = c }) $ parseHtmlColor val
updateColorDef def ("structure", val) =
    maybe def (\c -> def { structureColor = c }) $ parseHtmlColor val
updateColorDef def ("statement", val) =
    maybe def (\c -> def { statementColor = c }) $ parseHtmlColor val
updateColorDef def ("preproc", val) =
    maybe def (\c -> def { preprocColor = c }) $ parseHtmlColor val
updateColorDef def ("macro", val) =
    maybe def (\c -> def { macroColor = c }) $ parseHtmlColor val
updateColorDef def ("typedef", val) =
    maybe def (\c -> def { typedefColor = c }) $ parseHtmlColor val
updateColorDef def ("include", val) =
    maybe def (\c -> def { includeColor = c }) $ parseHtmlColor val
updateColorDef def ("exception", val) =
    maybe def (\c -> def { exceptionColor = c }) $ parseHtmlColor val
updateColorDef def ("operator", val) =
    maybe def (\c -> def { operatorColor = c }) $ parseHtmlColor val
updateColorDef def ("storageClass",val) =
    maybe def (\c -> def { storageClassColor = c }) $ parseHtmlColor val

updateColorDef def ("number",val) =
    maybe def (\c -> def { numberColor = c }) $ parseHtmlColor val
updateColorDef def ("char",val) =
    maybe def (\c -> def { charColor = c }) $ parseHtmlColor val
updateColorDef def ("float",val) =
    maybe def (\c -> def { floatColor = c }) $ parseHtmlColor val
updateColorDef def ("bool",val) =
    maybe def (\c -> def { boolColor = c }) $ parseHtmlColor val

updateColorDef def ("function", val) =
    maybe def (\c -> def { functionColor = c }) $ parseHtmlColor val
updateColorDef def ("tag", val) =
    maybe def (\c -> def { tagColor = c }) $ parseHtmlColor val
updateColorDef def ("attribTag", val) =
    maybe def (\c -> def { attribTagColor = c }) $ parseHtmlColor val

updateColorDef def ("errorLine",val) =
    maybe def (\c -> def { errorLineColor = c }) $ parseHtmlColor val
updateColorDef def ("warningLine",val) =
    maybe def (\c -> def { warningLineColor = c }) $ parseHtmlColor val
updateColorDef def ("infoLine",val) =
    maybe def (\c -> def { infoLineColor = c }) $ parseHtmlColor val

updateColorDef def _ = def

