module CodeOverviewGenerator.Language.Html ( htmlCodeDef ) where

import Control.Applicative
import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Color
import qualified CodeOverviewGenerator.ByteString as B
import Debug.Trace

htmlCodeDef :: ColorDef -> CodeDef [ViewColor]
htmlCodeDef colors = emptyCodeDef
            { multiLineCommBeg = strComment "<!--"
            , multiLineCommEnd = strComment "-->"
            , specificParser = [tagParser colors]
            }

tagParser :: ColorDef -> Parser [ViewColor]
tagParser colors = tagTranslater <$> between '<' '>' innerTag
  where tagTranslater pixels = bracketCol : pixels ++ [bracketCol]
        bracketCol = normalColor colors
        attribColor = attribTagColor colors
        spaceColor = emptyColor colors
        tagCol = tagColor colors

        innerTag = endTag
                <|> ((\idt sp lst -> trace (show (sp,lst)) $ replicate (B.length idt) tagCol
                                    ++ replicate sp spaceColor ++ lst) 
                        <$> identParse <*> eatWhiteSpace 4 <*> attribList)

        endTag =
            (\_ ident -> bracketCol : replicate (B.length ident) tagCol)
              <$> charParse '/' <*> identParse

        attribList = concat <$> many attrib

        attrib = (\spCount a _ b white c -> 
                           replicate spCount spaceColor
                        ++ replicate (B.length a) attribColor
                        ++ [bracketCol] ++ b 
                        ++ replicate white spaceColor
                        ++ if c == '/' then [bracketCol] else [])
              <$> eatWhiteSpace 4
                    <*> identParse
                    <*> charParse '=' 
                    <*> attribVal
                    <*> eatWhiteSpace 4
                    <*> (charParse '/' <|> return ' ')

        attribVal = trace "v" $
              ((\b -> replicate (B.length b) attribColor) <$> identParse)
          <|> stringParser False (htmlCodeDef colors) colors

