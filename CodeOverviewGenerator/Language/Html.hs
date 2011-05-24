module CodeOverviewGenerator.Language.Html ( htmlCodeDef ) where

import Control.Applicative
import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Color
import qualified CodeOverviewGenerator.ByteString as B

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

        colorize color byteString =
            replicate (B.length byteString) color

        innerTag = endTag
                <|> ((\idt sp lst -> idt ++ replicate sp spaceColor ++ lst) 
                        <$> tagName <*> eatWhiteSpace 4 <*> attribList)

        endTag = (const $ (bracketCol:)) <$> charParse '/' <*> tagName

        tagName = convertToColors
                     <$> identParse 
                     <*> (nameSpaced <|> return [])
            where nameSpaced = (\_ idt -> bracketCol : colorize tagCol idt)
                            <$> charParse ':' <*> identParse
                  convertToColors idt rest = colorize tagCol idt ++ rest

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

        attribVal =
              ((\b -> replicate (B.length b) attribColor) <$> identParse)
          <|> stringParser False (htmlCodeDef colors) colors

