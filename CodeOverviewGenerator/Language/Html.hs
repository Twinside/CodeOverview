module CodeOverviewGenerator.Language.Html ( htmlCodeDef ) where

import Control.Applicative
import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Color
import qualified CodeOverviewGenerator.ByteString as B

htmlCodeDef :: CodeDef [CodeEntity]
htmlCodeDef = emptyCodeDef
            { multiLineCommBeg = strComment "<!--"
            , multiLineCommEnd = strComment "-->"
            , specificParser = [tagParser]
            }

tagParser ::  Parser [CodeEntity]
tagParser = tagTranslater <$> between '<' '>' innerTag
  where tagTranslater pixels = bracketCol : pixels ++ [bracketCol]
        bracketCol = NormalEntity
        attribColor = AttribTagEntity

        colorize color byteString =
            replicate (B.length byteString) color

        innerTag = endTag
                <|> ((\idt sp lst -> idt ++ replicate sp EmptyEntity ++ lst) 
                        <$> tagName <*> eatWhiteSpace 4 <*> attribList)

        endTag = (const $ (bracketCol:)) <$> charParse '/' <*> tagName

        tagName = convertToColors
                     <$> identParse 
                     <*> (nameSpaced <|> return [])
            where nameSpaced = (\_ idt -> bracketCol : colorize TagEntity idt)
                            <$> charParse ':' <*> identParse
                  convertToColors idt rest = colorize TagEntity idt ++ rest

        attribList = concat <$> many attrib

        attrib = (\spCount a _ b white c -> 
                           replicate spCount EmptyEntity
                        ++ replicate (B.length a) attribColor
                        ++ [bracketCol] ++ b 
                        ++ replicate white EmptyEntity
                        ++ if c == '/' then [bracketCol] else [])
              <$> eatWhiteSpace 4
                    <*> identParse
                    <*> charParse '=' 
                    <*> attribVal
                    <*> eatWhiteSpace 4
                    <*> (charParse '/' <|> return ' ')

        attribVal =
              ((\b -> replicate (B.length b) attribColor) <$> identParse)
          <|> stringParser False htmlCodeDef

