-- | Define ruby language parser
module CodeOverviewGenerator.Language.Ruby ( rubyCodeDef ) where

{-import qualified Data.Map as Map-}

import Control.Applicative
import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Language.Shell
import CodeOverviewGenerator.Color
import qualified CodeOverviewGenerator.ByteString as B

-- | Ruby tokens
rDefines, rKeywords, rBoolean, rConditional, rControl, 
    rRepeat, rPseudovar  :: [String]
rDefines = ["def", "class", "undef", "module", "end"]
rKeywords = ["super", "yield", "alias", "undef" ]

rBoolean = ["true", "false"]
rControl = ["and", "break", "in", "next", "not", "or", "redo", 
            "rescue", "retry", "return"]
rConditional = ["if", "case", "then", "else", "when", "elsif", "unless"]
rRepeat = ["while", "until", "for", "in"]
rPseudovar = ["nil", "self", "__FILE__", "__LINE__"]

symbolParser :: Parser [CodeEntity]
symbolParser = (\_ ident -> replicate (B.length ident) ConstantEntity)
            <$> token ":" <*> identParse

-- | Ruby language definition.
rubyCodeDef :: CodeDef [CodeEntity]
rubyCodeDef = def
    where def = shellCodeDef
            { multiLineCommBeg = strComment "=begin"
            , multiLineCommEnd = strComment "=end"
            , specialIdentifier = prepareKeywords
                [ (rDefines, PreprocEntity)
                , (rKeywords, KeywordEntity)
                , (rControl, StatementEntity)
                , (rConditional, ConditionalEntity)
                , (rBoolean, BoolEntity)
                , (rRepeat, RepeatEntity)
                , (rPseudovar, ConstantEntity)
                ]
            , specificParser = [intParser, symbolParser]
            }

