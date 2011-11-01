-- | Define python langage
module CodeOverviewGenerator.Language.Python ( pythonCodeDef ) where

import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Language.Shell
import CodeOverviewGenerator.Color

-- | Python tokens
pythonStatement, pythonConditional, pythonRepeat, pythonOperator,
    pythonException, pythonInclude :: [String]
pythonStatement	= ["False", "None", "True", "as", "assert", "break"
                  ,"continue", "del", "exec", "global", "lambda"
                  ,"nonlocal", "pass", "print", "return", "with"
                  , "yield", "class", "def"]

pythonConditional = ["elif", "else", "if"]
pythonRepeat	  = ["for", "while"]
pythonOperator	  = ["and", "in", "is", "not", "or"]
pythonException	  = ["except", "finally", "raise", "try"]
pythonInclude	  = ["from", "import"]

-- | Python language definition.
pythonCodeDef :: CodeDef [CodeEntity]
pythonCodeDef = def
    where def = shellCodeDef
           { identParser = identWithPrime
           , strParser = Just $ stringParser False def
           , specialIdentifier = prepareKeywords
                [ (pythonStatement, StatementEntity)
                , (pythonConditional, ConditionalEntity)
                , (pythonRepeat, RepeatEntity)
                , (pythonOperator, OperatorEntity)
                , (pythonException, ExceptionEntity)
                , (pythonInclude, IncludeEntity)
                ]
           }

