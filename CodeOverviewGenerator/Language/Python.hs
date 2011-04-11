module CodeOverviewGenerator.Language.Python ( pythonCodeDef ) where

import qualified Data.Map as Map

import CodeOverviewGenerator.Language
import CodeOverviewGenerator.Language.Shell
import CodeOverviewGenerator.Color

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

pythonCodeDef :: ColorDef -> CodeDef
pythonCodeDef colors = def
    where def = (shellCodeDef colors)
           { identParser = identWithPrime
           , strParser = Just $ stringParser False def
           , specialIdentifier = Map.fromList $
               prepareKeywords pythonStatement (statementColor colors)
             ++ prepareKeywords pythonConditional (conditionalColor colors)
             ++ prepareKeywords pythonRepeat (repeatColor colors)
             ++ prepareKeywords pythonOperator (operatorColor colors)
             ++ prepareKeywords pythonException (exceptionColor colors)
             ++ prepareKeywords pythonInclude (includeColor colors)
           }

