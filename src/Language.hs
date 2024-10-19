module Language where

import Text.Parsec
import Text.Parsec.Token
import Text.ParserCombinators.Parsec.Language

refinementDef :: LanguageDef st
refinementDef =
    emptyDef
        { commentStart = "/**"
        , commentEnd = "*/"
        , commentLine = "//"
        , nestedComments = True
        , identStart = letter
        , identLetter = alphaNum <|> oneOf "_'"
        , opStart = opLetter refinementDef
        , opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
        , reservedOpNames = ["/\\", "\\/", "->", "#", "~", "<==>", "==>"]
        , reservedNames = ["VAR", "PROCEDURE", "DO", "OD", "IF", "THEN", "ELSE", "CON", "of"]
        , caseSensitive = True
        }

refinement :: TokenParser st
refinement = makeTokenParser refinementDef
