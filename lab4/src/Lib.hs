module Lib (
    satisfy,
    ParseReturn,
    resultParse,
    Parser,
    runParser,
) where

newtype ParseError = ParseError String

data ParseReturn a
    = SucRead
        { content :: String
        , resultParse :: a
        }
    | Error ParseError

newtype Parser a = Parser
    { runParser :: String -> ParseReturn a
    }

errorEOF :: ParseReturn a
errorEOF = Error $ ParseError "Unexpected EoF"

errorToken :: (Show b) => b -> ParseReturn a
errorToken c = Error $ ParseError $ "Unexpected " ++ show c

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser check
  where
    check [] = errorEOF
    check tts@(tok : toks) =
        if f tok
            then SucRead toks tok
            else errorToken tok

anyChar :: Parser Char
anyChar = satisfy (const True)
