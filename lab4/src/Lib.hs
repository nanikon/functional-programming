module Lib (
    satisfy,
    ParseReturn,
    resultParse,
) where

newtype ParseError = ParseError String

data ParseReturn a
    = SucRead
        { content :: String
        , resultParse :: a
        }
    | Error ParseError

-- type Parser a = Parser a { unParser :: }

errorEOF :: ParseReturn a
errorEOF = Error $ ParseError "Unexpected EoF"

errorToken :: (Show b) => b -> ParseReturn a
errorToken c = Error $ ParseError $ "Unexpected " ++ show c

satisfy :: (Char -> Bool) -> (String -> ParseReturn Char)
satisfy f = check
  where
    check [] = errorEOF
    check tts@(tok : toks) =
        if f tok
            then SucRead toks tok
            else errorToken tok

anyChar :: (String -> ParseReturn Char)
anyChar = satisfy (const True)
