module Lib (
    satisfy,
    ParseReturn,
    resultParse,
    Parser,
    runParser,
    choice,
) where

newtype ParseError = ParseError String

data ParseReturn a
    = SucRead
        { content :: String
        , resultParse :: a
        }
    | Error ParseError

newtype Parser a = Parser
    {runParser :: String -> ParseReturn a}

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

choice :: Parser a -> Parser a -> Parser a
choice m n =
    Parser
        ( \s -> case runParser m s of
            Error _ -> runParser n s
            SucRead toks res -> SucRead toks res
        )

bind :: (Semigroup a) => Parser a -> Parser a -> Parser a
bind n m =
    Parser
        ( \s -> case runParser m s of
            Error err -> Error err
            SucRead toks res -> case runParser n toks of
                Error err' -> Error err'
                SucRead toks' res' -> SucRead toks' (res <> res')
        )
