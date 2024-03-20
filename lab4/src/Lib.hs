module Lib (
    satisfy,
    ParseReturn,
    resultParse,
    Parser,
    parse,
    choice,
) where

newtype ParseError = ParseError String

data ParseReturn a
    = SucRead
        { content :: String
        , resultParse :: a
        }
    | Error ParseError

newtype Parser a b = Parser {runParser :: a -> String -> ParseReturn b}

parse :: Parser () a -> (String -> ParseReturn a) -- parser - to start
parse p = runParser p ()

bind :: Parser a b -> Parser b c -> Parser a c
bind n m = Parser $ \context input ->
    let res1 = runParser n context input
     in case res1 of
            Error err -> Error err
            SucRead toks res -> runParser m res toks

satisfy :: (Char -> Bool) -> Parser u Char
satisfy f = Parser check
  where
    check _ [] = errorEOF
    check _ tts@(tok : toks) =
        if f tok
            then SucRead toks tok
            else errorToken tok

errorEOF :: ParseReturn a
errorEOF = Error $ ParseError "Unexpected EoF"

errorToken :: (Show b) => b -> ParseReturn a
errorToken c = Error $ ParseError $ "Unexpected " ++ show c

anyChar :: Parser u Char
anyChar = satisfy (const True)

choice :: Parser a b -> Parser a b -> Parser a b
choice m n =
    Parser
        ( \context s -> case runParser m context s of
            Error _ -> runParser n context s
            SucRead toks res -> SucRead toks res
        )

-- func1 :: (a -> b) -> Parser a -> Parser b -- functor
-- func1 f p1 = Parser $ \s -> case runParser p1 s of
--    Error err -> Error err
--    SucRead toks res -> SucRead toks (f res)
