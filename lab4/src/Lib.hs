module Lib (
    satisfy,
    ParseReturn,
    resultParse,
    content,
    Parser,
    parse,
    choice,
    bind,
    stringPars,
    anyChar,
    errMsg,
) where

data ParseReturn a
    = SucRead
        { content :: String
        , resultParse :: a
        }
    | ParseError {errMsg :: String}

newtype Parser a b = Parser {runParser :: a -> String -> ParseReturn b}

instance Functor (Parser a) where
    fmap f p = Parser $ \c s -> case runParser p c s of
        ParseError err -> ParseError err
        SucRead toks res -> SucRead toks (f res)

parse :: Parser () a -> (String -> ParseReturn a)
parse p = runParser p ()

bind :: Parser a b -> Parser b c -> Parser a c
bind n m = Parser $ \context input ->
    let res1 = runParser n context input
     in case res1 of
            ParseError err -> ParseError err
            SucRead toks res -> runParser m res toks

oneChar :: Char -> Parser u Char
oneChar c = satisfy (== c)

stringPars :: String -> Parser u String
stringPars [] = Parser $ \c s -> SucRead s ""
stringPars (t : ts) = reverse <$> foldl (\acc el -> bind acc (satisfyAcc (== el))) (fmap (: []) (satisfy (== t))) ts

satisfy :: (Char -> Bool) -> Parser u Char
satisfy f = Parser check
  where
    check _ [] = errorEOF
    check _ tts@(tok : toks) =
        if f tok
            then SucRead toks tok
            else errorToken tok

satisfyAcc :: (Char -> Bool) -> Parser String String
satisfyAcc f = applyContext (flip (:)) (satisfy f)

errorEOF :: ParseReturn a
errorEOF = ParseError "Unexpected EoF"

errorToken :: (Show b) => b -> ParseReturn a
errorToken c = ParseError $ "Unexpected " ++ show c

anyChar :: Parser u Char
anyChar = satisfy (const True)

choice :: Parser a b -> Parser a b -> Parser a b
choice m n =
    Parser
        ( \context s -> case runParser m context s of
            ParseError _ -> runParser n context s
            SucRead toks res -> SucRead toks res
        )

applyContext :: (a -> b -> c) -> Parser a b -> Parser a c
applyContext f p = Parser $ \c s -> case runParser p c s of
    ParseError err -> ParseError err
    SucRead toks res -> SucRead toks (f c res)

-- manyUntil :: (Char -> Bool) -> Parser a b -> Parser a [b]
-- manyUntil f p = Parser $ \c s -> let
--    walk [] _ = SucRead "" s
--    walk tts@(t:ts) rs = if f t then walk ts else SucRead tts rs
--    in
