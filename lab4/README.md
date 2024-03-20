# Лабораторная работа №4

*Выполнила Никонова Наталья*

## Цель работы

Получить навыки работы со специфичными для выбранной технологии/языка приемами.

## Задание

Реализовать библиотеку парсер комбинаторов.

## Реализация

Для начала я определила тип данных, показывающий, что возвращает парсер. В случае успеха это объект, который удалось распарсить, и оставшаяся входная строка. В случае неудачи - сообщение об ошибке.

```haskell
data ParseReturn a
    = SucRead
        { content :: String
        , resultParse :: a
        }
    | ParseError {errMsg :: String}
```

Далее определила тип функции парсера. На вход она принимает контекст и строку, а возвращает тип данных результата.

```haskell
newtype Parser a b = Parser {runParser :: a -> String -> ParseReturn b}
```

Также этот тип функций сделала функтором, чтобы можно было преобразовывать результаты парсинга в случае успеха.

```haskell
instance Functor (Parser a) where
    fmap f p = Parser $ \c s -> case runParser p c s of
        ParseError err -> ParseError err
        SucRead toks res -> SucRead toks (f res)
```

Далее реализовала конструктор низкоуровнего парсера для Char - он принимает функцию и возвращает успех если для первого символа строки она выполняется успешно. И несколько простых функций на основе него.

```haskell
satisfy :: (Char -> Bool) -> Parser u Char
satisfy f = Parser check
  where
    check _ [] = errorEOF
    check _ tts@(tok : toks) =
        if f tok
            then SucRead toks tok
            else errorToken tok

oneChar :: Char -> Parser u Char
oneChar c = satisfy (== c)

anyChar :: Parser u Char
anyChar = satisfy (const True)
```

Далее для работы, по аналогии с булевой алгеброй, надо было сделать функцию выбора парсера и функцию склейки. Вначале сделала "или" - функцию, применяющую второй парсер в случае если первый парсер отработал с ошибкой. Но выглядит как один парсер.

```haskell
choice :: Parser a b -> Parser a b -> Parser a b
choice m n =
    Parser
        ( \context s -> case runParser m context s of
            ParseError _ -> runParser n context s
            SucRead toks res -> SucRead toks res
        )
```

Дальше взялась за "и". Для этого сделала функцию, которая в случае успеха видоизменяет результат в соответствии с контекстом, и функцию bind, которая результат работы первого парсера передает как вход второго парсера.

```haskell
applyContext :: (a -> b -> c) -> Parser a b -> Parser a c
applyContext f p = Parser $ \c s -> case runParser p c s of
    ParseError err -> ParseError err
    SucRead toks res -> SucRead toks (f c res)

bind :: Parser a b -> Parser b c -> Parser a c
bind n m = Parser $ \context input ->
    let res1 = runParser n context input
     in case res1 of
            ParseError err -> ParseError err
            SucRead toks res -> runParser m res toks
```

С помощью этих комбинаторов мне удалось собрать парсер посложнее - который сравнивает начало переданной и входной строки. Если переданная строка полностью помещается в начало входной - то парсер возвращает ок.

```haskell
stringPars :: String -> Parser u String
stringPars [] = Parser $ \c s -> SucRead s ""
stringPars (t : ts) = reverse <$> foldl (\acc el -> bind acc (satisfyAcc (== el))) (fmap (: []) (satisfy (== t))) ts
```

Также с помощью них я ещё написала комбинаторы для множественного применения одного парсера - many (0 или много), many1 (1 или много) и skipMany (пропускающий 0 или более символов).

```haskell
manyUntil :: (b -> d -> d) -> d -> Parser a b -> Parser a d
manyUntil f startAcc p = Parser $ \c s ->
    let
        walk "" rs = SucRead "" rs
        walk tts@(t : ts) rs = case runParser p c tts of
            ParseError _ -> SucRead tts rs
            SucRead _ res -> walk ts (f res rs)
     in
        walk s startAcc

many :: Parser a Char -> Parser a String
many = manyUntil (:) ""

many1 :: Parser Char Char -> Parser Char String
many1 p = bind p (applyContext (:) (many p))

skipMany :: Parser a Char -> Parser a ()
skipMany = manyUntil (\_ _ -> ()) ()
```
