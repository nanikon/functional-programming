# Лабораторная работа №3

*Выполнила Никонова Наталья*

## Цель работы

Получить навыки работы с вводом-выводом, потоковой обработкой данных, командной строкой.

## Задание

Реализовать лабораторную лаботу с предмета "Вычислительная математика" посвященную интерполяции со следующими дополнениями:

- обязательно должна быть реализованна линейная интерполяция;
- настройки алгоритма интерполяциии выводимых данных должны задаваться через аргументы командной строки:
    - какие алгоритмы использовать (в том числе два сразу);
    - частота дискретизации результарующих данных;
    - и т.п.
- входные данные должны задаваться в текстовом формате на подобии csv и подаваться на стандартный ввод, входные данные должны быть отсортированны по возрастанию х;
- выходные данные должны подаваться на стандартный вывод;
- программа должна работать в потоков режиме, это значит, что при запуске программы она должна ожидать получения данных на стандартный ввод и, по мере получения достаточного количества данных, должна выводить рассчитанные точки в стандартный вывод.

Общие требования:

- программа должна быть организованна в функциональном стиле;
- ввод/вывод должен быть отделен от алгоритмов аппроксимации;
- требуется использовать идиоматичный для технологии стиль программирования.

## Реализация

Первый модуль - представление входных данных и их обработка. Для этого я создала два новых типа - Point, обозначающий одну точку, и PointWindow, с помощью которого представляется окно из точек. По сути это массив точек, и все функции, работающие с ним, подразумевают, что в нем даны точки по уменьшению х (для более быстрых перестроек окна при получении новой точки).

```haskell
type Point = (Double, Double)

type PointWindow = [Point]
```

Для них определены такие функции, как парсин строки в точку (подразумевается формат через двоеточие), форматированный вывод списка точек с использованием библиотки fmt, изменение окна - добавление новой точки в начало и удаление последней из конца, а также генерация списка точек для интерполирования по окну и заданному количеству точек.

```haskell
changeWindow :: PointWindow -> Point -> PointWindow
changeWindow w p = p : take (length w - 1) w

generateXs :: PointWindow -> Int -> [Double]
generateXs [] _ = []
generateXs w@(p : _) n =
    let
        minX = fst . getLastPoint $ w
        period = (fst p - minX) / fromIntegral (n + 1)
     in
        scanl (+) (minX + period) (replicate (n - 1) period)

parsePoint :: String -> Point
parsePoint s =
    let numbers = splitOn ";" s
     in (read (head numbers), read (numbers !! 1))

formatPointList :: [Point] -> Builder
formatPointList ps = foldl1 (\a b -> a +| ", " |+ b) (map tupleF ps)
```

Далее модуль алогритмов интерполяции. Каждый из них представлен функцией, принимающей коно и одну точку и интерполирующей её. Первый метод - линейная интерполяция. Ищет два ближайших с обоих сторон икса, и далее рассчитывает по заданной формуле.

```haskell
linInterOnePoint :: PointWindow -> Double -> Double
linInterOnePoint w x =
    let
        number = length . takeWhile (\e -> fst e > x) $ w
        point1 = w !! (number - 1)
        point0 = w !! number
        y1 = snd point1
        y0 = snd point0
        x1 = fst point1
        x0 = fst point0
     in
        y0 + (y1 - y0) / (x1 - x0) * (x - x0)
```

Второй алгоритм - алгоритм Лагранжа. В полиноме Лагранжа есть повторяемая для одного окна, но разных точек интерполяции часть в знаменателе и числителе - поэтому они рассчитываются отдельными функциями, которые используют мемоизацию библиотеки stable-memo для ускорения вычислений.

```haskell
-- make multiply by differenses second arg and first arg, without element in n index
makeMultiplyByDiff :: [Double] -> Double -> Int -> Double
makeMultiplyByDiff listX element number = makeMultiplyByDiff' listX element number 1 0
  where
    makeMultiplyByDiff' xs x n result cur_ptr
        | length xs == cur_ptr = result
        | n == cur_ptr = makeMultiplyByDiff' xs x n result (cur_ptr + 1)
        | otherwise = makeMultiplyByDiff' xs x n (result * (x - (xs !! cur_ptr))) (cur_ptr + 1)

makeDivider :: [Double] -> Int -> Double
makeDivider xs n = makeMultiplyByDiff xs (xs !! n) n

makeConstMult :: PointWindow -> Int -> Double
makeConstMult = memo2 (\w n -> snd (w !! n) / makeDivider (map fst w) n)

computeOnePolynom :: PointWindow -> Double -> Int -> Double
computeOnePolynom w x n = makeConstMult w n * makeMultiplyByDiff (map fst w) x n

lagrangInterOnePoint :: PointWindow -> Double -> Double
lagrangInterOnePoint w x = sum (map (computeOnePolynom w x) [0 .. length w - 1])
```

Третий модуль - аргументы командной строки. Для этого я использовала библиотеку optparse-applicative. Описала все аргументы в структуре, и далее собрала их описание для help-а. Аргументы поддерживаются следующие:
- размер окна - первый, позиционный, задает сколько точек содержится в окне;
- количество точек, на котоыре делится окно и которые интерполируются;
- флаг, использовать ли метод линейной интерполяции для вычислений;
- флаг, использовать ли метод Лагранжа для вычислений;
Если ни один флаг не указан, работают оба метода.

```haskell
data Params = Params
    { windowSize :: Int
    , pointCount :: Int
    , useLinear :: Bool
    , useLagrang :: Bool
    }

mkParams :: Parser Params
mkParams =
    Params
        <$> argument auto (metavar "WINDOW_SIZE" <> help "The number of points in the window")
        <*> argument auto (metavar "POINT_COUNT" <> help "The number of calucalted points in the window")
        <*> switch (long "linear" <> short 'i' <> help "compute by linear method (if no method, then use all)")
        <*> switch (long "lagrang" <> short 'a' <> help "compute by lagrang method (if no method, then use all)")

cmdLineParser :: IO Params
cmdLineParser = execParser opts
  where
    opts =
        info
            (mkParams <**> helper)
            (fullDesc <> progDesc "Interpolation with streaming processing")
```

Последний модуль - ввода/вывода, он же основной. В main парсятся аргументы командной строки, и дальше передается управление функции work. В ней лениво получем входящий поток с помощью getContents, разбиваем по строкам и каждую маппим в точку. Далее ожидаем минимальное число точек для первого окна, а далее для каждой последующей производим рассчеты и выводим результат.

```haskell
main :: IO ()
main = cmdLineParser >>= work

work :: Params -> IO ()
work params = do
    input <- getContents
    let points = map parsePoint $ words input
    let firstOffset = windowSize params - 1
    foldM_ handleWindow (reverse ((0.0, 0.0) : take firstOffset points)) (drop firstOffset points)
  where
    handleWindow acc point = do
        let window = changeWindow acc point
        putStr $ fmt $ "Current window: " +| formatPointList window |+ "\n"
        let xs = generateXs window (pointCount params)
        let linearResult = "Linear interpolation: " +| formatPointList (mapAndZip (linInterOnePoint window) xs) |+ "\n"
        let lagrangResult = "Lagrang interpolation: " +| formatPointList (mapAndZip (lagrangInterOnePoint window) xs) |+ "\n"
        when (useLinear params) $ putStr $ fmt linearResult
        when (useLinear params) $ putStr $ fmt lagrangResult
        unless (useLinear params || useLagrang params) $ putStr $ fmt $ linearResult <> lagrangResult
        return window
    mapAndZip f a = zip a (map f a)
```

В качестве тестирования были написаны unit и property-based тесты. В рамках unit- тестов я првоерила работу алгоритмов по заранее рассчитанным значениям.

```haskell
unitTests =
    testGroup
        "Unit tests"
        [ testLagrange
        , testLinear
        , testParse
        ]
testLagrange =
    testGroup
        "Lagrange method test"
        [ testCase "in x point ys must be equal" $ do
            let window = [(0.3, 0.1), (0.1, 0.0)]
            let xs = map fst window
            let ys = map snd window
            all (\el -> uncurry (-) el < 0.001) (zip ys (map (lagrangInterOnePoint window) xs)) @? "differens beetwen y more than 0.001"
        , testCase "same withalready computed values" $ do
            let window = [(6.0, 9.0), (5.0, 5.0), (2.0, 4.0)]
            let xs = [3.0, 4.0, 5.5]
            let ys = [2.5, 2.83, 6.77]
            all (\el -> uncurry (-) el < 0.001) (zip ys (map (lagrangInterOnePoint window) xs)) @? "differens beetwen y more than 0.001"
        ]
testLinear =
    testGroup
        "Linear method test"
        [ testCase "same withalready computed values" $ do
            let window = [(6.0, 9.0), (5.0, 5.0), (2.0, 4.0)]
            let xs = [3.0, 4.0, 5.5]
            let ys = [4.3333, 4.6666, 7.0]
            all (\el -> uncurry (-) el < 0.001) (zip ys (map (linInterOnePoint window) xs)) @? "differens beetwen y more than 0.001"
        ]
testParse =
    testGroup
        "Test parse point from string"
        [ testCase "parse intvalues in string" $ parsePoint "1;1" @?= (1.0, 1.0)
        , testCase "parse double values in string" $ parsePoint "2.5;0.2345" @?= (2.5, 0.2345)
        ]
```

В property-based тестировании я использовала библиотеку quickcheck, и првоерила что многочлен Лагранжа в точках, по которым он строится, принимает соответствующие значения.

```haskell
propertyBasedTests :: TestTree
propertyBasedTests =
    testGroup
        "Property-based tests"
        [ testProperty "lagrange must be eq with y in x" (lagrangeEq :: [(Double, Double)] -> Bool)
        ]

lagrangeEq :: [(Double, Double)] -> Bool
lagrangeEq w = all (\el -> uncurry (-) el < 0.001) (zip (ys (window w)) (map (lagrangInterOnePoint (window w)) (xs (window w))))
  where
    window = sortBy (flip (\a b -> compare (fst a) (fst b))) . nubBy (\a b -> fst a == fst b)
    xs = map fst
    ys = map snd
```
