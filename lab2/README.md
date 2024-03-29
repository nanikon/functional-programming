# Лабораторная работа №2. Вариант Separate Chaining Hashmap

## Цель работы

Освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property based testing)

## Задание

Реализовать предложенную структуру данных. Требования

- Функции
    - добавление и удаление элементов;
    - фильтрация;
    - отображение (map);
    - свертки (левая и правая);
    - структура должна быть моноидом.
- Структуры должны быть неизменяемыми.
- Библиотека должна быть протестирована в рамках unit testing.
- Библиотека должна быть протестирована в расках property-based тестирования (минимум 3 свойства, включая свойство моноида).
- Структура должна быть полиморфной
- Требуется использовать идиоматичный для технологии стиль программирования.

## Реализация

В моей реализации структура представлетс из себя значение максимального коэффициента заполнености, и списка списков из пар ключ-значение. Индекс элемента в первом измерении показывает, какой хэш (точнее, его модуль при делении на длину этого измерения) имеет ключ. Второе измерение - цепочка из пар, для которых хэш ключа совпал.

```
0.8     (0) [] -> [(key1, value1), (key2, value2), ...]
        (1) [] -> [(key3, value3), (key4, value4), ...]
        ...
```

Для пары ключ-значения и их цепочки я сделала новое именование уже существующих типов, а для самой хэшмапы определила новый тип.

```haskell
type Elem a b = (a, b)
type Bucket a b = [Elem a b]
data SepChainHashMap a b = SepChainHashMap
    { filledHashMap :: Double
    , dataHashMap :: [Bucket a b]
    }
```

Этот тип реализует следующие классы: Eq, Show (для удобства тестирования), Semigroup, Monoid **(для выполнения условия задания)**

```haskell
instance (Eq a, Eq b) => Eq (SepChainHashMap a b) where
    (==) x y = (filledHashMap x == filledHashMap y) && (dataHashMap x == dataHashMap y)

instance (Show a, Show b) => Show (SepChainHashMap a b) where
    show x = "{ " ++ show (filledHashMap x) ++ " - " ++ show (dataHashMap x) ++ "}"

instance (Hashable a, Ord a) => Semigroup (SepChainHashMap a b) where
    (<>) x y = createHashMap (max (filledHashMap x) (filledHashMap y)) (concatData x ++ concatAndFilterData x y)
      where
        concatData = concat . dataHashMap
        concatAndFilterData a = filter (\e -> fst e `notElem` map fst (concatData a)) . concatData

instance (Hashable a, Ord a) => Monoid (SepChainHashMap a b) where
    mempty = SepChainHashMap 0 []
```

И последняя часть интерфейса - экспоритуемые функции

```haskell
createHashMap :: (Hashable a) => Double -> [(a, b)] -> SepChainHashMap a b
addElem :: (Hashable a, Eq b) => SepChainHashMap a b -> a -> b -> SepChainHashMap a b
deleteElem :: (Hashable a) => SepChainHashMap a b -> a -> SepChainHashMap a b
getElem :: (Hashable a) => SepChainHashMap a b -> a -> Maybe b
filterHashMap :: ((a, b) -> Bool) -> SepChainHashMap a b -> SepChainHashMap a b
mapHashMap :: (Hashable c) => (Elem a b -> Elem c d) -> SepChainHashMap a b -> SepChainHashMap c d
foldlHashMap :: (c -> Elem a b -> c) -> c -> SepChainHashMap a b -> c
foldrHashMap :: (Elem a b -> c -> c) -> c -> SepChainHashMap a b -> c
getSize :: SepChainHashMap a b -> Int 
getCurrentFilled :: (Eq a, Eq b) => SepChainHashMap a b -> Double
```

Помимо необходимых функций я также сделала функцию создания хэшмапы - её и использую выше в бинарной операции, так как на мой взгляд это быстрее, чем по очереди добавлять элементы из одной мапы в другую (при дубликатах ключей значение сохранится из первого аргумента).

Сама функция создания, во-первых, валидирует значение заполненности и высчитывает, сколько должно быть бакетов, чтобы заполненность была в два раза меньше заданной при указанном начальном количестве элементов. Во-вторых, убирает дубликаты ключей (здесь не определено, какое значение останется). В третьих, рассчитывает для ключей хэши, соритрует и группирует пары по этому признаку в списки. Для значений хэша, у которых не нашлись данные, вставляются пробелы.

```haskell
createHashMap filled pairs
    | (filled <= 0) || (filled >= 1) = error "Filled must be in (0; 1)"
    | otherwise = SepChainHashMap filled (makeHashMapData (getLenForHalfFilled filled (length (deleteDublicates pairs))) (deleteDublicates pairs))
  where
    makeHashMapData len data_ = fillProbels [] 0 len (groupAndSortByHash len data_)
    fillProbels result curMod len input
        | curMod == len = reverse result
        | null input = reverse result ++ replicate (len - curMod) []
        | curMod == shortHashElemFromHeadBucket input len = fillProbels (head input : result) (curMod + 1) len (tail input)
        | otherwise = fillProbels ([] : result) (curMod + 1) len input
    deleteDublicates = DL.nubBy (equalRes fst)
    groupAndSortByHash len xs = DL.groupBy (equalRes (shortHashElem len)) (DL.sortBy (compareRes (shortHashElem len)) xs)
```

Также помимо функций в задании я сделала функцию получения пары по ключу - на случай отсутствия резульат отдается через Maybe. А заодно несколько нужных потом функций: 
- shortHash - модуль от деления модуля хэша (благодаря ограничению на класс типов Hashable функция hash должна быть реализована) на количество бакетов (и несколько оберток для него), 
- getNeededBucket - получение соответствующего ключу бакета и его номера (по логике такой бакет найдется всегда, так что после фильтра спокойно берем head)
- getNeededElem - по ключу ищем в бакете нужную пару и её номер в цепочке. Тут её может и не быть, поэтому возвращаем список, пустой или из одного элемента.

```haskell
shortHash :: (Hashable a) => a -> Int -> Int
shortHash key len = abs (hash key) `mod` len

getNeededBucket :: (Hashable a) => SepChainHashMap a b -> a -> (Int, Bucket a b)
getNeededBucket hM key = head $ filter (\b -> fst b == shortHash key (length (dataHashMap hM))) (zip [0 ..] (dataHashMap hM))

getNeededElem :: (Hashable a) => [Elem a b] -> a -> [(Int, Elem a b)]
getNeededElem b key = filter (\e -> fst (snd e) == key) (zip [0 ..] b)

getElem hM key =
    case getNeededElem (snd (getNeededBucket hM key)) key of
            [] -> Nothing
            (_, (_, v)) : _ -> Just v
```

Ещё для теста есть getSize - сколько пар лежит в хешмапе, и getCurrentFilled - отношение числа заполненных бакетов ко всем. Врочем, их реализация тривиальна, и перейдем к функциям по заданию.

**Добавление и удаление элементов**

При добавлении я ищу нужный бакет. Если он пустой - новый бакет это просто пара. Если нет - проверяю, нет ли уже такого ключа. Если есть - собираю новый бакет из начала, конца и новой пары (тут конечно соблюдаемый мной порядок не так важен, но все равно два списка конкатенировать, если его нарушить - все равно не соптимизировать). А дальше заново собираю хэшмапу - беру начальные бакеты, новый и конечные. Потом проверяю заполненность - если она стала равна максимальной или больше - пересоздаю мапу. А по алгоритму создания заполненность новой мапы как раз станет в два раза меньше максимальной. 

```haskell
addElemWithoutCheckFill :: (Hashable a) => SepChainHashMap a b -> a -> b -> SepChainHashMap a b
addElemWithoutCheckFill hM key value =
    let (number, b) = getNeededBucket hM key
        d = dataHashMap hM
        newB = case b of
            [] -> [(key, value)]
            _ -> case getNeededElem b key of
                [] -> (key, value) : b
                (numberElem, (_, _)) : _ -> take numberElem b ++ (key, value) : getListAfterElem b numberElem
     in SepChainHashMap (filledHashMap hM) (take number d ++ newB : getListAfterElem d number)

checkFillAndExpand :: (Hashable a, Eq b) => SepChainHashMap a b -> SepChainHashMap a b
checkFillAndExpand hM
    | getCurrentFilled hM >= filledHashMap hM = createHashMap (filledHashMap hM) (concat . dataHashMap $ hM)
    | otherwise = hM

addElem hM key value = checkFillAndExpand $ addElemWithoutCheckFill hM key value
```

Удаление можно сказать симметрично - нахожу новый бакет, его пересобираю без нужного элемента - есть он есть, и потом с новым бакетом пересобираю хэшмапу. Только на заполненность проверять не надо.

```haskell
deleteElemFromBucket :: (Hashable a) => Bucket a b -> a -> Bucket a b
deleteElemFromBucket b key = case getNeededElem b key of
    [] -> b
    (number, (_, _)) : _ -> take number b ++ getListAfterElem b number

deleteElem hM key =
    let (number, b) = getNeededBucket hM key
     in SepChainHashMap (filledHashMap hM) (take number (dataHashMap hM) ++ deleteElemFromBucket b key : getListAfterElem (dataHashMap hM) number)
```

**Функции высшего порядка**

Фильтрация - отображаю список бакетов через фильтрацию пар в каждом

```haskell
filterHashMap cond hM = SepChainHashMap (filledHashMap hM) (map (filter cond) (dataHashMap hM))
```

Отображение - создаю новую хэшмапу с тем же коэффициентом заполнености, а в качестве данных - отображенные пары.

```haskell
mapHashMap func hM = createHashMap (filledHashMap hM) (concatMap (map func) . dataHashMap $ hM)
```

Свертки - сворачиваю все пары, для левой - по очереди начиная с бакета, соответствующего наименьшему хэшу, для правой - наоборот.

```haskell
foldlHashMap func startAcc = foldl (foldl func) startAcc . dataHashMap

foldrHashMap func startAcc = foldr (flip (foldr func)) startAcc . dataHashMap
mapHashMap func hM = createHashMap (filledHashMap hM) (concatMap (map func) . dataHashMap $ hM)
```

**Unit тестирование**

Есть набор тестов на каждую функцию - при каждой следующей я считаю что предыдущая работает корректно и её можно использовать для тестирования. Они сгруппированны по проверяемым функциям.

```haskell
testCreate =
    testGroup
        "Test createHashMap"
        [ testCase "current filled less then inited" $ do
            let initedFill = 0.8
            let hM = createHashMap initedFill [('b', "a"), ('a', "a")]
            getCurrentFilled hM <= initedFill @? "Current fill more than inited"
        , testCase "remove dublicates key" $ getSizeMap (createHashMap 0.8 [('a', '1'), ('a', '2')]) @?= 1
        , testCase "don't remove dublicates hash" $ getSizeMap (createHashMap 0.8 [('a', '1'), ('f', '2')]) @?= 2
        , testCase "two elem with same hash key not change fill" $ do
            let sameHashHM = createHashMap 0.8 [('a', '1'), ('f', '2')]
            let diffHashHM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            getCurrentFilled sameHashHM < getCurrentFilled diffHashHM @? "Not less filled where less buckets use"
        ]
testGet =
    testGroup
        "Test getElem"
        [ testCase "can get elem with exists key" $ do
            let diffHashHM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            getElem diffHashHM 'a' @?= Just '1'
        , testCase "can get correct elem then two key has same hash" $ do
            let sameHashHM = createHashMap 0.8 [('a', '1'), ('f', '2')]
            getElem sameHashHM 'a' @?= Just '1'
            getElem sameHashHM 'f' @?= Just '2'
        , testCase "can't get elem with not exists key" $ do
            let hM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            getElem hM 'c' @?= Nothing
            getElem hM 'f' @?= Nothing
        ]
testDelete =
    testGroup
        "Test deleteElem"
        [ testCase "can delete elem with exists key" $ do
            let hM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            let deletedKey = 'a'
            let deletedHM = deleteElem hM deletedKey
            getSizeMap deletedHM < getSizeMap hM @? "Not change size"
            getElem deletedHM deletedKey @?= Nothing
        , testCase "can delete correct elem then two key has same hash" $ do
            let sameHashHM = createHashMap 0.8 [('a', '1'), ('f', '2')]
            let deletedKey = 'a'
            getElem (deleteElem sameHashHM deletedKey) deletedKey @?= Nothing
        , testCase "map not changed when delete elem with not exists key" $ do
            let hM = createHashMap 0.8 [('a', '1'), ('f', '2')]
            let deletedHM = deleteElem hM 'b'
            deletedHM @?= hM
        , testCase "map not changed when delete elem with not exists key but exists key hash" $ do
            let hM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            let deletedHM = deleteElem hM 'f'
            deletedHM @?= hM
        ]
testAdd =
    testGroup
        "Test addElem"
        [ testCase "can add elem with new key" $ do
            let hM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            let addedKey = 'c'
            let addedValue = '3'
            let addedHM = addElem hM addedKey addedValue
            getSizeMap addedHM > getSizeMap hM @? "Not change size"
            getElem addedHM addedKey @?= Just addedValue
            getCurrentFilled addedHM > getCurrentFilled hM @? "Not change filled"
        , testCase "can add elem with new key with same hash" $ do
            let hM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            let addedKey = 'f'
            let addedValue = '3'
            let addedHM = addElem hM addedKey addedValue
            getSizeMap addedHM > getSizeMap hM @? "Not change size"
            getElem addedHM addedKey @?= Just addedValue
            getCurrentFilled addedHM @?= getCurrentFilled hM
        , testCase "replace value when add elem with exist key" $ do
            let hM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            let addedKey = 'c'
            let addedValue = '3'
            let addedHM = addElem hM addedKey addedValue
            getElem addedHM addedKey @?= Just addedValue
        , testCase "change bucket count then filled overflow" $ do
            let hM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            let addedHM = addElem hM 'c' '3'
            let doubleAddedHM = addElem addedHM 'd' '4'
            getCurrentFilled addedHM > getCurrentFilled doubleAddedHM @? "Not resize hashmap"
        ]
testFilter =
    testGroup
        "Test filterHashMap"
        [ testCase "filter hashMap" $ do
            let firstHM = createHashMap 0.8 [('a', '1'), ('b', '2'), ('c', '3')]
            let secondHM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            let filteredHM = filterHashMap (\a -> fst a < 'c') firstHM
            secondHM @?= filteredHM
        ]
testMap =
    testGroup
        "Test mapHashMap"
        [ testCase "map key to string and double, value to int" $ do
            let firstHM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            let secondHM = createHashMap 0.8 [("aa", 1 :: Int), ("bb", 2 :: Int)]
            let mappedHM = mapHashMap (\(a, b) -> ([a, a], ord b - ord '0')) firstHM
            secondHM @?= mappedHM
        ]
testFold =
    testGroup
        "Test foldHashMap"
        [ testCase "left fold iterate from min hash to max" $ do
            let hM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            foldlHashMap (\acc (a, b) -> a : b : acc) "" hM @?= "b2a1"
        , testCase "right fold iterate from max hash to min" $ do
            let hM = createHashMap 0.8 [('a', '1'), ('b', '2')]
            foldrHashMap (\(a, b) acc -> a : b : acc) "" hM @?= "a1b2"
        ]
```

**Property-based тестирование**

Для удобства генерации данных реализовала класс типов Arbitrary из библиотеки QuickCheck для обертки тестируемого типа, чтобы не добавлять в зависимости основного модуля тестовую библиотеку и чтобы не отключать предупреждение линтера.

```haskell
newtype TestSepChainHashMap a b = TestSCHM {unTestSCHM :: SepChainHashMap a b} deriving (Semigroup, Monoid, Eq, Show)

instance (Hashable a, Arbitrary a, Arbitrary b) => Arbitrary (TestSepChainHashMap a b) where
    arbitrary = TestSCHM <$> (createHashMap <$> filled <*> listOf arbitrary)
      where
        filled = choose (0.1, 0.9)
```

Для тестирования я выделила следующие свойства:
- ассоциативность бинарной операции моноида (полугруппы);
- нулевой элемент моноида - бинарная операция с ним для любого другого элемента возращает этот самый элемент;
- коммутативность бинарной операции с нулевым элементом;
- текущая заполненность хэшмапы не превышает максимальной заполненности.

```haskell
propertyBasedTests :: TestTree
propertyBasedTests =
    testGroup
        "Property-based tests"
        [ testProperty "binary operation is associative" (binOpAssociative :: TestSepChainHashMap Char Char -> TestSepChainHashMap Char Char -> TestSepChainHashMap Char Char -> Bool)
        , testProperty "binary operation with mempty not change elem" (binOpWithEmptyNotChange :: TestSepChainHashMap Char Char -> Bool)
        , testProperty "binary operation with mempty is commutative" (binWithEmptyCommutative :: TestSepChainHashMap Char Char -> Bool)
        , testProperty "currentFilled always less than inited" (currentFilledLessInited :: TestSepChainHashMap Char Char -> NonEmptyList (Char, Char) -> Bool)
        ]

binOpAssociative :: (Hashable a, Eq b) => TestSepChainHashMap a b -> TestSepChainHashMap a b -> TestSepChainHashMap a b -> Bool
binOpAssociative = associative (<>)

binOpWithEmptyNotChange :: (Hashable a, Eq b) => TestSepChainHashMap a b -> Bool
binOpWithEmptyNotChange x = x == (x <> mempty)

binWithEmptyCommutative :: (Hashable a, Eq b) => TestSepChainHashMap a b -> Bool
binWithEmptyCommutative = commutative (<>) mempty

currentFilledLessInited :: (Hashable a, Eq b) => TestSepChainHashMap a b -> NonEmptyList (a, b) -> Bool
currentFilledLessInited hM elems =
    snd $
        foldl
            ( \acc e ->
                let newMap = uncurry (addElem (fst acc)) e
                    checkFilled = getCurrentFilled newMap < filledHashMap newMap
                 in (newMap, snd acc && checkFilled)
            )
            (unTestSCHM hM, True)
            (getNonEmpty elems)
```
