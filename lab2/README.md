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
createHashMap :: (Hashable a, Ord a) => Double -> [(a, b)] -> SepChainHashMap a b
addElem :: (Hashable a, Ord a, Eq b) => SepChainHashMap a b -> a -> b -> SepChainHashMap a b
deleteElem :: (Hashable a) => SepChainHashMap a b -> a -> SepChainHashMap a b
getElem :: (Hashable a) => SepChainHashMap a b -> a -> Maybe b
filterHashMap :: ((a, b) -> Bool) -> SepChainHashMap a b -> SepChainHashMap a b
mapHashMap :: (Hashable c, Ord c) => (Elem a b -> Elem c d) -> SepChainHashMap a b -> SepChainHashMap c d
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

checkFillAndExpand :: (Hashable a, Ord a, Eq b) => SepChainHashMap a b -> SepChainHashMap a b
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

Есть набор тестов на каждую функцию - при каждой следующей я считаю что предыдущая работает корректно и её можно использовать для тестирования. Плюс две заранее созданные хэшмапы, где в одной - два значения лежат в разных бакетах, а в другой - в одном.

```haskell
hMDiffHash = createHashMap 0.8 [('a', '1'), ('b', '2')]
hMSameHash = createHashMap 0.8 [('a', '1'), ('f', '2')]

testCreate =
    TestList
        [ "current filled less then inited" ~: True ~=? 0.8 >= getCurrentFilled (createHashMap 0.8 [('b', "a"), ('a', "a")])
        , "remove dublicates key" ~: 1 ~=? getSize (createHashMap 0.8 [('a', '1'), ('a', '2')])
        , "don't remove dublicates hash" ~: 2 ~=? getSize (createHashMap 0.8 [('a', '1'), ('f', '2')])
        , "two elem with same hash key not change fill" ~: 2 * getCurrentFilled (createHashMap 0.8 [('a', '1'), ('f', '2')]) ~=? getCurrentFilled (createHashMap 0.8 [('a', '1'), ('b', '2')])
        ]
testGet =
    TestList
        [ "can get elem with exists key" ~: Just '1' ~=? getElem hMDiffHash 'a'
        , "can get correct elem then two key has same hash" ~: True ~=? (getElem hMSameHash 'a' == Just '1') && (getElem hMSameHash 'f' == Just '2')
        , "can't get elem with not exists key" ~: Nothing ~=? getElem hMDiffHash 'c'
        , "can't get elem with not exists key but exists key hash" ~: Nothing ~=? getElem hMDiffHash 'f'
        ]
testDelete =
    TestList
        [ "can delete elem with exists key" ~: 1 ~=? getSize (deleteElem hMDiffHash 'a')
        , "can't get elem after delete it" ~: Nothing ~=? getElem (deleteElem hMDiffHash 'a') 'a'
        , "can delete correct elem then two key has same hash" ~: Nothing ~=? getElem (deleteElem hMSameHash 'a') 'a'
        , "map not changed when delete elem with not exists key" ~: hMSameHash ~=? deleteElem hMSameHash 'b'
        , "map not changed when delete elem with not exists key but exists key hash" ~: hMDiffHash ~=? deleteElem hMDiffHash 'f'
        ]
testAdd =
    TestList
        [ "can add elem with new key" ~: getSize hMDiffHash + 1 ~=? getSize (addElem hMDiffHash 'c' '3')
        , "can get added elem" ~: Just '3' ~=? getElem (addElem hMDiffHash 'c' '3') 'c'
        , "replace value when add elem with exist key" ~: Just '3' ~=? getElem (addElem hMDiffHash 'b' '3') 'b'
        , "change filled then add elem with diff hash key" ~: 2 * getCurrentFilled hMSameHash ~=? getCurrentFilled (addElem hMSameHash 'c' '3')
        , "not change filled then add elem with same hash key" ~: getCurrentFilled hMDiffHash ~=? getCurrentFilled (addElem hMDiffHash 'f' '3')
        , "change bucket count then filled overflow" ~: True ~=? (getCurrentFilled (addElem hMDiffHash 'c' '3') > getCurrentFilled (addElem (addElem hMDiffHash 'c' '3') 'd' '4'))
        ]
testFilter =
    TestList
        ["filter hashMap" ~: hMDiffHash ~=? filterHashMap (\a -> fst a == 'a' || snd a == '2') (addElem (addElem hMDiffHash 'f' '3') 'c' '4')]
testMap =
    TestList
        ["map key to string and double, value to int" ~: createHashMap 0.8 [("aa", 1 :: Int), ("bb", 2 :: Int)] ~=? mapHashMap (\(a, b) -> ([a, a], ord b - ord '0')) hMDiffHash]
testFold =
    TestList
        [ "left fold" ~: "b2a1" ~=? foldlHashMap (\acc (a, b) -> a : b : acc) "" hMDiffHash
        , "right fold" ~: "a1b2" ~=? foldrHashMap (\(a, b) acc -> a : b : acc) "" hMDiffHash
        ]
```

**Property-based тестирование**

Для тестирования я выделила следующие свойства:
- ассоциативность бинарной операции моноида (полугруппы);
- нулевой элемент моноида - бинарная операция с ним для любого другого элемента возращает этот самый элемент;
- коммутативность бинарной операции с нулевым элементом;
- текущая заполненность хэшмапы не превышает максимальной заполненности.

```haskell
fistHM = createHashMap 0.8 [('a', 1), ('b', 1)]
secondHM = createHashMap 0.8 [('c', 2), ('d', 2)]
thirdHM = createHashMap 0.6 [('e', 3), ('f', 3)]

propertyBasedTests =
    TestList
        [ "binary operation must be associative" ~: ((fistHM <> secondHM) <> thirdHM) ~=? (fistHM <> (secondHM <> thirdHM))
        , "binary operation with empty must not change map" ~: fistHM ~=? fistHM <> mempty
        , "bianry operation with empty must be commutative" ~: fistHM <> mempty ~=? mempty <> fistHM
        , "current filled must be less then inited" ~: do
            result <-
                foldM
                    ( \acc e -> do
                        let newMap = addElem acc e (1 :: Int)
                        assertEqual ("add elem with key" ++ show e) True (getCurrentFilled newMap < filledHashMap newMap)
                        return newMap
                    )
                    (createHashMap 0.8 [('A', 1 :: Int)])
                    ['B' .. 'z']
            assertEqual "result size" 58 (getSize result)
        ]
```
