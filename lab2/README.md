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

Этот тип реализует следующие классы: Eq, Show (для удобства тестирования), Semigroup, Monoid (для выполнения условия задания)

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

Помимо необходимых функций я также сделала функцию создания хэшмапы - её и использую выше в бинарной операции, так как на мой взгляд это быстрее, чем по очереди добавлять элементы из одной мапы в другую (при дубликатах ключей значение сохранится из первого аргумента).

Сама функция создания, во-первых, валидирует значение заполненности и высчитывает, сколько должно быть бакетов, чтобы заполненность была в два раза меньше заданной при указанном начальном количестве элементов. Во-вторых, убирает дубликаты ключей (здесь не определено, какое значение останется). В третьих, рассчитывает для ключей хэши и группирует пары по этому признаку в списки. Полученные цепочки сортируются по возрастанию хэшей, и для недостающих значений хэша между ними вставляются пустые списки.

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
    groupAndSortByHash len xs = DL.sortBy (compareRes (shortHashElem len . head)) (DL.groupBy (equalRes (shortHashElem len)) xs)
```

