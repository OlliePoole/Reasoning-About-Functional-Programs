Oliver Poole (12022846)


> import Data.Char

> type BarCode = Int
> type TillType = [BarCode]

The barcodes to search

> barcodes :: TillType
> barcodes = [1234, 4719, 3814, 1112, 1113, 1234]

Types

> type Name = String
> type Price = Int
> type Database = [(BarCode, Name, Price)]

The database of items to lookUp

> codeIndex :: Database
> codeIndex = [ (4719, "Fish Fingers" , 199),
>               (5643, "Nappies" , 1010),
>               (3814, "Orange Jelly", 89),
>               (1111, "Hula Hoops", 75),
>               (1112, "Hula Hoops (Giant)", 200),
>               (1234, "Dry Sherry, 1lt", 909)]


> type BillItem = (Name, Price)
> type BillType = [BillItem]


Task One - Format the int into a string

>  -- |The 'formatPence' function converts pence to pounds
>  -- It takes one argument, the pence to convert, of type 'Int'.
> formatPence :: Int -> String
> formatPence x
>   | x >= 100  = show(x `div` 100) ++ ['.'] ++ show(x `mod` 100)
>   | x < 10    = ['0'] ++ ['.'] ++ ['0'] ++ show(x `mod` 100)
>   | otherwise = ['0'] ++ ['.'] ++ show(x `mod` 100)
> -- TODO: Find out how to do this recusrsively


Task Two

Introduce a constant for the line length

>  -- |The 'lineLength' function returns the length of the line
> lineLength :: Int
> lineLength = 30


Define the `formatLine` function

>  -- |The 'formatLine' function formats a line in the bill to the correct format
>  -- It takes one argument, the BillItem to display, of type 'BillItem'.
> formatLine :: BillItem -> String
> formatLine (name, pence) = name ++ rep (lineLength - (length(name) + length(formatPence pence))) "." ++ formatPence pence ++ "\n"




>  -- |The 'rep' function repeats a character x times
>  -- It takes two arguments, the number of repetitions, and the char to repeat.
> rep :: Int -> String -> String
> rep v c = concat [c | r <- [1..v]]


>  -- |The 'formatLines' function formats all lines in the bill
>  -- It takes one argument, the BillType to display
> formatLines :: BillType -> String
> formatLines bt = concat (map (\item -> formatLine item) bt)


>  -- |The 'makeTotal' function calculates the total of a bill
>  -- It takes one argument, the BillType to containing the bill items
> makeTotal :: BillType -> Int
> makeTotal []           = 0
> makeTotal ((x, y): xs) = y + makeTotal xs


>  -- |The 'formatTotal' function formats the total of the bill
>  -- It takes one argument, the total value of the bill
> formatTotal :: Int -> String
> formatTotal total = "\n" ++ formatLine ("Total", total)


>  -- |The 'formatBill' function formats the entire bill
>  -- It takes one argument, the BillType to display
> formatBill :: BillType -> String
> formatBill [] = ""
> formatBill bt = formatLines bt ++ formatTotal (makeTotal bt)


> testBill :: BillType
> testBill = [ ("Dry Sherry, 1lt",540), ("Fish Fingers",121), ("Orange Jelly",56), ("Hula Hoops (Giant)",133), ("Unknown Item", 0), ("Dry Sherry, 1lt",540) ]


> -- The Database
>
>  -- |The 'look' function finds an item in a user-defined database
>  -- It takes two arguments, the database and the barcode to find.
>  -- If the barcode cannot be found, ("Unknown Item", 0) will be returned
> look :: Database -> BarCode -> BillItem
> look db bc
>   | length items > 0 = removeFirst(head items)
>   | otherwise = ("Unknown Item", 0)
>   where items = filter (\(code, _, _) -> code == bc) db


> removeFirst :: (BarCode, Name, Price) -> BillItem
> removeFirst (_, name, price) = (name, price)


-- Tests
-- `look codeIndex 1234` returns ("Dry Sherry, 1lt",909)
-- `look codeIndex 1` returns ("Unknown Item",0)


>  -- |The 'lookUp' function finds an item in the set database
>  -- It takes one arguments, the barcode to find.
> lookUp :: BarCode -> BillItem
> lookUp code = look codeIndex code


> unsortedMakeBill :: TillType -> BillType
> unsortedMakeBill tt = map (\code -> (lookUp code)) tt


> printBill :: TillType -> String
> printBill tt = formatBill (makeBill tt)


***** UPDATED makeBill to sort first

> makeBill :: TillType -> BillType
> makeBill tt = map (\code -> (lookUp code)) (qSort tt)


>  -- Quicksort algorithm
> qSort :: Ord a => [a] -> [a]
> qSort []     = []
> qSort (p:xs) = (qSort lower) ++ [p] ++ (qSort upper)
>    where
>        lower  = filter (< p) xs
>        upper  = filter (>= p) xs




                        PART TWO
Types

> type Symbol = Char
> type Code = [Symbol]
> type Classfn = String

> data DeweyTree = Node Symbol Classfn DeweyForest
>   deriving Show

> type DeweyForest = [DeweyTree]

    TASK 11

>  -- |The 'symbol' function returns the Symbol of a DeweyTree
>  -- It takes one argument, the tree
> symbol :: DeweyTree -> Symbol
> symbol (Node s _ _) = s

>  -- |The 'classfn' function returns the Classfn of a DeweyTree
>  -- It takes one argument, the tree
> classfn :: DeweyTree -> Classfn
> classfn (Node _ c _) = c

>  -- |The 'children' function returns the DeweyForest of a DeweyTree
>  -- It takes one argument, the tree
> children :: DeweyTree -> DeweyForest
> children (Node _ _ f) = f

    TASK 12

> insertClassfnT :: (Code,Classfn) -> DeweyTree -> DeweyTree
> insertClassfnT (c:cs,classfn) (Node s d f)
>   | s == c = Node s d (insertClassfn (cs, classfn) f)




> insertClassfn :: (Code,Classfn) -> DeweyForest -> DeweyForest
> insertClassfn ([c],classfn) []     = [Node c classfn []]
> insertClassfn ([c],classfn) (t:ts)
>   | c == (symbol t) = error "The classification being entered is already in the tree"
>   | otherwise       = t: insertClassfn ([c], classfn) ts
>
> insertClassfn (c:cs,classfn) []     = error "parent of new code not present"
> insertClassfn (c:cs,classfn) (t:ts)
>   | symbol t == c = insertClassfnT (c:cs, classfn) t : ts
>   | otherwise     = t: insertClassfn (c:cs, classfn) ts

    TEST DATA

> a = ("02", "Biology")
> f = [(Node '0' "Science" [ (Node '0' "Physics" []), (Node '1' "Computing" [])])]

> buildHeirarchy :: [(Code, Classfn)] -> DeweyForest
> buildHeirarchy c = foldr (insertClassfn) [] (reverse c)

> info = [ ("0","Science"), ("1","Arts"), ("2","Religion"),
>          ("01","Physics"), ("02","Computing"), ("03", "Mathematics"),
>          ("020","Computer hardware"), ("021","Computer languages"), ("022","Artificial Intelligence"),
>          ("0210","Imperative languages"), ("0211","Functional languages"),
>          ("02110","Haskell"),("02111","ML"),("02112","Scala") ]




> lookupClassfn :: Code -> DeweyForest -> Classfn
> lookupClassfn [] [] = error "That code doesn't exist"
> lookupClassfn [c] (t:ts) = classfn t
> lookupClassfn (c:cs) (t:ts)
>   | (symbol t) == c = lookupClassfn cs (children t)
>   | otherwise       = lookupClassfn (c:cs) ts


    Task 15

> showForest :: DeweyForest -> String
> showForest [] = ""
> showForest (t:ts)
>   | length (children t) > 0 = (showTree t) ++ showForest (children t) ++ showForest (ts)
>   | length (ts) > 0         = (showTree t) ++ showForest (ts)
>   | otherwise               = (showTree t)

> showTree :: DeweyTree -> String
> showTree t = [(symbol t)] ++ ": " ++ (classfn t) ++ "\n"

    Task 15 Tests
