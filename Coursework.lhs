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
>   | (x `mod` 100) < 10 = pound ++ ".0" ++ pence
>   | otherwise = pound ++ "." ++ pence
>    where pound = show (x `div` 100)
>          pence = show (x `mod` 100)


****** Task One Tests ******

*Main> formatPence 2989
"29.89"

*Main> formatPence 0010
"0.10"



Task Two

Introduce a constant for the line length

>  -- |The 'lineLength' function returns the length of the line
> lineLength :: Int
> lineLength = 30


****** Task Two Tests ******

*Main> lineLength
30



Define the `formatLine` function

>  -- |The 'formatLine' function formats a line in the bill to the correct format
>  -- It takes one argument, the BillItem to display, of type 'BillItem'.
> formatLine :: BillItem -> String
> formatLine (name, pence) = name ++ rep (lineLength - (length(name) + length(formatPence pence))) "." ++ formatPence pence ++ "\n"

****** FormatLine Tests ******

*Main> formatLine ("Food Item One", 1000)
"Food Item One............10.00\n"

*Main> formatLine ("Another Cool Food Item", 0999)
"Another Cool Food Item....9.99\n"



>  -- |The 'rep' function repeats a character x times
>  -- It takes two arguments, the number of repetitions, and the char to repeat.
> rep :: Int -> String -> String
> rep v c = concat [c | r <- [1..v]]

****** Rep Tests ******

*Main> rep 10 "o"
"oooooooooo"

*Main> rep 20 "o"
"oooooooooooooooooooo"



Task Three - Format Lines Function

>  -- |The 'formatLines' function formats all lines in the bill
>  -- It takes one argument, the BillType to display
> formatLines :: BillType -> String
> formatLines bt = concat (map (\item -> formatLine item) bt)

****** Format Lines Tests ******

*Main> formatLines testBill
"Dry Sherry, 1lt...........5.40\nFish Fingers..............1.21\nOrange Jelly..............0.56\nHula Hoops (Giant)........1.33\nUnknown Item..............0.00\nDry Sherry, 1lt...........5.40\n"



Task Four - Make Total Function

>  -- |The 'makeTotal' function calculates the total of a bill
>  -- It takes one argument, the BillType to containing the bill items
> makeTotal :: BillType -> Int
> makeTotal []           = 0
> makeTotal ((x, y): xs) = y + makeTotal xs

****** Make Totla Tests ******

*Main> makeTotal testBill
1390

*Main> makeTotal [("Item One", 1000), ("Item Two", 2000), ("Item Three", 3000)]
6000



Task Five - Format Total

>  -- |The 'formatTotal' function formats the total of the bill
>  -- It takes one argument, the total value of the bill
> formatTotal :: Int -> String
> formatTotal total = "\n" ++ formatLine ("Total", total)

****** Format Total Tests ******

*Main> formatTotal 6000
"\nTotal....................60.00\n"

*Main> formatTotal 0101
"\nTotal.....................1.01\n"



Task Six - Format Bill

>  -- |The 'formatBill' function formats the entire bill
>  -- It takes one argument, the BillType to display
> formatBill :: BillType -> String
> formatBill [] = ""
> formatBill bt = formatLines bt ++ formatTotal (makeTotal bt)

****** Format Bill Tests ******

*Main> formatBill testBill
"Dry Sherry, 1lt...........5.40\nFish Fingers..............1.21\nOrange Jelly..............0.56\nHula Hoops (Giant)........1.33\nUnknown Item..............0.00\nDry Sherry, 1lt...........5.40\n\nTotal....................13.90\n"

*Main> formatBill [("Item One", 1000), ("Item Two", 2000), ("Item Three", 3000)]
"Item One.................10.00\nItem Two.................20.00\nItem Three...............30.00\n\nTotal....................60.00\n"



>  -- A bill for testing
> testBill :: BillType
> testBill = [ ("Dry Sherry, 1lt",540), ("Fish Fingers",121), ("Orange Jelly",56), ("Hula Hoops (Giant)",133), ("Unknown Item", 0), ("Dry Sherry, 1lt",540) ]


Task Seven - Look function

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

> -- Removes the first item from a three part tuple
> removeFirst :: (BarCode, Name, Price) -> BillItem
> removeFirst (_, name, price) = (name, price)


***** Look Tests *****

*Main> look codeIndex 1234
("Dry Sherry, 1lt",909)

*Main> look codeIndex 1
("Unknown Item",0)


Task Eight - Lookup Function

>  -- |The 'lookUp' function finds an item in the set database
>  -- It takes one arguments, the barcode to find.
> lookUp :: BarCode -> BillItem
> lookUp code = look codeIndex code


***** LookUp Tests *****

*Main> lookUp 123
("Unknown Item",0)

*Main> lookUp 1234
("Dry Sherry, 1lt",909)

*Main> lookUp 1111
("Hula Hoops",75)


Task Nine - An unsorted version of the Make Bill Function

> unsortedMakeBill :: TillType -> BillType
> unsortedMakeBill tt = map (lookUp) tt

***** UnsortedMakeBill Tests *****

*Main> unsortedMakeBill barcodes
[("Dry Sherry, 1lt",909),("Fish Fingers",199),("Orange Jelly",89),("Hula Hoops (Giant)",200),("Unknown Item",0),("Dry Sherry, 1lt",909)]

*Main> unsortedMakeBill [1111, 1234, 8765]
[("Hula Hoops",75),("Dry Sherry, 1lt",909),("Unknown Item",0)]



> printBill :: TillType -> String
> printBill tt = formatBill (makeBill tt)

***** PrintBill Tests *****

*Main> printBill barcodes
"Hula Hoops (Giant)........2.00\nUnknown Item..............0.00\nDry Sherry, 1lt...........9.09\nDry Sherry, 1lt...........9.09\nOrange Jelly..............0.89\nFish Fingers..............1.99\n\nTotal....................23.06\n"

*Main> putStr (printBill barcodes)
Hula Hoops (Giant)........2.00
Unknown Item..............0.00
Dry Sherry, 1lt...........9.09
Dry Sherry, 1lt...........9.09
Orange Jelly..............0.89
Fish Fingers..............1.99

Total....................23.06



    Task 10 - Sorted Make Bill Function

> makeBill :: TillType -> BillType
> makeBill tt = map (\code -> (lookUp code)) (qSort tt)

***** Sorted make bill Tests *****

*Main> makeBill barcodes
[("Hula Hoops (Giant)",200),("Unknown Item",0),("Dry Sherry, 1lt",909),("Dry Sherry, 1lt",909),("Orange Jelly",89),("Fish Fingers",199)]

*Main> makeBill [1234, 1111, 3412]
[("Hula Hoops",75),("Dry Sherry, 1lt",909),("Unknown Item",0)]
*Main>



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


***** Symbol, Classfn & Children Tests *****

*Main> symbol (Node '1' "Item One" [])
'1'

*Main> symbol (Node '0' "Item One" [])
'0'

*Main> classfn (Node '0' "Item One" [])
"Item One"

*Main> classfn (Node '0' "Item Ten" [])
"Item Ten"

*Main> children (Node '0' "Item Ten" [])
[]

*Main> children (Node '0' "Item Ten" [Node '0' "SubItem 1" []])
[Node '0' "SubItem 1" []]



TASK 12 - Inserting into a tree

>  -- |The 'insertClassfnT' function inserts a DeweyTree into another DeweyTree
>  -- It takes one argument, the tree to insert
> insertClassfnT :: (Code,Classfn) -> DeweyTree -> DeweyTree
> insertClassfnT (c:cs,classfn) (Node s d f)
>   | s == c = Node s d (insertClassfn (cs, classfn) f)

>  -- |The 'insertClassfn' function inserts a DeweyForest into another DeweyForest
>  -- It takes one argument, the DeweyForest to insert
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

> a = ("0", "Biology")
> f = (Node '1' "Science" [ (Node '2' "Physics" []), (Node '3' "Computing" [])])

***** Insertion Tests *****

*Main> insertClassfn a []
[Node '0' "Biology" []]


Task 13 - Build the Hierarchy

>  -- |The 'buildHierarchy' function inserts a builds a DeweyTree
>  -- It takes one argument, an array of items to insert
> buildHierarchy :: [(Code, Classfn)] -> DeweyForest
> buildHierarchy c = foldr (insertClassfn) [] (reverse c)

> -- Test data to insert into the tree
> info = [ ("0","Science"), ("1","Arts"), ("2","Religion"),
>          ("01","Physics"), ("02","Computing"), ("03", "Mathematics"),
>          ("020","Computer hardware"), ("021","Computer languages"), ("022","Artificial Intelligence"),
>          ("0210","Imperative languages"), ("0211","Functional languages"),
>          ("02110","Haskell"),("02111","ML"),("02112","Scala") ]


**** Build Hierarchy Tests *****

*Main> buildHierarchy info
[Node '0' "Science" [Node '1' "Physics" [],Node '2' "Computing" [Node '0' "Computer hardware" [],Node '1' "Computer languages" [Node '0' "Imperative languages" [],Node '1' "Functional languages" [Node '0' "Haskell" [],Node '1' "ML" [],Node '2' "Scala" []]],Node '2' "Artificial Intelligence" []],Node '3' "Mathematics" []],Node '1' "Arts" [],Node '2' "Religion" []]

*Main> buildHierarchy [("0", "Head"), ("01", "Left-Sub"), ("02","RightSub")]
[Node '0' "Head" [Node '1' "Left-Sub" [],Node '2' "RightSub" []]]


>  -- |The 'lookupClassfn' function finds a classification based on a
>  -- It takes one argument, an array of items to insert
> lookupClassfn :: Code -> DeweyForest -> Classfn
> lookupClassfn [] [] = error "That code doesn't exist"
> lookupClassfn [c] (t:ts) = classfn t
> lookupClassfn (c:cs) (t:ts)
>   | (symbol t) == c = lookupClassfn cs (children t)
>   | otherwise       = lookupClassfn (c:cs) ts

***** LookupClassfn Tests *****

*Main> lookupClassfn "01" (buildHierarchy info)
"Physics"

*Main> lookupClassfn "0" (buildHierarchy info)
"Science"




Task 15 - Showing the forest
    Wasn't able to get this to work and ran out of time that could be sensibly spent on the coursework.

>  -- |The 'showForest' function prints a formatted forest
>  -- It takes one argument, the forest to display
> showForest :: DeweyForest -> String
> showForest [] = ""
> showForest (t:ts)
>   | length (children t) > 0 = (showTree t) ++ [symbol t] ++ showForest (children t) ++ [symbol t] ++ showForest ts
>   | length (ts) > 0         = (showTree t) ++ showForest ts
>   | otherwise               = (showTree t)

>  -- |The 'showTree' function formats a tree for printing
>  -- It takes one argument, the tree to display
> showTree :: DeweyTree -> String
> showTree t = [(symbol t)] ++ ": " ++ (classfn t) ++ "\n"

***** Show Forest Tests *****

*Main> putStr (showForest (buildHierarchy info))
0: Science
01: Physics
2: Computing
20: Computer hardware
1: Computer languages
10: Imperative languages
1: Functional languages
10: Haskell
1: ML
2: Scala
112: Artificial Intelligence
23: Mathematics
01: Arts
2: Religion
