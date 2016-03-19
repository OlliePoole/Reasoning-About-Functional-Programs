> -- Oliver Poole (12022846)
> import Data.Char
>
> type BarCode = Int
> type TillType = [BarCode]
>
> barcodes :: TillType
> barcodes = [1234, 4719, 3814, 1112, 1113, 1234]
>
> type Name = String
> type Price = Int
> type Database = [(BarCode, Name, Price)]
>
> codeIndex :: Database
> codeIndex = [ (4719, "Fish Fingers" , 199),
>               (5643, "Nappies" , 1010),
>               (3814, "Orange Jelly", 89),
>               (1111, "Hula Hoops", 75),
>               (1112, "Hula Hoops (Giant)", 200),
>               (1234, "Dry Sherry, 1lt", 909)]
>
> type BillItem = (Name, Price)
> type BillType = [BillItem]
>
>  -- |The 'formatPence' function converts pence to pounds
>  -- It takes one argument, the pence to convert, of type 'Int'.
> formatPence :: Int -> String
> formatPence x
>   | x >= 100  = show(x `div` 100) ++ ['.'] ++ show(x `mod` 100)
>   | x < 10    = ['0'] ++ ['.'] ++ ['0'] ++ show(x `mod` 100)
>   | otherwise = ['0'] ++ ['.'] ++ show(x `mod` 100)
> -- TODO: Find out how to do this recusrsively
>
>  -- |The 'lineLength' function returns the length of the line
> lineLength :: Int
> lineLength = 30
>
>  -- |The 'formatLine' function formats a line in the bill to the correct format
>  -- It takes one argument, the BillItem to display, of type 'BillItem'.
> formatLine :: BillItem -> String
> formatLine (name, pence) = name ++ rep (lineLength - (length(name) + length(formatPence pence))) "." ++ formatPence pence ++ "\n"
>
>  -- |The 'rep' function repeats a character x times
>  -- It takes two arguments, the number of repetitions, and the char to repeat.
> rep :: Int -> String -> String
> rep v c = concat [c | r <- [1..v]]
>
>  -- |The 'formatLines' function formats all lines in the bill
>  -- It takes one argument, the BillType to display
> formatLines :: BillType -> String
> formatLines []      = ""
> formatLines (x: xs) = formatLine x ++ formatLines xs
>
>  -- |The 'makeTotal' function calculates the total of a bill
>  -- It takes one argument, the BillType to containing the bill items
> makeTotal :: BillType -> Int
> makeTotal []           = 0
> makeTotal ((x, y): xs) = y + makeTotal xs
>
>  -- |The 'formatTotal' function formats the total of the bill
>  -- It takes one argument, the total value of the bill
> formatTotal :: Int -> String
> formatTotal total = "\n" ++ formatLine ("Total", total)
>
>  -- |The 'formatBill' function formats the entire bill
>  -- It takes one argument, the BillType to display
> formatBill :: BillType -> String
> formatBill [] = ""
> formatBill bt = formatLines bt ++ formatTotal (makeTotal bt)
>
> testBill :: BillType
> testBill = [ ("Dry Sherry, 1lt",540), ("Fish Fingers",121), ("Orange Jelly",56), ("Hula Hoops (Giant)",133), ("Unknown Item", 0), ("Dry Sherry, 1lt",540) ]
>
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
>
> removeFirst :: (BarCode, Name, Price) -> BillItem
> removeFirst (_, name, price) = (name, price)
>
> -- Tests
> -- `look codeIndex 1234` returns ("Dry Sherry, 1lt",909)
> -- `look codeIndex 1` returns ("Unknown Item",0)
>
>
>  -- |The 'lookUp' function finds an item in the set database
>  -- It takes one arguments, the barcode to find.
> lookUp :: BarCode -> BillItem
> lookUp code = look codeIndex code
>
> makeBill :: TillType -> BillType
> makeBill tt = map (\code -> (lookUp code)) tt
>
> printBill :: TillType -> String
> printBill tt = formatBill (makeBill tt)
>
>
