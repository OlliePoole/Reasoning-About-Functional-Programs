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
>
