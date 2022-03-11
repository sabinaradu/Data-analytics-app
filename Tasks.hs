{-
	PP Project 2021
-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Tasks where
import Text.Printf
import Dataset
import Data.List

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]

{-
	TASK SET 1
-}

-- Task 1
compute_exam_grades :: Table -> Table
compute_exam_grades table = ["Nume", "Punctaj Exam"] : (create_table table)

grades_string :: Row -> [String]
grades_string r = tail(init r)

nullToZero :: Row -> [String]
nullToZero r = map change (grades_string r)
    where
        change "" = "0"
        change c = c

grades_float :: Row -> [Float]
grades_float r = map(read :: String -> Float) (nullToZero r)

calc_grade :: Row -> Float
calc_grade r = (sum(grades_float r)) / 4

exam_grade :: Row -> Float
exam_grade r = calc_grade r + read (last r) :: Float

create_line :: Row -> Row
create_line r = (head r) : [printf "%.2f" (exam_grade r)]

create_table :: Table -> Table
create_table table = map create_line (tail table)

-- Task 2
-- Number of students who have passed the exam:

get_passed_students_num :: Table -> Int
get_passed_students_num table = foldr (\a aux -> if(check a) /= 1 then aux  else aux +1) 0 (create_table table)

check :: Row -> Int
check r = 
    if(last r) >= "2.5" then 1 
    else 0

-- Percentage of students who have passed the exam:
get_passed_students_percentage :: Table -> Float
get_passed_students_percentage table = (fromIntegral (get_passed_students_num table)) / (fromIntegral(foldr (\a aux -> aux + 1) 0 (create_table table)))

-- Average exam grade
get_exam_avg :: Table -> Float
get_exam_avg table = (get_grades_sum table) / (fromIntegral(length table) - 1)

get_grades :: Table -> Table
get_grades table = (tail(compute_exam_grades table))

get_list :: Table -> Table
get_list table = transpose(get_grades table)

get_grades_sum :: Table -> Float
get_grades_sum table = sum(map (read :: String -> Float) (last(get_list table)))


-- Number of students who gained at least 1.5p from homework:
get_passed_hw_num :: Table -> Int
get_passed_hw_num table = foldr (\a aux -> if(passed_hw a) /= 1 then aux  else aux +1) 0 (stringToFloat table)

get_hw :: Table -> Table
get_hw table = tail(map (take 3) (map (drop 2) table))

null_grade :: Table -> Table
null_grade table = map(map change) (get_hw table)
    where
        change "" = "0"
        change c = c

stringToFloat :: Table -> [[Float]]
stringToFloat table = map(map (read :: String -> Float)) (null_grade table)

passed_hw :: [Float] -> Int
passed_hw row =
    if (sum row) >= 1.5 then 1
    else 0

-- Task 3
get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs table = ["Q1","Q2","Q3","Q4","Q5","Q6"] : [map (printf "%.2f") (get_qavg table)]

create_qtable :: Table -> [[Float]]
create_qtable table = map grades_float (tail table)

get_qsum :: Table -> [Float]
get_qsum table = map sum (transpose(create_qtable table))

get_qavg :: Table -> [Float]
get_qavg table = map (/(fromIntegral(length table) - 1)) (get_qsum table) 

-- Task 4
get_exam_summary :: Table -> Table
get_exam_summary table = ["Q","0","1","2"] : (get_exam_summary_aux (grades_float4 table) grades_string4)

cmp4 :: [Float] -> Float -> Integer -> Integer
cmp4 [] _ n = n
cmp4 (h:t) x n
   |x == h = cmp4 t x (n+1)
   |otherwise = cmp4 t x n

line :: [Float] -> [String]
line row = [show (cmp4 row 0 0)] ++ [show (cmp4 row 1 0)] ++ [show (cmp4 row 2 0)]

grades_string4 = init(tail(transpose exam_grades))

grades_float4 :: Table -> [[Float]]
grades_float4 table = transpose (create_qtable table)

get_exam_summary_aux :: [[Float]] -> Table -> Table
get_exam_summary_aux [] _ = []
get_exam_summary_aux _ [] = []
get_exam_summary_aux (h1:t1) (h2:t2) = [(head h2) : (line h1)] ++ (get_exam_summary_aux t1 t2)

-- Task 5
get_ranking :: Table -> Table
get_ranking table = (head (compute_exam_grades table)) : (sortBy (sort1_5) (tail (compute_exam_grades table)))

sort1_5 :: Row -> Row -> Ordering
sort1_5 r1 r2
   |(tail r1) > (tail r2) = GT
   |(tail r1) == (tail r2) && (head r1 > head r2) = GT
   |otherwise = LT

-- Task 6
get_exam_diff_table :: Table -> Table
get_exam_diff_table table = first_r : (sortBy (sort1_6) (build_t table))

first_r = ["Nume", "Punctaj interviu" , "Punctaj scris" , "Diferenta"]

get_oral_gr :: Row -> Float 
get_oral_gr row = (sum (grades_float row) / 4)

build_l :: Row -> Row
build_l row
    |(get_oral_gr row) > (read (last row) :: Float) = [head row] ++
        [(printf "%.2f" (get_oral_gr row))] ++ [(printf "%.2f" ( read (last row) :: Float))] ++
        [printf "%.2f" ((get_oral_gr row) - (read (last row) :: Float))]
    |otherwise = [head row] ++ [(printf "%.2f" (get_oral_gr row))] ++
        [(printf "%.2f" ( read (last row) :: Float))] ++
        [printf "%.2f" ((read (last row) :: Float) - (get_oral_gr row))]

build_t :: Table -> Table
build_t table = map build_l (tail table)

sort1_6 :: Row -> Row -> Ordering
sort1_6 r1 r2
   |(last r1) > (last r2) = GT
   |(last r1) == (last r2) && (head r1)> (head r2) = GT
   |otherwise = LT



{-
	TASK SET 2
-}

--Prerequisite

splitByLine :: String -> [String]
splitByLine = foldr op []
    where
        op '\n' [] = ["",""]
        op '\n' acc = [] : acc
        op c [] = [[c]]
        op c (y : ys) = (c : y) : ys

splitByCom :: String -> [String]
splitByCom = foldr op []
    where
        op ',' [] = ["",""]
        op ',' acc = [] : acc
        op c [] = [[c]]
        op c (y : ys) = (c : y) : ys 

read_csv :: CSV -> Table
read_csv s = map splitByCom (splitByLine s)

rowToString :: Row -> String
rowToString r = intercalate "," r 

addLine :: Row -> String
addLine s = (rowToString s) ++ "\n"

intercalate_lines :: Table -> CSV
intercalate_lines [] = ""
intercalate_lines (x:xs) = (addLine x) ++ (intercalate_lines xs)

write_csv :: Table -> CSV
write_csv t = take ((length (intercalate_lines t)) - 1) (intercalate_lines t)


--Task 1
as_list :: String -> Table -> [String]
as_list str table@(h:t)
    |str == head(head(transpose table)) = tail(head (transpose table))
    |otherwise = as_list str (transpose (tail (transpose table)))

--Task 2
tsort :: String -> Table -> Table
tsort col t = (head t) : (sortBy (sort2 (get_index_list (head t) col 1)) (tail t))

sort2 :: Int -> Row -> Row -> Ordering
sort2 i row1 row2 
    |(return_elem i row1) > (return_elem i row2) = GT
    |(return_elem i row1) == (return_elem i row2) && (head row1) > (head row2) = GT
    |otherwise = LT

return_elem :: Int -> Row -> String
return_elem i r = if i == 1 then head r else return_elem (i - 1) (tail r)

--Task 3
vmap :: (Value -> Value) -> Table -> Table
vmap f = map(map f)

--Task 4
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f r t= r : (map f (tail t))

get_hw_grade_total :: Row -> Row
get_hw_grade_total r = (head r) : [get_sum r]

nullZero :: Row -> Row
nullZero r = map f r
    where
        f "" = "0"
        f c = c

get_grades_hw :: Row -> [Float]
get_grades_hw r = map (read :: String -> Float) (nullZero (drop 2 r))  

get_sum :: Row -> String
get_sum r = (printf "%.2f") (sum(get_grades_hw r))

--Task 5
vunion :: Table -> Table -> Table
vunion t1 t2
    |colName (head t1) (head t2) == 0 = t1
    |otherwise = t1 ++ (tail t2)


colName :: Row -> Row -> Int
colName [] [] = 1
colName [] _ = 0
colName _ [] = 0
colName (h1:t1) (h2:t2) = if (h1 == h2) then colName t1 t2 
                          else 0 

--Task 6
hunion :: Table -> Table -> Table
hunion t1 t2
    |(length t1) > (length t2) = zipWith (++) t1 (fillTable ((length t1) - (length t2)) t2)
    |otherwise = zipWith (++) (fillTable ((length t2) - (length t1)) t1) t2

fillTable :: Int -> Table -> Table
fillTable l table
    |l > 0 = (fillTable (l - 1) table) ++ [(generateBlank (head table))]
    |otherwise = table

generateBlank :: Row -> Row
generateBlank [] = []
generateBlank (h:t) = "" : (generateBlank t)

--Task 7
tjoin :: String -> Table -> Table -> Table
tjoin s t1 t2 = tjoin_aux t1 t2

tjoin_aux :: Table -> Table -> Table
tjoin_aux [] _ = []
tjoin_aux _ [] = []
tjoin_aux (h1:t1) table2@(h2:t2) = if (head h1) == (head h2) then [h1 ++ (tail h2)] ++ (tjoin_aux t1 t2) else [h1 ++ (tail (generateBlank h2))] ++ (tjoin_aux t1 table2)

--Task 8
cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian f col t1 t2 = col : (aux f (tail t1) (tail t2))

aux :: (Row -> Row -> Row) -> Table -> Table -> Table
aux _ _ [] = []
aux _ [] _ = []
aux f (h:t) t2 = (map(f h) t2) ++ (aux f t t2)

--Task 9
projection :: [String] -> Table -> Table
projection list t = transpose(proj_aux t list)

proj_aux :: Table -> [String] -> Table
proj_aux [] _ = []
proj_aux t list
    |(get_index_list list (head (head (transpose t))) 0) /= -1 = ((head (transpose t)) : (proj_aux (transpose (tail (transpose t))) list))
    |otherwise = [] ++ (proj_aux (transpose (tail (transpose t))) list)


{-
	TASK SET 3
-}

--Prerequisite

data Query =
    FromCSV CSV
    | ToCSV Query
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query
    | Graph EdgeOp Query
-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

data QResult = CSV CSV | Table Table | List [String]
-- don't confuse first 'CSV' and second 'CSV': first refers to constructor name,
-- second refers to type CSV (defined in taskset 1); same with 'Table'.

instance Show QResult where
    show (CSV csv) = show csv
    show (Table table) = write_csv table
    show (List list) = show list

class Eval a where
    eval :: a -> QResult

ret_colname :: Query -> Row
ret_colname query = head (toTable query)

instance Eval Query where
    eval (FromCSV csv) = Table (read_csv csv)
    eval (ToCSV query) = CSV (show (eval query))
    eval (AsList s query) = List (as_list s (toTable query))
    eval (Sort s query) = Table (tsort s (toTable query))
    eval (ValueMap f query) = Table (vmap f (toTable query))
    eval (RowMap f l query) = Table (rmap f l (toTable query))
    eval (VUnion query1 query2) = Table (vunion (toTable query1) (toTable query2))
    eval (HUnion query1 query2) = Table (hunion (toTable query1) (toTable query2))
    eval (TableJoin s query1 query2) = Table (tjoin s (toTable query1) (toTable query2))
    eval (Cartesian f l query1 query2) = Table (cartesian f l (toTable query1) (toTable query2))
    eval (Projection l query) = Table (projection l (toTable query))
    eval (Filter condition query) = Table ((ret_colname query) : (filter (feval (head (toTable query)) condition) (tail (toTable query))))

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

type FilterOp = Row -> Bool

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

instance FEval Float where
    feval list (Eq str ref) = \r -> ((read (r !! (get_index_list list str 0)) :: Float) == ref)
    feval list (Lt str ref) = \r -> ((read (r !! (get_index_list list str 0)) :: Float) < ref)
    feval list (Gt str ref) = \r -> ((read (r !! (get_index_list list str 0)) :: Float) > ref)
    feval list (In str ref) = \r -> (find_float ref (read ((nullZero r) !! (get_index_list list str 0)) :: Float) == True)
    feval list (FNot ref) = \r -> (not(feval list ref r))
    feval list (FieldEq str ref) = \r -> ((read (r !! (get_index_list list str 0)) :: Float) == (read (r !! (get_index_list list ref 0)) :: Float))

instance FEval String where
    feval list (Eq str ref) = \r -> ((r !!(get_index_list list str 0)) == ref)
    feval list (Lt str ref) = \r -> ((r !!(get_index_list list str 0)) < ref)
    feval list (Gt str ref) = \r -> ((r !!(get_index_list list str 0)) > ref)
    feval list (In str ref) = \r -> (find_string ref ((nullZero r) !! (get_index_list list str 0)) == True)
    feval list (FNot ref) = \r -> (not(feval list ref r))
    feval list (FieldEq str ref) = \r -> ((r !!(get_index_list list str 0)) == (r !!(get_index_list list ref 0)))

get_index_list :: [String] -> String -> Int -> Int
get_index_list [] _ _ = -1
get_index_list (x:xs) s i = if x == s then i else (get_index_list xs s (i+1))

find_string :: [String] -> String -> Bool
find_string [] _ = False
find_string (x:xs) s
    |x /= s = (find_string xs s)
    |otherwise = True  

find_float :: [Float] -> Float -> Bool
find_float [] _ = False
find_float (x:xs) s
    |x /= s = (find_float xs s)
    |otherwise = True

similarities_query :: Query
similarities_query = undefined

toTable :: Query -> Table
toTable query = read_csv (show (eval query))

correct_table :: [Char] -> [Char] -> [Char] -> String
correct_table = undefined

grades :: [Char] -> [Char] -> [Char] -> [Char] -> String
grades = undefined
