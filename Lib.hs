module Lib
where

import Parser
import System.IO

type MapValue = (String, AccType)
type Env = [MapValue]

-- create a default environment
emptyDataStore :: Env
emptyDataStore = []

-- manipulate environment
addToDataStore :: Env -> MapValue -> Env
addToDataStore l val = (val:l)

changeMapValue :: MapValue -> MapValue -> MapValue
changeMapValue val1@(a, _) val2@(c, _) 
    | a == c = val1
    | otherwise = val2

updateDataStore :: Env -> MapValue -> Env
updateDataStore store mapVal = (map (changeMapValue mapVal) store)


saveVal :: MapValue-> Env -> IO Env
saveVal (var, val) store = return ( case lookup var store of
    Just _ -> updateDataStore store (var, val)
    Nothing -> addToDataStore store (var, val))


-- get stuff
getVal :: String -> Env -> Int 
getVal a store = case lookup a store of
    Just (Int val) -> val
    Nothing  -> 0

getBool :: String -> Env -> Bool
getBool a store = case lookup a store of
    Just (Bool val) -> val
    Nothing  -> False

getVar :: String -> Env -> AccType 
getVar a store = case lookup a store of
    Just val -> val
    Nothing  -> NullVal


-- evaluate a term

calcTerm :: Term -> Env -> AccType
calcTerm (AccType (Sym s)) store = getVar s store
calcTerm (AccType a) store = a

-- when basic types
calcTerm (Plus ( AccType val1) ( AccType val2)) store = Int( (calcIntOp val1 store) + (calcIntOp val2 store) )
calcTerm (Minus ( AccType val1) ( AccType val2)) store = Int( (calcIntOp val1 store) - (calcIntOp val2 store) )
calcTerm (Multiply ( AccType val1) ( AccType val2)) store = Int( (calcIntOp val1 store) * (calcIntOp val2 store) )
calcTerm (Divide ( AccType val1) ( AccType val2)) store = Int( div (calcIntOp val1 store) (calcIntOp val2 store) )
calcTerm (Modulo ( AccType val1) ( AccType val2)) store = Int( mod (calcIntOp val1 store) (calcIntOp val2 store) )
calcTerm (Power ( AccType val1) ( AccType val2)) store = Int( (calcIntOp val1 store) ^ (calcIntOp val2 store) )

-- when complex types
calcTerm (Plus val1 val2) store = calcTerm (Plus (AccType ( calcTerm val1 store )) (AccType ( calcTerm val2 store ))) store
calcTerm (Minus val1 val2) store = calcTerm (Minus (AccType ( calcTerm val1 store )) (AccType ( calcTerm val2 store ))) store
calcTerm (Multiply val1 val2) store = calcTerm (Multiply (AccType ( calcTerm val1 store )) (AccType ( calcTerm val2 store ))) store
calcTerm (Divide val1 val2) store = calcTerm (Divide (AccType ( calcTerm val1 store )) (AccType ( calcTerm val2 store ))) store
calcTerm (Modulo val1 val2) store = calcTerm (Modulo (AccType ( calcTerm val1 store )) (AccType ( calcTerm val2 store ))) store
calcTerm (Power val1 val2) store = calcTerm (Power (AccType ( calcTerm val1 store )) (AccType ( calcTerm val2 store ))) store

calcTerm ( ExtractIndex (AccType (Sym arr)) index ) store = getFromPosition ( getVar arr store ) ( calcTerm index store ) store

calcTerm (ApplyFunction (AccType (Sym arr)) fct_name) store
   | fct_name == ( Func "size" ) = Int (getArrLength (getVar arr store))
   | fct_name == ( Func "size_intern" ) = Int (getArrLength (getVar arr store) - 1)

calcTerm (ApplyFunctionArg (AccType (Sym arr)) fct_name arguments) store
   | fct_name == ( Func "ins" ) = Arr (insertIntoArr arr arguments store )
   | fct_name == ( Func "rmv" ) = Arr (removeFromArr arr arguments store )


calcIntOp :: AccType -> Env -> Int
calcIntOp (Int a) store = a
calcIntOp (Sym s) store = getVal s store

-- parse one line from the input
wordsWhen     :: (Char -> Bool) -> String -> [AccType]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> (Int (read w::Int)) : wordsWhen p s''
        where (w, s'') = break p s'

evalExpression :: Exp -> Env -> IO Env
-- comment
evalExpression (Comm comm) store = do
    return store

evalExpression (BlockComm comm) store = do
    return store

-- assign
evalExpression( Assign var (Fun (Func "read")) ) store = do
    end <- isEOF 
    if end
        then do
            saveVal ( var, NullVal ) store
        else do
            line <- getLine
            let res = (Arr (wordsWhen (==' ') line))
            saveVal ( var, res ) store


evalExpression( Assign var term ) store = do
    let res = calcTerm term store
    saveVal (var, res) store

evalExpression ( ChangeArr var index new_value ) store = do
    let new_arr = (changeAtIndex (getVar var store) (calcIntOp (calcTerm index store) store) (calcTerm new_value store))
    saveVal( var, new_arr ) store

-- print
evalExpression (Print term) store = do
    printValue term store
evalExpression (PrintNewLine term) store = do
    printlnValue term store        

-- control blocks
evalExpression (IfExp val) store = do
    evalIfExpression val store
evalExpression (WhileExp cond) store = do
    evalWhileExpression cond store

eval :: [Exp] -> Env -> IO Env
eval [x] store = evalExpression x store
eval (x:xs) store = do
        newStore <- evalExpression x store
        eval xs newStore

evalCond :: Conditional -> Env -> Bool
evalCond (Condition (CondTerm (AccType (Bool b)))) store = b
evalCond (Condition (CondTerm (AccType (Sym s)))) store = getBool s store
evalCond ( And cond1 cond2 ) store = ( evalCond cond1 store ) && ( evalCond cond2 store)
evalCond ( Or cond1 cond2 ) store = ( evalCond cond1 store ) || ( evalCond cond2 store)

-- mathematical equations
evalCond (Condition (CondMath (Less val1 val2))) store = (calcIntOp (calcTerm val1 store) store) < (calcIntOp (calcTerm val2 store) store)
evalCond (Condition (CondMath (LessEqual val1 val2))) store = (calcIntOp (calcTerm val1 store) store) <= (calcIntOp (calcTerm val2 store) store)
evalCond (Condition (CondMath (Greater val1 val2))) store = (calcIntOp (calcTerm val1 store) store) > (calcIntOp (calcTerm val2 store) store)
evalCond (Condition (CondMath (GreaterEqual val1 val2))) store = (calcIntOp (calcTerm val1 store) store) >= (calcIntOp (calcTerm val2 store) store )

-- general equations
evalCond (Condition (CondMath (Equal val1 val2))) store = evalEquality (calcTerm val1 store) (calcTerm val2 store) store 
evalCond (Condition (CondMath (Diff val1 val2))) store = not (evalEquality (calcTerm val1 store) (calcTerm val2 store) store ) 


evalEquality :: AccType -> AccType -> Env -> Bool
evalEquality (Sym s) (Sym q) store = (getVar s store) == (getVar q store)
evalEquality (Sym s) a store = (getVar s store) == a
evalEquality a (Sym s) store = (getVar s store) == a
evalEquality a b store = a == b


evalIfExpression :: IfBody -> Env -> IO Env
evalIfExpression (If cond part1) store = do
    if evalCond cond store
        then eval part1 store
        else return store
evalIfExpression (IfElse cond part1 part2) store = do
    if evalCond cond store
        then eval part1 store
        else eval part2 store

evalWhileExpression :: WhileBody -> Env -> IO Env
evalWhileExpression (WhileLoop cond body) store = do
    if evalCond cond store
        then do
            newStore <- eval body store
            evalWhileExpression (WhileLoop cond body) newStore
        else return store

printValue :: Term -> Env -> IO Env
printValue ( AccType ( Arr arr ) ) store = do
    putStr (arrToStr arr store)
    return store
printValue val store = do
    putStr (escapeType (calcTerm val store) store)
    return store

printlnValue :: Term -> Env -> IO Env
printlnValue ( AccType ( Arr arr ) ) store = do
    putStrLn (arrToStr arr store)
    return store
printlnValue val store = do
    putStrLn (escapeType (calcTerm val store) store)
    return store

arrToStr :: [AccType] -> Env -> String
arrToStr [] _ = ""
arrToStr [(Int x)] _ = show x
arrToStr [(Float x)] _ = show x
arrToStr [(Bool x)] _ = show x
arrToStr [(Str x)] _ = x
arrToStr ((Sym x):arr) store = (arrToStr [( getVar x store )] store) ++ " " ++ ( arrToStr arr store )
arrToStr (x:arr) store = (arrToStr [x] store) ++ " " ++ ( arrToStr arr store )

escapeType :: AccType -> Env -> String
escapeType (Int x) _ = show x
escapeType (Float x) _ = show x
escapeType (Bool x) _ = show x
escapeType (Str x) _ = x
escapeType (Arr arr) store = arrToStr arr store
escapeType NullVal store = "null"

getArrLength :: AccType -> Int
getArrLength (Arr arr) = length arr
getArrLength _ = 0

getFromPosition :: AccType -> AccType -> Env -> AccType
getFromPosition (Arr arr) (Int 0) _ = head arr
getFromPosition arr (Sym x) store = getFromPosition arr ( getVar x store ) store
getFromPosition (Arr (arr_first:arr)) (Int x) _ = getFromPosition (Arr arr) (Int (x-1)) []


changeAtIndex :: AccType -> Int -> AccType -> AccType
changeAtIndex (Arr arr) index new_value = (Arr ( let (ys,zs) = splitAt index arr in ys ++ [new_value] ++ ( tail zs)) )

insertIntoArr :: String -> [Term] -> Env -> [AccType]
insertIntoArr arr arguments store = ys ++ [value] ++ zs
    where
        index = calcIntOp (calcTerm ( head arguments ) store) store
        value = calcTerm ( arguments !! 1 ) store
        (Arr old_arr) = getVar arr store
        (ys,zs) = splitAt index old_arr 


removeFromArr :: String -> [Term] -> Env -> [AccType]
removeFromArr arr arguments store 
   | zs == [] = ys
   | otherwise = ys ++ ( tail zs )
    where
        index = calcIntOp (calcTerm ( head arguments ) store) store
        (Arr old_arr) = getVar arr store
        (ys,zs) = splitAt index old_arr 