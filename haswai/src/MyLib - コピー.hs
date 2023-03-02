module MyLib where

import Control.Exception  -- for SomeException

import Language.C

import Data.Loc (Pos, Loc(Loc, NoLoc), SrcLoc,
                 startPos, noLoc, locOf, locStart, locEnd, posCol, posLine)
import Data.String
import Data.Either
import qualified Data.ByteString as B (ByteString,length,pack)
import Data.ByteString.UTF8 as U8 (fromString, toString)
import Data.Char(ord, isSpace)
import Data.List(dropWhileEnd)
import Text.Show.Unicode

someFunc :: IO ()
someFunc = putStrLn ("Hello")

start (Loc s _) = s
end (Loc _ e)   = e 

parseProg :: String -> Either SomeException [Definition]
parseProg str = parse [C11] [] parseUnit (U8.fromString str) Nothing

eraseComments :: String -> String -> String
eraseComments [] rs  = reverse rs
eraseComments [a] rs = reverse (a : rs)
eraseComments ('/' : '*' : cs) rs = eraseCommentsIn cs (' ' : ' ' : rs)
  where eraseCommentsIn [] _  = error "EOF encountered in comment"
        eraseCommentsIn [a] _ = error "EOF encountered in comment"
        eraseCommentsIn ('*' : '/' : cs) rs = eraseComments cs (' ' : ' ' : rs)
        eraseCommentsIn ('\n' : cs) rs      = eraseCommentsIn cs ('\n' : rs)
        eraseCommentsIn (c : cs) rs | ord c < 128 = eraseCommentsIn cs (' ': rs)
                                    | otherwise
           = let n = B.length (U8.fromString [c])
               in eraseCommentsIn cs (replicate n ' ' ++ rs)
eraseComments ('"' : cs) rs = eraseCommentsInStr cs ('"' : rs)
  where eraseCommentsInStr [] _        = error "EOF encountered in string literal"
        eraseCommentsInStr ('"' : cs) rs = eraseComments cs ('"' : rs)
        eraseCommentsInStr ('\\' : c : cs) rs
          = eraseCommentsInStr cs (c : '\\' : rs)
        eraseCommentsInStr (c : cs) rs = eraseCommentsInStr cs (c : rs)
eraseComments (c : cs) rs    = eraseComments cs (c : rs)

untabify :: String -> String
untabify line = untabifyAux line "" 0
  where
    untabifyAux [] r _    = reverse r
    untabifyAux ('\t' : cs) r idx
      = let n = 8 - idx `mod` 8
          in untabifyAux cs (replicate n ' ' ++ r) (idx + n)
    untabifyAux (c : cs) r idx = untabifyAux cs (c : r) (idx + 1)

strsearch :: Int -> String -> Int
strsearch num (x1:x2:xs)
  | (((x1 == '%') && (x2 == 'd')) == True) = strsearch (num+1) xs
  | otherwise = strsearch num (x2:xs)
strsearch num _ = num

abStm :: Stm -> Maybe Int -> ([Int], [Int]) --(種類[printf(1), if(2)], 真偽[printf(1,2), if(3,4)])

abStm (If (Assign _ _ _ _) _ Nothing loc) _ = ([2], [4])
abStm (If (BinOp _ _ _ _) (Exp (Just (FnCall (Var (Id "printf" _) _) ((Const (StringConst _ str _) _):args) _)) _) Nothing _) _
  | (strsearch 0 str) == (length args) = ([2, 1], [3, 1])
  | otherwise = ([2, 1], [3, 2])
abStm (Exp (Just (FnCall (Var (Id "printf" _) _) ((Const (StringConst _ str _) _):args) _)) loc) override
  | (strsearch 0 str) == (length args) = ([1], [1])
  | otherwise = ([1], [2])
abStm (Exp (Just (FnCall (Var (Id "scanf" _) _) ((Const (StringConst _ str _) _):args) _)) loc) override
  | (strsearch 0 str) == (length args) = ([3], [5])
  | otherwise = ([3], [6])
abStm stm _ = ([], [])
--[posLine (start (locOf loc))],

abBlockItem :: BlockItem -> ([Int], [Int])
abBlockItem (BlockStm stm)
  = let (kind, bool) = abStm stm Nothing
     in (kind, bool)
abBlockItem bi = ([], [])

abFunc :: Func -> ([Int], [Int])
abFunc (Func declSpec id decl params blockItems loc)
  = let (kind, bool) = unzip $ map abBlockItem blockItems
      in (concat kind, concat bool)
abFunc (OldFunc declSpec id decl ids initGroup blockItems loc)
  = let (kind, bool) = unzip $ map abBlockItem blockItems
      in (concat kind, concat bool)

abDefinition :: Definition -> ([Int], [Int])
abDefinition (FuncDef func loc)
  = let (kind, bool) = abFunc func
      in  (kind, bool)
abDefinition def = ([], [])

abTranslationUnit :: [Definition] -> ([Int], [Int])
abTranslationUnit defs = let (kind, bool) = unzip $ map abDefinition defs
                           in (concat kind, concat bool)

typeStm :: Stm -> Maybe Int -> Int --関数の中にreturnがあるか判定
typeStm (Return (Just (Const (IntConst str _ int _) _)) _) _ = 1  
typeStm stm _ = 0

typeBlockItem :: BlockItem -> Int
typeBlockItem (BlockStm stm)
  = let num = typeStm stm Nothing
     in num
typeBlockItem bi = 0

typestruct :: DeclSpec -> [Int] --関数がint型-１
typestruct (DeclSpec storage typequal typespec loc)
  | ((typespec == (Tint Nothing loc)) == True) = [1]
  | otherwise = [0]

typeFunc :: Func -> ([Int], [Int])
typeFunc (Func declSpec id decl params blockItems loc)
  = let bool = map typeBlockItem blockItems
        kind = typestruct declSpec 
      in (kind, bool)
typeFunc (OldFunc declSpec id decl ids initGroup blockItems loc)
  = let bool = map typeBlockItem blockItems
        kind = typestruct declSpec 
      in (kind, bool)

typeDefinition :: Definition -> ([Int], [Int])
typeDefinition (FuncDef func loc)
  = let (kind, bool) = typeFunc func
      in  (kind, bool)
typeDefinition def = ([], [])

typeSpec :: [Definition] -> ([[Int]], [[Int]])
typeSpec defs = let (kind, bool) = unzip $ map typeDefinition defs
                  in (kind, bool)

namelist :: Id -> ([String], [Int])
namelist (Id string loc) = ([string], [0])

nameFunc :: Func -> ([String], [Int])
nameFunc (Func declSpec id decl params blockItems loc)
  = let (kind, bool) = namelist id
--        kind2 = nameBlockitem blockItems 
      in (kind, bool)

nameDefinition :: Definition -> ([String], [Int])
nameDefinition (FuncDef func loc)
  = let (kind, bool) = nameFunc func
      in  (kind, bool)
nameDefinition def = ([], [])

funcName :: [Definition] -> ([String], [Int])
funcName defs = let (kind, bool) = unzip $ map nameDefinition defs
                  in (concat kind, concat bool)

linenum :: Int -> Int -> [Int] -> [Int]
linenum maxnum num list = if maxnum > num then linenum maxnum (num+1) (list++[num]) else list

printffind :: Int -> String -> Int
printffind num (x1:x2:x3:x4:x5:x6:xs)
  | (((x1 == 'p') && (x2 == 'r') && (x3 == 'i') && (x4 == 'n') && (x5 == 't') && (x6 == 'f')) == True) = printffind (num+1) xs
  | otherwise = printffind num (x2:x3:x4:x5:x6:xs)
printffind num _ = num

printfsearch :: Int -> String -> Int
printfsearch num source = do
                              printffind 0 source

scanffind :: Int -> String -> Int
scanffind num (x1:x2:x3:x4:x5:xs)
  | (((x1 == 's') && (x2 == 'c') && (x3 == 'a') && (x4 == 'n') && (x5 == 'f')) == True) = scanffind (num+1) xs
  | otherwise = scanffind num (x2:x3:x4:x5:xs)
scanffind num _ = num

scanfsearch :: Int -> String -> Int
scanfsearch num source = do
                              scanffind 0 source

iffind :: Int -> String -> Int
iffind num (x1:x2:xs)
  | (((x1 == 'i') && (x2 == 'f')) == True) = iffind (num+1) xs
  | otherwise = iffind num (x2:xs)
iffind num _ = num

ifsearch :: Int -> String -> Int
ifsearch num source = do
                              iffind 0 source

printfboolcheck :: [Int] -> Int -> [Int] -> [Int]
printfboolcheck (b:bs) num list | b == 2 = printfboolcheck bs (num+1) (list++[num])
                                | b == 1 = printfboolcheck bs (num+1) list
                                | otherwise = printfboolcheck bs num list
printfboolcheck _ num list = list

scanfboolcheck :: [Int] -> Int -> [Int] -> [Int]
scanfboolcheck (b:bs) num list | b == 6 = scanfboolcheck bs (num+1) (list++[num])
                               | b == 5 = scanfboolcheck bs (num+1) list
                               | otherwise = scanfboolcheck bs num list
scanfboolcheck _ num list = list

ifboolcheck :: [Int] -> Int -> [Int] -> [Int]
ifboolcheck (b:bs) num list | b == 4 = ifboolcheck bs (num+1) (list++[num])
                            | b == 3 = ifboolcheck bs (num+1) list
                            | otherwise = ifboolcheck bs num list
ifboolcheck _ num list = list

checklist :: Int -> Int -> [Int] -> [Int] -> [Int] -> [Int]
checklist num1 num2 (s:ss) (b:bs) list | s == 0 = checklist (num1+1) num2 ss (b:bs) list
                                       | (s == 1 && b /= num2) = checklist (num1+1) (num2+1) ss (b:bs) list
                                       | (s == 1 && b == num2) = checklist (num1+1) (num2+1) ss bs (list++[num1])
checklist _ _ _ _ list = list

funccheck :: [[Int]] -> [[Int]] -> [Int] -> [Int]
funccheck (k:ks) (b:bs) list
  | (((k == [1]) && ((sum b) == 1)) == True) = funccheck ks bs (list++[1])
  | (((k == [1]) && ((sum b) == 1)) == False) = funccheck ks bs (list++[0])
funccheck [] [] list = list

duplicheck :: [String] -> [Int] -> [Int]
duplicheck (l1:l2:ls) list
  | ((l1 == l2) == True) = (list++[1])--duplicheck (l2:ls) (list++[1]) (num+1)
  | otherwise = duplicheck (l1:ls) list
duplicheck _ list = (list++[0])

dupli :: [String] -> [Int] -> [Int]
dupli (l:ls) list = (list ++ (duplicheck (l:ls) [])) ++ dupli ls list
dupli _ list = list

stripStart :: String -> String
stripStart = dropWhile isSpace

strlen :: [Int] -> [Int] -> [Int] -> [Int]
strlen (l:ls) (s:ss) list = strlen ls ss (list++[l-s])
strlen _ _ list = list

boxfind :: Int -> String -> Int
boxfind num (x:xs)
  | (x == '{') = boxfind (num+1) xs
  | (x == '}') = boxfind (num-1) xs
  | otherwise = boxfind num xs
boxfind num _ = num

boxsearch :: Int -> String -> Int
boxsearch num source = do
                            boxfind 0 source

numlist :: Bool -> [Int] -> Int -> Int -> Int -> [Int]
numlist b list num num2 len
  | (num > len) = list
  | ((b == True) && (num >= num2)) = numlist b (list++[4]) (num+1) num2 len
  | ((b == True) && (num < num2)) = numlist b (list++[0]) (num+1) num2 len
  | ((b == False) && (num >= num2)) = numlist b (list++[-4]) (num+1) num2 len
  | ((b == False) && (num < num2)) = numlist b (list++[0]) (num+1) num2 len

sumlist :: [Int] -> [Int] -> [Int] -> [Int]
sumlist (l:ls) (s:ss) list = sumlist ls ss (list++[l+s])
sumlist _ _ list = list

divlist :: [Int] -> [Int] -> [Int] -> [Int]
divlist (l:ls) (s:ss) list = divlist ls ss (list++[l-s])
divlist _ _ list = list

makelist :: Int -> Int -> [Int] -> [Int]
makelist num len list
  | (num < len) = makelist (num+1) len (list++[0])
  | otherwise = list

boxnum :: [Int] -> Int -> Int -> [Int] -> [Int]
boxnum (b:bl) num len list
  | (b == 1) = boxnum bl (num+1) len (sumlist list (numlist True [] 1 (num+1) len) [])
  | (b == -1) = boxnum bl (num+1) len (sumlist list (numlist False [] 1 num len) [])
  | otherwise = boxnum bl (num+1) len list
boxnum _ _ _ list = list

printfbool :: [Int] -> [String] -> [String]
printfbool (l:ls) list = printfbool ls (list++["Line " ++ show l ++ " printf error"])
printfbool _ list = list

scanfbool :: [Int] -> [String] -> [String]
scanfbool (l:ls) list = scanfbool ls (list++["Line " ++ show l ++ " scanf error"])
scanfbool _ list = list

printifbool :: [Int] -> [String] -> [String]
printifbool (l:ls) list = printifbool ls (list++["Line " ++ show l ++ " if error"])
printifbool _ list = list

printduplibool :: [Int] -> Int -> [String] -> [String]
printduplibool (l:ls) num list
  | (l == 1) = printduplibool ls (num+1) (list++["FuncNo." ++ show num ++ " overlap error"])
  | otherwise = printduplibool ls (num+1) list
printduplibool _ _ list = list

printfuncbool :: [Int] -> Int -> [String] -> [String]
printfuncbool (l:ls) num list
  | (l == 0) = printfuncbool ls (num+1) (list++["FuncNo." ++ show num ++ " return error"])
  | otherwise = printfuncbool ls (num+1) list
printfuncbool _ _ list = list

printindentbool :: [Int] -> Int -> [String] -> [String]
printindentbool (l:ls) num list
  | (l > 0) = printindentbool ls (num+1) (list++["Line " ++ show num ++ " " ++ show l ++ " indent error"])
  | otherwise = printindentbool ls (num+1) list
printindentbool _ _ list = list

search :: String -> ([String], [String], [Int])
search source
  = let source1 = source--lines (eraseComments source "")
      in case parseProg source1 of
        Left ex    -> (["parse error!"],[],[])
        Right defs ->
          let list = []
              (kind, bool) = abTranslationUnit defs
              (kind2, bool2) = typeSpec defs
              (kind3, bool3) = funcName defs
              --(kind3, bool3) = funcName defs
              --col1 = posCol (head pos); row1 = posLine (head pos)
              ls2 = map untabify (lines source)
            in do
                  --print defs  
                  --putStrLn "-----"
                  --let list = []
                  --putStr (unlines ls2)
                  --putStrLn ""
                  --print (kind, bool)
                  let str = lines source1
                  let printfsourcelist = (map (printfsearch 0) str)
                  let printfboollist = (printfboolcheck bool 1 list)
                  let ifsourcelist = (map (ifsearch 0) str)
                  let ifboollist = (ifboolcheck bool 1 list)
                  let scanfsourcelist = (map (scanfsearch 0) str)
                  let scanfboollist = (scanfboolcheck bool 1 list)
                  let len = length ls2
                  let str = lines source1
                  let lslen = map length ls2
                  let strip = map stripStart ls2
                  let striplen = map length strip
                  let divlen = strlen lslen striplen list
                  let boxsourcelist = (map (boxsearch 0) str)
                  --print boxsourcelist
                  let bonu = boxnum boxsourcelist 1 len (makelist 1 len list)
                  --print (bonu++[0])
                  let div = divlist (bonu++[0]) divlen list
                  --printindentbool div 1 
                  --print ("printf ", printfsourcelist)
                  --print ("if ", ifsourcelist)
                  --print ("printf ", printfboollist)
                  --print ("if ", ifboollist)
                  let b1 = printfbool (checklist 1 1 printfsourcelist printfboollist list) list
                  let b2 = scanfbool (checklist 1 1 scanfsourcelist scanfboollist list) b1
                  let b3 = printifbool (checklist 1 1 ifsourcelist ifboollist list) b2
                  --print (kind2, bool2)
                  --printduplibool (dupli kind3 []) 1
                  let b4 = printduplibool (dupli kind3 []) 1 b3
                  let b5 = printfuncbool (funccheck kind2 bool2 list) 1 b4
                  let b6 = printindentbool div 1 b5
                  (ls2, b6, divlen)
                  --print(b1+b2+b3)