{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import System.Environment (getArgs)
import Control.Exception  -- for SomeException
import Language.C
import Data.ByteString.Char8 (unpack)
import qualified MyLib
import Network.Wai as Wai
import Network.HTTP.Simple
import Network.HTTP.Types as HType
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Application.Static as Static
import Network.Wai.Parse
import Data.IORef
import Lucid
import Data.Text
import GHC.IO.Encoding
import Data.List
import Data.ByteString.Lazy (toStrict,fromStrict)
import Data.Text.Encoding (encodeUtf8)


router :: Wai.Application
router req =
  case Wai.pathInfo req of
    [] -> app req
    ["result"] -> reapp req
    ["sample1"] -> sam1app req
    ["sample2"] -> sam2app req
    --["result","check"] -> chapp req
    --["kihon.php"] -> phpapp req
    _ -> notapp req

-- app :: Wai.Application
-- app req respond = do
--   respond $ responseLBS status200 [] $ renderBS mainHtml

app :: Wai.Application
app req send = do
  send $ Wai.responseFile HType.status200 [("Content-Type","text/html")] "index.html" Nothing

mainHtml :: Html ()
mainHtml = html_ $ do
  head_ $ do
    title_ "sample page"
  body_ $ do
    h1_ "Welcome to our site!"
    h2_ $ span_ "enter your code"
    div_ [class_ "sourcecode"] $ do
      form_ [action_ "/result", method_ "post"] $ do
        textarea_ [name_ "textcode", rows_ "30", cols_ "50", spellcheck_ "false"] $ do
          "write here"
        br_ []
        input_ [type_ "submit", value_ "CHECK"]
        --input_ [type_ "button", value_ "Check", onclick_ "click()", name_ "checkButton"]
    --a_ [href_ "/result"] "RESULT"
    --script_ [src_ "/main.js"] empty
    --script_ "document.write('HELLO')"
    --script_ "function click(){document.write('hello')}"
    br_ []
    --h3_ "Welcome to our site!"

-- reqapp :: Application
-- reqapp req respond = do
--     (params, _) <- parseRequestBody lbsBackEnd req
--     let maybeValue = lookup "textcode" params
--     case maybeValue of
--         Just value -> do
--             -- do something with the value
--             respond $ responseLBS ok200 [] "Form value received"
--         Nothing ->
--             respond $ responseLBS badRequest400 [] "Form value not found"

reapp :: Application
reapp req respond = do
    (params, _) <- parseRequestBody lbsBackEnd req
    let value = lookup "textcode" params
    case value of
        Just v  -> do
          writeFile "form_value.txt" (Data.ByteString.Char8.unpack v)
          contents <- readFile "form_value.txt"
          maid contents
          respond $ Wai.responseFile HType.status200 [("Content-Type","text/html")] "file.html" Nothing
        Nothing -> respond $ responseLBS ok200 [] (fromStrict $ "Form value not found")

sam1app :: Wai.Application
sam1app req send = do
  send $ Wai.responseFile HType.status200 [("Content-Type","text/txt")] "sample1.txt" Nothing

sam2app :: Wai.Application
sam2app req send = do
  send $ Wai.responseFile HType.status200 [("Content-Type","text/txt")] "sample2.txt" Nothing

-- reapp :: Wai.Application
-- reapp req send = do --reqにはいっている
--   send $ Wai.responseFile HType.status200 [("Content-Type","text/html")] "file.html" Nothing

-- chapp :: Wai.Application
-- chapp req send = do
--   --outputStrings "index2.html" strlist
--   send $ Wai.responseFile HType.status200 [("Content-Type","text/html")] "index2.html" Nothing

-- phpapp :: Wai.Application
-- phpapp req send = do
--   send $ Wai.responseFile HType.status200 [("Content-Type","text/html")] "kihon.php" Nothing

notapp :: Wai.Application
notapp req send
  = send $ Wai.responseBuilder HType.status404 [] "not found"

outputStrings :: FilePath -> [String] -> IO ()
outputStrings filename xs = do
  handle <- openFile filename WriteMode
  mapM_ (hPutStrLn handle) xs
  hClose handle

lenfunc :: Int -> Int -> [String] -> [String]
lenfunc len num list
    | (len >= num) = lenfunc len (num+1) (list++[show num])
    | otherwise = list

appendindent :: Int -> String -> String
appendindent num str
  | (num > 0) = appendindent (num-1) ("&nbsp;"++str)
  | otherwise = str

inden :: [Int] -> [String] -> [String] -> [String]
inden (i:is) (l:ls) list
  | (i > 0) = inden is ls (list++[appendindent i l])
  | otherwise = inden is ls (list++[l])
inden _ _ list = list

colorchange :: String -> [Int] -> String
colorchange str (i:is)
  | (str == (show i)) = ("<font color='red'><strong>"++str++"</strong></font>")
  | otherwise = colorchange str is
colorchange str _ = str

fontcol :: [String] -> [Int] -> [String] -> [String]
fontcol (s:ss) boo strlist = fontcol ss boo (strlist++[colorchange s boo])
fontcol _ _ strlist = strlist

specialchara :: String -> String -> String
specialchara (s:ss) str
  | (s == '<') = specialchara ss (str++"&lt")
  | (s == '>') = specialchara ss (str++"&gt")
  | otherwise = specialchara ss (str++[s])
specialchara _ str = str

specialsearch :: Int -> String -> Int
specialsearch num (x:xs)
  | (x == '<')= specialsearch (num+1) xs
  | otherwise = specialsearch num xs
specialsearch num _ = num

special :: [String] -> [String] -> [String]
special (s:ss) str = special ss (str++[specialchara s ""])
special _ str = str

indenterror :: Int -> Int -> String
indenterror l i 
  | (i > 0) = show l ++ "行目 インデントをスペース" ++ show i ++ "個分追加してください<br>"
  | otherwise = show l ++ "行目 インデントをスペース" ++ show (i*(-1)) ++ "個分減らしてください<br>"

printferror :: Int -> String
printferror l = show l ++ "行目 printfで指定されている引数の数が違います<br>"

scanferror :: Int -> String
scanferror l = show l ++ "行目 scanfで指定されている引数の数が違います<br>"

iferror :: Int -> String
iferror l = show l ++ "行目 if文の条件式は = ではなく == を使用してください<br>"

duplierror :: Int -> String
duplierror l = show l ++ "行目の関数名が他の関数と重複しています<br>"

funcerror :: Int -> String
funcerror l = show l ++ "行目の関数にreturnがありません<br>"

errorkind :: [Int] -> [Int] -> [Int] -> [String] -> [String]
errorkind (k:ks) (l:ls) indent str
  | (k == 1) = errorkind ks ls indent (str++[indenterror l (indent !! (l-1))])
  | (k == 2) = errorkind ks ls indent (str++[printferror l])
  | (k == 3) = errorkind ks ls indent (str++[scanferror l])
  | (k == 4) = errorkind ks ls indent (str++[iferror l])
  | (k == 5) = errorkind ks ls indent (str++[duplierror l])
  | (k == 6) = errorkind ks ls indent (str++[funcerror l])
errorkind _ _ _ str = str

main :: IO ()
main = do
  -- writeFile "sample.html" ""
  -- handle <- openFile "sample.html" AppendMode
  -- hSetEncoding handle utf8
  -- hPutStrLn handle "<!DOCTYPE html>"
  -- hPutStrLn handle "<html>"
  -- hPutStrLn handle "<head>"
  -- hPutStrLn handle "<meta http-equiv='Conten-Type' content='text/html'; charset=UTF-8>"
  -- hPutStrLn handle "<title>サンプルコード</title>"
  -- hPutStrLn handle "</head>"
  -- hPutStrLn handle "<body>"
  -- hPutStrLn handle 
  -- hPutStrLn handle "<form action='/..', method='post'>"
  -- hPutStrLn handle "<input type='submit' value='TOP'>"
  -- hPutStrLn handle "</form>"
  -- hPutStrLn handle "</body>"
  -- hPutStrLn handle "</html>"
  -- hClose handle

  writeFile "index.html" ""
  handle <- openFile "index.html" AppendMode
  hSetEncoding handle utf8
  hPutStrLn handle "<!DOCTYPE html>"
  hPutStrLn handle "<html>"
  hPutStrLn handle "<head>"
  hPutStrLn handle "<meta http-equiv='Conten-Type' content='text/html'; charset=UTF-8>"
  hPutStrLn handle "<title>解析システム</title>"
  hPutStrLn handle "<style type='text/css'>"
  hPutStrLn handle ".contentA{"
  hPutStrLn handle "width:50%;"
  hPutStrLn handle "height:auto;"
  hPutStrLn handle "}"
  hPutStrLn handle ".contentB{"
  hPutStrLn handle "width:50%;"
  hPutStrLn handle "height:auto;"
  hPutStrLn handle "}"
  hPutStrLn handle ".main{"
  hPutStrLn handle "display:flex;"
  hPutStrLn handle "}"
  hPutStrLn handle "</style>"
  hPutStrLn handle "</head>"
  hPutStrLn handle "<body>"
  hPutStrLn handle "<h1><strong>C言語ソースコード解析システムです。</strong></h1>"
  hPutStrLn handle "<h2>解析したいソースコードを入力してCHECKボタンを押してください。</h2>"
  hPutStrLn handle "<div class='main'>"
  hPutStrLn handle "<div class='contentA'>"
  hPutStrLn handle "<form action='/result', method='post'>"
  hPutStrLn handle "<textarea name='textcode' rows='30' cols='50' spellcheck='false'></textarea><br>"
  hPutStrLn handle "<input type='submit' value='CHECK'>"
  hPutStrLn handle "</form>"
  hPutStrLn handle "</div>"
  hPutStrLn handle "<div class='contentB'>"
  --hPutStrLn handle "<a href='/sample1' target='_blank' rel='noopener noreferrer'>サンプルコード１</a><br>"
  --hPutStrLn handle "<a href='/sample2' target='_blank' rel='noopener noreferrer'>サンプルコード２</a>"
  hPutStrLn handle "</div>"
  hPutStrLn handle "</div>"
  hPutStrLn handle "</body>"
  hPutStrLn handle "</html>"
  hClose handle
  --outputStrings "index.html" htmllist
  putStrLn $ "http://localhost:8080/"
  Warp.run 8080 router

-- divother :: [Int] -> [Int] -> [Int] -> [Int]
-- divother (l:ls) (s:ss) list
--   | (((l == 1) && (s == 1)) ==True) = divother ls ss (list++[0])
--   | otherwise = divother ls ss (list++[l])
-- divother _ _ list = list

divother :: [Int] -> [Int] -> [Int] -> [Int]
divother (l:ls) (s:ss) list
 | (s == 1) = divother ls ss (list++[0])
 | otherwise = divother ls ss (list++[l])
divother _ _ list = list

maid :: String -> IO ()
maid source = do
    --args <- getArgs
    --MyLib.someFunc
    --source <- Prelude.readFile $ args !! 0
    let (strs, lists, indent, boo, kind, div) = MyLib.search source
    writeFile "file2.txt" source
    --let list = ["Hello, ぴWorld!"]
    --let list = Prelude.map (++ "<br>") lists
    let len = Prelude.map Prelude.length (Prelude.lines source)
    --print (indent, len)
    let indentdiv = divother indent len []
    --print indentdiv
    putStrLn("")
    let strss = special strs []
    let strip = Prelude.map MyLib.stripStart strss
    let indentstr = inden indentdiv strip []
    --print(indentstr)
    let str = Prelude.map (++ "<br>") indentstr
    let leng = Prelude.length str
    let lengt = lenfunc leng 1 []
    --print boo
    --let ind = Prelude.map ("<font color='red'>"++) indentstr
    --let indstr = Prelude.map (++"</font>") ind
    let le = fontcol lengt boo []
    let strlen = Prelude.map (++ "<br>") le
    --print str
    -- print indent
    -- print div
    -- print (div !! 3)
    -- print (lists, kind)
    let zi = Prelude.zip lists kind
    -- print zi
    --myTupleList = [(1, "apple"), (2, "banana"), (3, "cherry")]
    let sortList = sortBy (\(a, _) (b, _) -> compare a b) zi
    -- print sortList
    let (li, ki) = Prelude.unzip sortList
    -- print (li,ki)

    --print indentstr
    --print strss
    -- let str' = convertToString(putStrLn ("ぷ"))
    -- print str'
    --print japaneseList
    --print lists
    --print kind
    let list = errorkind ki li div []
    
    let strlist = ["<!DOCTYPE html>",
                   "<html>",
                   "<head>",
                   "<meta http-equiv='Conten-Type' content='text/html'; charset=UTF-8>",
                   "<title>TITLE</title>",
                   "<style type='text/css'>",
                   "p3{color:blue}",
                   ".contentA{",
                   "width:5%;",
                   "height:auto;",
                   "}",
                   ".contentB{",
                   "width:35%;",
                   "height:auto;",
                   "}",
                   ".contentC{",
                   "width:60%;",
                   "height:auto;",
                   "}",
                   ".main{",
                   "display:flex;",
                   "}",
                   "</style>",
                   "</head>",
                   "<body>",
                   "<h2><strong>RESULT</strong></h2>",
                   "<div class='main'>",
                   "<div class='contentA'>"]++strlen++["</div>",
                   "<div class='contentB', style='background-color:lightyellow'>"]++str++["</div>",
                   "<div class='contentC'>",
                   "<p3><strong>ERROR</strong></p3><br>",
                   "<p4>"]++list++["</p4>",
                   "</div>",
                   "</div>",
                   "</body>",
                   "</html>"]

    let htmlString = "<html><body>Hello, ぴWorld!</body></html>"
    -- HTMLファイルに書き込み
    writeFile "file.html" ""
    handle <- openFile "file.html" AppendMode
    hSetEncoding handle utf8
    hPutStrLn handle "<!DOCTYPE html>"
    hPutStrLn handle "<html>"
    hPutStrLn handle "<head>"
    hPutStrLn handle "<meta http-equiv='Conten-Type' content='text/html'; charset=UTF-8>"
    hPutStrLn handle "<title>解析結果ページ</title>"
    hPutStrLn handle "<style type='text/css'>"
    hPutStrLn handle "p3{color:blue}"
    hPutStrLn handle ".contentA{"
    hPutStrLn handle "width:5%;"
    hPutStrLn handle "height:auto;"
    hPutStrLn handle "}"
    hPutStrLn handle ".contentB{"
    hPutStrLn handle "width:35%;"
    hPutStrLn handle "height:auto;"
    hPutStrLn handle "}"
    hPutStrLn handle ".contentC{"
    hPutStrLn handle "width:60%;"
    hPutStrLn handle "height:auto;"
    hPutStrLn handle "}"
    hPutStrLn handle ".main{"
    hPutStrLn handle "display:flex;"
    hPutStrLn handle "}"
    hPutStrLn handle "</style>"
    hPutStrLn handle "</head>"
    hPutStrLn handle "<body>"
    hPutStrLn handle "<h2><strong>解析結果</strong></h2>"
    hPutStrLn handle "<div class='main'>"
    hPutStrLn handle "<div class='contentA'>"
    hPutStrLn handle (Prelude.concat strlen)
    hPutStrLn handle "</div>"
    hPutStrLn handle "<div class='contentB', style='background-color:lightyellow'>"
    hPutStrLn handle (Prelude.concat str)
    hPutStrLn handle "</div>"
    hPutStrLn handle "<div class='contentC'>"
    hPutStrLn handle "<p3><strong>ERROR</strong></p3><br>"
    hPutStrLn handle "<p4>"
    hPutStrLn handle (Prelude.concat list)
    hPutStrLn handle "</p4>"
    hPutStrLn handle "</div>"
    hPutStrLn handle "</div><br>"
    -- hPutStrLn handle "<form action='/..', method='post'>"
    -- hPutStrLn handle "<input type='submit' value='TOP'>"
    -- hPutStrLn handle "</form>"
    hPutStrLn handle "<a href='/..'>TOP</a><br>"
    hPutStrLn handle "</body>"
    hPutStrLn handle "</html>"
    hClose handle
    --let strlist = []
    --outputStrings "index2.html" strlist
    --putStrLn $ "http://localhost:8080/"
    --Warp.run 8080 router
    --Warp.run 8080 hello
    --res <- httpLBS "https://example.com"
    --print (getResponseBody res)