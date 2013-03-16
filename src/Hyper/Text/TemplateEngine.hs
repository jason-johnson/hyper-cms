module Hyper.Text.TemplateEngine
(
  applyTemplate
)
where

import Data.ByteString as B
import Data.ByteString.Char8 as B8
import System.IO (stderr)
import System.FilePath ((</>))
import Data.Map as M

type VariableMap = M.Map ByteString ByteString

processFileContents :: ByteString -> (ByteString -> IO ()) -> VariableMap -> IO ()
processFileContents contents write vars = do
    breakFileContents contents
        where
            process (static, command) = do
                write static
                parseCommand command
            parseCommand "" = return ()
            parseCommand c = let (command, rest) = breakCommand c in
                processCommand command $ parseArgs command rest
            parseArgs c a = let
                (args, rest) = breakEndTag a
                (content, rest') = breakCloseTag c rest in
                (processArgs args, content, dropCommand c rest')
            processArgs = Prelude.filter (not . B8.null) . B8.splitWith (\c -> c == ' ' || c == '=')
            breakFileContents = process . B8.breakSubstring "<hyper:"
            breakCommand = B8.break (== ' ') . B8.drop 7
            breakEndTag = B8.break (== '>')
            breakCloseTag c = B8.breakSubstring ("</hyper:" `B8.append` c `B8.append` ">") . B8.drop 1
            dropCommand c = B8.drop $ 9 + B8.length c
            processCommand c (args, content, rest) = B8.hPutStrLn stderr $ B8.concat [
                  "got command: '"
                , c
                , "' with arguments: '"
                , B8.pack . show $ args
                , "' content: '"
                , content
                , "' and rest: '"
                , rest
                , "'"
                ]

applyTemplate :: FilePath -> FilePath -> FilePath -> VariableMap -> IO ()
applyTemplate template current root vars = do
    c <- B8.readFile $ root </> current </> template
    processFileContents c write vars
    where
        write "" = return ()
        write s = B8.hPutStrLn stderr $ "writing to cache: '" `B8.append` s `B8.append` "'"

applyTemplate' :: FilePath -> FilePath -> FilePath -> IO ()
applyTemplate' template current root = do
    c <- B8.readFile $ root </> current </> template
    let (static,rest) = B8.breakSubstring "<hyper:" c
        (command, rest') = B8.break (== ' ') . B8.drop 7 $ rest
        (args, rest'') = B8.break (== '>') rest'
        args' = processArgs args
        (content,rest''') = B8.breakSubstring ("</hyper:" `B8.append` command `B8.append` ">") . B8.drop 1 $ rest''
        rest'''' = B8.drop (9 + B8.length command) rest''' 
    B8.hPutStrLn stderr $ "static: '" `B8.append` static `B8.append` "'"
    B8.hPutStrLn stderr $ "command: '" `B8.append`  command `B8.append` "'"
    B8.hPutStrLn stderr $ "args': '" `B8.append`  (B8.pack $ show args') `B8.append` "'"
    B8.hPutStrLn stderr $ "content: '" `B8.append`  content `B8.append` "'"
    B8.hPutStrLn stderr $ "rest'''': '" `B8.append`  rest'''' `B8.append` "'"
        where processArgs = Prelude.filter (not . B8.null) . B8.splitWith (\c -> c == ' ' || c == '=')