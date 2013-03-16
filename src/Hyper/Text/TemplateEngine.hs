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
import Data.List as L
import Data.Maybe (fromMaybe)

data Variable = Content | Var ByteString deriving (Eq, Ord, Show)
type VariableMap = M.Map Variable ByteString

type CommandArgs = [(ByteString, ByteString)]

commands :: M.Map ByteString (CommandArgs -> ByteString -> VariableMap -> VariableMap)
commands = M.fromList [
    ("let", commandLet)
    ]

-- TODO: most of these exceptions aren't actually being thrown.  That will have to be dealt with at some point
processFileContents :: ByteString -> (ByteString -> IO ()) -> VariableMap -> IO ()
processFileContents contents write vars = breakFileContents contents
    where
        process (static, command) = do
            write static
            parseCommand command
        parseCommand "" = B8.hPutStrLn stderr $ "variables were: " `B8.append` (B8.pack $ show  vars)
        parseCommand c = let (command, rest) = breakCommand c in
            processCommand command $ parseArgs command rest
        parseArgs c a = let
            (args, rest) = breakEndTag a
            (content, rest') = breakCloseTag c rest in
            (processArgs c args, content, dropCommand c rest')
        processArgs com = toPairs com . Prelude.filter (not . B8.null) . B8.splitWith (\c -> c == ' ' || c == '=')
        breakFileContents = process . B8.breakSubstring "<hyper:"
        breakCommand = B8.break (== ' ') . B8.drop 7
        breakEndTag = B8.break (== '>')
        breakCloseTag c = B8.breakSubstring ("</hyper:" `B8.append` c `B8.append` ">") . B8.drop 1
        dropCommand c = B8.drop $ 9 + B8.length c
        toPairs _ [] = []
        toPairs c [x] = error $ "parse error in tag '" ++ B8.unpack c ++ "' attribute '" ++ B8.unpack x ++ "' has no value"
        toPairs c  (k:v:r) = (k,v): toPairs c r
        processCommand c (args, content, rest) = let
            f = fromMaybe parseFail . M.lookup c $ commands
            vars' = f args content vars in
            processFileContents rest write vars'
            where
                parseFail = error $ "parse fail: templied tried to use non-existent command: '" ++ B8.unpack c ++ "'"


--B8.hPutStrLn stderr $ B8.concat [
--              "got command: '"
--            , c
--            , "' with arguments: '"
--            , B8.pack . show $ args
--            , "' content: '"
--            , content
--            , "' and rest: '"
--            , rest
--            , "'"
--            ]

applyTemplate :: FilePath -> FilePath -> FilePath -> VariableMap -> IO ()
applyTemplate template current root vars = do
    c <- B8.readFile $ root </> current </> template
    processFileContents c write vars
    where
        write "" = return ()
        write s = B8.hPutStrLn stderr $ "writing to cache: '" `B8.append` s `B8.append` "'"

-- commands

commandLet :: CommandArgs -> ByteString -> VariableMap -> VariableMap
commandLet args content vars = let name = fromMaybe parseFail . L.lookup "name" $ args in
    M.insert (Var name) content vars
    where
        parseFail = error "parse fail: let tag without name attribute"