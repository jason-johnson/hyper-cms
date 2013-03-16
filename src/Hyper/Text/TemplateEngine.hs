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

commands :: M.Map ByteString (CommandArgs -> ByteString -> VariableMap -> IO VariableMap)
commands = M.fromList [
      ("let", commandLet)
    , ("apply", commandApply)
    ]

-- TODO: most of these exceptions aren't actually being thrown.  That will have to be dealt with at some point
processFileContents :: ByteString -> (ByteString -> VariableMap -> IO VariableMap) -> VariableMap -> IO VariableMap
processFileContents contents write vars = breakFileContents contents
    where
        breakFileContents = process . B8.breakSubstring "<hyper:"
        process (static, command) = do
            vars' <- write static vars
            parseCommand command write vars'

parseCommand :: ByteString -> (ByteString -> VariableMap -> IO VariableMap) -> VariableMap -> IO VariableMap
parseCommand "" _ vars = return vars
parseCommand comm write vars = let (command, rest) = breakCommand comm in
    processCommand command $ parseArgs command rest
    where
        parseArgs c a = let
            (args, rest) = breakEndTag a
            (content, rest') = breakCloseTag c rest in
            (processArgs c args, content, dropCommand c rest')
        processArgs com = toPairs com . Prelude.filter (not . B8.null) . B8.splitWith (\c -> c == ' ' || c == '=')
        breakCommand = B8.break (== ' ') . B8.drop 7
        breakEndTag = B8.break (== '>')
        breakCloseTag c = B8.breakSubstring ("</hyper:" `B8.append` c `B8.append` ">") . B8.drop 1
        dropCommand c = B8.drop $ 9 + B8.length c
        toPairs _ [] = []
        toPairs c [x] = error $ "parse error in tag '" ++ B8.unpack c ++ "' attribute '" ++ B8.unpack x ++ "' has no value"
        toPairs c  (k:v:r) = (k,v): toPairs c r
        processCommand c (args, content, rest) = do
            let f = fromMaybe parseFail . M.lookup c $ commands
            vars' <- f args content vars
            processFileContents rest write vars'
            where
                parseFail = error $ "parse fail: templied called undefined command: '" ++ B8.unpack c ++ "'"

applyTemplate :: FilePath -> FilePath -> FilePath -> VariableMap -> IO ()
applyTemplate template current root vars = do
    c <- B8.readFile $ root </> current </> template
    vars' <- processFileContents c write vars
    B8.hPutStrLn stderr $ "vars were: " `B8.append` B8.pack (show vars')
    where
        write "" v = return v
        write s v = do
            B8.hPutStrLn stderr $ "writing to cache: '" `B8.append` s `B8.append` "'"
            return v

-- commands

commandApply :: CommandArgs -> ByteString -> VariableMap -> IO VariableMap
commandApply args content vars = do
    let template = tryAttrLookup "apply" "template" args
    vars' <- processFileContents content write vars
    return $
        vars'
        where
            write "" v = return v
            write s v = return $ M.insertWith (flip B8.append) Content s v

commandLet :: CommandArgs -> ByteString -> VariableMap -> IO VariableMap
commandLet args content vars = let name = tryAttrLookup "let" "name" args in
    return $ M.insert (Var name) content vars

tryAttrLookup :: String -> ByteString -> CommandArgs -> ByteString
tryAttrLookup tag name args = fromMaybe parseFail . L.lookup name $ args
    where
        parseFail = error $ "parse fail: " ++ tag ++ " missing required attribute: " ++ B8.unpack name