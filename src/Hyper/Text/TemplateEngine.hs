module Hyper.Text.TemplateEngine
(
  applyTemplate
)
where

import Data.ByteString as B
import Data.ByteString.Char8 as B8
import System.IO (stderr)
import System.FilePath ((</>), takeDirectory, splitFileName, dropTrailingPathSeparator)
import System.Directory (doesFileExist)
import Data.Map as M
import Data.List as L
import Data.Maybe (fromMaybe)

data Variable   = Content
                | Clipboard
                | Var ByteString
                deriving (Eq, Ord, Show)
type VariableMap = M.Map Variable ByteString

type TemplateState = (String, String, String, VariableMap)

type CommandArgs = [(ByteString, ByteString)]
type CommandMap = M.Map ByteString (CommandArgs -> ByteString -> (ByteString -> TemplateState -> IO TemplateState) -> TemplateState -> IO TemplateState)

commands :: CommandMap
commands = M.fromList [
      ("let",   commandLet)
    , ("apply", commandApply)
    , ("var",   commandVar)
    ]

-- TODO: most of these exceptions aren't actually being thrown.  That will have to be dealt with at some point
processContents :: ByteString -> (ByteString -> TemplateState -> IO TemplateState) -> TemplateState -> IO TemplateState
processContents contents write state = do
    let (static, command) = B8.breakSubstring "<hyper:" contents
    state' <- write static state
    parseCommand command write state'

parseCommand :: ByteString -> (ByteString -> TemplateState -> IO TemplateState) -> TemplateState -> IO TemplateState
parseCommand "" _ vars = return vars
parseCommand comm write state = let (command, rest) = breakCommand comm in
    processCommand command $ parseArgs command rest
    where
        parseArgs c a = let
            (args, rest) = breakEndTag a
            closed = B8.last args == '/'
            (content, rest') =  if  closed
                                then ("", rest)
                                else breakCloseTag c rest
            in
                (processArgs c args, content, if closed then B8.drop 1 rest' else dropCommand c rest')
        processArgs com = toPairs com . Prelude.filter (not . B8.null) . B8.splitWith (\c -> c == ' ' || c == '=' || c == '"')
        breakCommand = B8.break (== ' ') . B8.drop 7
        breakEndTag = B8.break (== '>')
        breakCloseTag c = B8.breakSubstring ("</hyper:" `B8.append` c `B8.append` ">") . B8.drop 1
        dropCommand c = B8.drop $ 9 + B8.length c
        toPairs _ [] = []
        toPairs c [x] = error $ "parse error in tag '" ++ B8.unpack c ++ "' attribute '" ++ B8.unpack x ++ "' has no value"
        toPairs c  (k:v:r) = (k,v): toPairs c r
        processCommand c (args, content, rest) = do
            let f = fromMaybe parseFail . M.lookup c $ commands
            state' <- f args content write state
            processContents rest write state'
            where
                parseFail = error $ "parse fail: templied called undefined command: '" ++ B8.unpack c ++ "'"

applyTemplate :: FilePath -> TemplateState -> IO TemplateState
applyTemplate template (root, current, prev, vars) = do
    path <- findFile current'
    c <- B8.readFile path
    state' <- processContents c write (root, current, path, vars)
    B8.hPutStrLn stderr $ "state is: " `B8.append` B8.pack (show state')
    return state'
    where
        findFile c = do
            let path = root </> c </> template
            exists <- doesFileExist path
            if exists then return path else findFile (upDir c)
        upDir "." = error $ "template file: " ++ template ++ " not found"
        upDir dir = takeDirectory dir
        current' = let (dir, file) = splitFileName prev in
            if file == template
            then upDir . dropTrailingPathSeparator $ dir
            else current
        write "" v = return v
        write s v = do
            B8.hPutStrLn stderr $ "writing to cache: '" `B8.append` s `B8.append` "'"
            return v

-- commands

commandApply :: CommandArgs -> ByteString -> (ByteString -> TemplateState -> IO TemplateState) -> TemplateState -> IO TemplateState
commandApply args content _ state = do
    let template = tryAttrLookup "apply" "template" args
    s' <- processContents content write state
    s'' <- applyTemplate (B8.unpack template) s'
    return $
        s''
        where
            write "" s = return s
            write s (r, c, l, v)  = return $ (r, c, l, M.insertWith (flip B8.append) Content s v)

commandLet :: CommandArgs -> ByteString -> (ByteString -> TemplateState -> IO TemplateState) -> TemplateState -> IO TemplateState
commandLet args content _ state@(r, c, l, vars) = do
    let name = tryAttrLookup "let" "name" args
    (_, _, _, vars') <- processContents content write state
    return $ (r, c, l, M.insert (Var name) (content' vars') vars)
    where
        content' v = fromMaybe "" $ M.lookup Clipboard v
        write s (r', c', l', v) = return $ (r', c', l', M.insertWith (flip B8.append) Clipboard s v)

commandVar :: CommandArgs -> ByteString -> (ByteString -> TemplateState -> IO TemplateState) -> TemplateState -> IO TemplateState
commandVar args _ write s@(_, _, _, vars) = let
    name = tryAttrLookup "var" "name" args
    value = fromMaybe "" $ M.lookup (Var name) vars
    in
        write value s

tryAttrLookup :: String -> ByteString -> CommandArgs -> ByteString
tryAttrLookup tag name args = fromMaybe parseFail . L.lookup name $ args
    where
        parseFail = error $ "parse fail: " ++ tag ++ " missing required attribute: " ++ B8.unpack name