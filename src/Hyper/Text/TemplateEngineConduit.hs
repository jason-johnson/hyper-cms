module Hyper.Text.TemplateEngineConduit
(
  applyTemplate
, defaultCommands
)
where

import System.FilePath ((</>), takeDirectory, splitFileName, dropTrailingPathSeparator)
import System.Directory (doesFileExist)
import Data.Maybe (fromMaybe)
import System.IO (stderr)
import Data.ByteString.Char8 as B8
import Data.Map as M
import Data.List as L
import qualified Text.XML as X
import Text.XML (nameLocalName, namePrefix)
import Data.String (fromString)
import Control.Monad (foldM)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid (mempty)

data Variable   = Content
                | Clipboard
                | Var Text
                deriving (Eq, Ord, Show)
type VariableMap = M.Map Variable [X.Node]

data TemplateState = TemplateState {
      root          :: String
    , location      :: String
    , templateFile  :: String
    , commands      :: CommandMap
    , variables     :: VariableMap
    }

instance Show (TemplateState) where
    show s = L.concat [
        "TemplateState { root = ",
        show . root $ s,
        ", location = ",
        show . location $ s,
        ", templateFile = ",
        show . templateFile $ s,
        ", commands = ",
        show . M.keys . commands $ s,
        ", variables = ",
        show . variables $ s,
        " }"
        ]

type CommandArgs = M.Map X.Name Text
type Command = TemplateState -> CommandArgs -> [X.Node] -> IO (Maybe X.Element, TemplateState)
type CommandMap = M.Map Text Command

defaultCommands :: CommandMap
defaultCommands = M.fromList [
      ("let",   commandLet)
    , ("apply", commandApply)
    , ("var", commandVar)
    , ("content", commandContent)
    ]

applyTemplate :: FilePath -> TemplateState -> IO (Maybe X.Document)
applyTemplate template state = do
    path <- findFile current'
--    B8.hPutStrLn stderr $ "about to read file: '" `B8.append` B8.pack path `B8.append` "'"
--    jason <- B8.getLine
--    B8.hPutStrLn stderr jason
    doc <- X.readFile X.def $ fromString path
    applyTemplate' doc state { templateFile = path }
    -- X.writeFile X.def { X.rsPretty = True } "output.html" $ X.Document prologue rootE epilogue
    where
        findFile c = do
            let path = root state </> c </> template
            exists <- doesFileExist path
            if exists then return path else findFile (upDir c)
        upDir "." = error $ "template file: " ++ template ++ " not found"
        upDir dir = takeDirectory . dropTrailingPathSeparator $ dir
        current' = let (dir, file) = splitFileName (templateFile state) in
            if file == template
            then upDir dir
            else location state
        makeDoc p e r = X.Document p r e
        maybeDoc p e = fmap (makeDoc p e) . fst
        applyTemplate' (X.Document p r e) = fmap (maybeDoc p e) . processElement r

-- TODO: Should we be looking at adding State monad in here instead of manually handling state?  It would mean a transformer I think
processElement :: X.Element -> TemplateState -> IO (Maybe X.Element, TemplateState)
processElement (X.Element eName attrs children) state = do
    (children', state') <- foldM pn ([], state) children
    let children'' = L.reverse children'
    case eName of
        (X.Name {nameLocalName = name, namePrefix = Just "hyper" }) -> dispatch name state' attrs children''
        _                                                           -> return (Just $ X.Element eName attrs children'', state')
    where
        pn (cs, s) c = do
            (c', s') <- processNode c s
            return (consMaybe c' cs, s')
        dispatch name s = dispatch' name s s
        dispatch' name = fromMaybe (failFun name) . M.lookup name . commands
        failFun name = error $ "unknown command: " ++ show name ++ " called in template: " ++ (show . templateFile) state
        consMaybe (Just c) cs = c : cs
        consMaybe Nothing cs = cs

processNode :: X.Node -> TemplateState -> IO (Maybe X.Node, TemplateState)
processNode (X.NodeElement e) state = do
    (e', state') <- processElement e state
    return (fmap X.NodeElement e', state')
processNode c@(X.NodeContent _) s = return (Just c, s)
processNode c@(X.NodeComment _) s = return (Just c, s)
processNode (X.NodeInstruction _) s = return (Nothing, s)

-- commands

commandLet :: Command
commandLet state@(TemplateState { variables = vars }) args children = commandLet' args'
    where
        args' = parseAttrs parseArgs "" args
        parseArgs name [] = name
        parseArgs _ ((X.Name {nameLocalName = "name"}, n):rest) = parseArgs n rest
        parseArgs _ ((X.Name {nameLocalName = attr}, _):_) = error $ "let command recieved invalid attribute: " ++ show attr ++ " in file " ++ (show . templateFile) state
        commandLet' name = return (Nothing, state { variables = M.insert (Var name) children vars })

commandApply :: Command
commandApply state@(TemplateState { variables = vars }) args children = commandApply' args'
    where
        args' = parseAttrs parseArgs "" args
        parseArgs template [] = T.unpack template
        parseArgs _ ((X.Name {nameLocalName = "template"}, t):rest) = parseArgs t rest
        parseArgs _ ((X.Name {nameLocalName = attr}, _):_) = error $ "apply command recieved invalid attribute: " ++ show attr ++ " in file " ++ (show . templateFile) state
        getRoot (X.Document _ r _) = r
        commandApply' template = do
            B8.hPutStr stderr . (B8.append "state: ") . B8.pack . show $ state
            B8.hPutStrLn stderr . (B8.append ", children: ") . B8.pack . show $ children
            doc <- applyTemplate template state { variables = M.insert Content children vars }
            return (fmap getRoot doc, state)

commandVar :: Command
commandVar state@(TemplateState { variables = vars }) args [] = commandVar' args'
    where
        args' = parseAttrs parseArgs ("", "false") args
        parseArgs as [] = as
        parseArgs (_,dw) ((X.Name {nameLocalName = "name"}, n):rest) = parseArgs (n,dw) rest
        parseArgs (n,_) ((X.Name {nameLocalName = "div-wrap"}, dw):rest) = parseArgs (n,T.toLower dw) rest
        parseArgs _ ((X.Name {nameLocalName = attr}, _):_) = error $ "var command recieved invalid attribute: " ++ show attr ++ " in file " ++ (show . templateFile) state
        makeDiv wrap name cs = X.Element (X.Name "div" Nothing Nothing) (attrs wrap name) cs
        attrs "false" _ = mempty
        attrs "true" name = M.singleton "id" name
        attrs val _ = error $ "var command, div-wrap attributed set with unexpected value: '" ++ show val ++ "' in file " ++ (show . templateFile) state
        commandVar' (name, wrap) = return (fmap (makeDiv wrap name) $ M.lookup (Var name) vars, state)
commandVar state _ children = error $ "var command malformed, unexpected children: '" ++ show children ++ "' in file " ++ (show . templateFile) state

commandContent :: Command
commandContent state@(TemplateState { variables = vars }) args [] = commandContent' args'
    where
        args' = parseAttrs parseArgs "false" args
        parseArgs wrap [] = wrap
        parseArgs _ ((X.Name {nameLocalName = "div-wrap"}, dw):rest) = parseArgs (T.toLower dw) rest
        parseArgs _ ((X.Name {nameLocalName = attr}, _):_) = error $ "content command recieved invalid attribute: " ++ show attr ++ " in file " ++ (show . templateFile) state
        makeDiv wrap cs = X.Element (X.Name "div" Nothing Nothing) (attrs wrap) cs
        attrs "false" = mempty
        attrs "true" = M.singleton "id" "content"
        attrs val = error $ "content command, div-wrap attributed set with unexpected value: '" ++ show val ++ "' in file " ++ (show . templateFile) state
        commandContent' wrap = return (fmap (makeDiv wrap) $ M.lookup Content vars, state)
commandContent state _ children = error $ "content command malformed, unexpected children: '" ++ show children ++ "' in file " ++ (show . templateFile) state

-- helpers

parseAttrs :: (a -> [(X.Name, Text)] -> b) -> a -> Map X.Name Text -> b
parseAttrs p s = p s . M.toList