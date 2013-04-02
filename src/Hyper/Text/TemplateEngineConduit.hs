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
processElement (X.Element (X.Name {nameLocalName = name, namePrefix = Just "hyper" }) attrs children) state = do
    (children', state') <- foldM pn ([], state) children
    dispatch state' attrs $ L.reverse children'
    where
        pn (cs, s) c = do
            (c', s') <- processNode c s
            return (consMaybe c' cs, s')
        dispatch s = dispatch' s s
        dispatch' = fromMaybe failFun . M.lookup name . commands
        failFun = error $ "unknown command: " ++ show name ++ " called in template: " ++ (show . templateFile) state
        consMaybe (Just c) cs = c : cs
        consMaybe Nothing cs = cs
processElement element state = return (Just element, state)

processNode :: X.Node -> TemplateState -> IO (Maybe X.Node, TemplateState)
processNode (X.NodeElement e) state = do
    (e', state') <- processElement e state
    return (fmap X.NodeElement e', state')
processNode c@(X.NodeContent _) s = return (Just c, s)
processNode c@(X.NodeComment _) s = return (Just c, s)
processNode (X.NodeInstruction _) s = return (Nothing, s)

-- commands

commandLet :: Command
commandLet state@(TemplateState { variables = vars }) args children = do
    commandLet' args'
    where
        args' = parseArgs "" . M.toList $ args
        parseArgs name [] = name
        parseArgs _ ((X.Name {nameLocalName = "name"}, n):rest) = parseArgs n rest
        parseArgs _ ((X.Name {nameLocalName = attr}, _):_) = error $ "let command recieved invalid attribute: " ++ show attr ++ " in file " ++ (show . templateFile) state
        commandLet' name = return (Nothing, state { variables = M.insert (Var name) children vars })

commandApply :: Command
commandApply state _args children = do
    B8.hPutStr stderr . (B8.append "state: ") . B8.pack . show $ state
    B8.hPutStrLn stderr . (B8.append ", children: ") . B8.pack . show $ children
    error "commandApply not yet implemented"

-- helpers

parseAttrs :: (a -> [(X.Name, Text)] -> b) -> a -> Map X.Name Text -> b
parseAttrs p s = p s . M.toList
