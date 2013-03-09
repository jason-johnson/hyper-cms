module Hyper.Config.File.Parser
(
  parseConfigFile
, parseInput
)
where

import           Control.Applicative
import           Text.Parsec.Language          (emptyDef)
import qualified Text.Parsec.Token             as P
import           Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

data CValue = CString String
            | CInt Integer
            | CDouble Double
            | CBool Bool
            | CList [CValue]
            | CDefault
              deriving (Eq, Ord, Show)

data CEntry = CEntry String CValue deriving (Show)

data CSection = CSection String [CEntry] deriving (Show)

-- TODO: See if we could return a Record instead of this poorly typed CSection strategy.  Surely there is a way to use monoids or something to apply the config as we read it

p_configuration :: CharParser () [CSection]
p_configuration = spaces *> comments *> many1 (p_section)

p_between :: Char -> CharParser () a -> Char -> CharParser () a
p_between left parser right =
    between (char left <* spaces) (char right) (parser <* spaces)

p_section :: CharParser () CSection
p_section = CSection <$> p_between '[' (many1 ch) ']' <*> (spaces *> p_entries) <?> "section"
        where ch = satisfy (`notElem` "]\"\\")

p_entries :: CharParser () [CEntry]
p_entries = spaces *> comments *> many (entry <* comments) <?> "entries"
        where
                entry = CEntry <$> fieldName <*> (spaces *> char '=' *> spaces *> p_value)
                fieldName = (:) <$> letter <*> many fieldChar
                fieldChar = letter <|> digit <|> oneOf "-_"

p_comment :: CharParser () ()
p_comment = char '#' *> manyTill anyChar newline *> spaces *> pure () <?> "comment"

comments :: CharParser () [()]
comments = many p_comment

p_value :: CharParser () CValue
p_value = value <* spaces
  where value = CString <$> p_string
            <|> p_int_or_float
            <|> CBool <$> p_bool
            <|> CList <$> p_list
            <|> p_default
            <?> "entry value"

p_bool :: CharParser () Bool
p_bool = True <$ string "true"
     <|> False <$ string "false"

p_list :: CharParser () [CValue]
p_list = between (char '{' <* spaces) (char '}') $ (p_value <* spaces) `sepBy` (char ',' <* spaces)

p_default :: CharParser () CValue
p_default = CDefault <$ string "default"

-- TODO: negative numbers not support, is this needed?
p_int_or_float :: CharParser () CValue
p_int_or_float = pick <$> naturalOrFloat
    where
        pick (Left x)     = CInt x
        pick (Right x)    = CDouble x
        naturalOrFloat = P.naturalOrFloat lexer
        lexer          = P.makeTokenParser emptyDef

p_string :: CharParser () String
p_string = between (char '\"') (char '\"') (many ch)
    where ch = char '\\' *> p_escape <|> satisfy (`notElem` "\"\\")

p_escape :: CharParser () Char
p_escape = choice (zipWith decode "\\\"/" "\\\"/")
    where decode c r = r <$ char c

parseInput :: SourceName -> [Char] -> Either ParseError [CSection]
parseInput file input = parse p_configuration file input

parseConfigFile :: FilePath -> IO (Either ParseError [CSection])
parseConfigFile file = do
                        input <- readFile file
                        return $ parseInput file input
