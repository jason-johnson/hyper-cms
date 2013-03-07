module Hyper.Config.File.Parser
(
  parseConfigFile
, parseInput
)
where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

data CValue = CString String
            | CInt Integer
            | CDouble Double
            | CBool Bool
            | CDefault
              deriving (Eq, Ord, Show)
                          
data CEntry = CEntry String CValue deriving (Show)

data CSection = CSection String [CEntry] deriving (Show)

p_configuration :: CharParser () [CSection]
p_configuration = spaces *> many1 p_section
            
p_between :: Char -> CharParser () a -> Char -> CharParser () a
p_between left parser right =
    between (char left <* spaces) (char right) (parser <* spaces)
            
p_section :: CharParser () (CSection)
p_section = CSection <$> p_between '[' (many1 ch) ']' <*> (spaces *> p_entries) <?> "section"
        where ch = satisfy (`notElem` "]\"\\")

p_entries :: CharParser () [CEntry]
p_entries = many (optional p_comment *> entry) <?> "entries"
        where
                entry = CEntry <$> fieldName <*> (spaces *> char '=' *> spaces *> p_value)
                fieldName = (:) <$> letter <*> many fieldChar
                fieldChar = letter <|> digit <|> oneOf "-_"

p_comment :: CharParser () ()                
p_comment = spaces *> char '#' *> manyTill anyChar newline *> spaces *> pure ()
    
p_value :: CharParser () CValue
p_value = value <* spaces
  where value = CString <$> p_string
            <|> p_int_or_float
            <|> CBool <$> p_bool
            <|> p_default
            <?> "entry value"

p_bool :: CharParser () Bool
p_bool = True <$ string "true"
     <|> False <$ string "false"

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