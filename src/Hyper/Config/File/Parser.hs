module Hyper.Config.File.Parser
(
  parseConfigFile
, parseInput
)
where

import           Control.Applicative
import qualified Data.Map                      as M
import           Data.Monoid                   (mappend, mempty)
import           Hyper.Config.Types
import           Text.Parsec.Language          (emptyDef)
import qualified Text.Parsec.Token             as P
import           Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Debug.Trace(trace)

p_configuration :: Configuration -> CharParser () Configuration
p_configuration config = spaces *> comments *> p_section config

p_section :: Configuration -> CharParser () Configuration
p_section config = do
        section <- p_between '[' (many1 ch) ']'
        let config' = updateSections section
        config'' <- (spaces *> p_entries section config')
        p_section (trace "Looping section" config'') <|> return (trace "Returning config from p_section" config'')
                where
                        p_between left parser right = between (char left <* spaces) (char right) (parser <* spaces)
                        ch = satisfy (`notElem` "]\"\\")
                        sites = configurationSites config
                        ds = configurationDefaultSite config
                        keys = M.keys sites
                        updateSections "server" = config
                        updateSections section  | section `elem` keys = config
                                                | otherwise = if specialSiteName section `elem` keys
                                                              then config
                                                              else
                                                                   let m = M.insert section ds sites
                                                                   in config { configurationSites = m }


p_entries :: String -> Configuration -> CharParser () Configuration
p_entries section config = spaces *> comments *> p_entries' section config <?> "entries"

-- TODO: It's bad enough that this fails but the second problem is that I'm hand checking fields.  I should be making parsers for that.  Maybe that would let me drop this crazy recursive thing that isn't working
p_entries' :: String -> Configuration -> CharParser () Configuration
p_entries' section config = do
        name <- trace "Reading field name now" fieldName
        (trace (name ++ " pealing spaces off now") spaces *> char '=' *> spaces)
        config' <- case (section, name) of
                ("server", "port")                      -> trace (name ++ " Reading port now") $ p_port <|> p_port_list
                ("server", "sslPort")                   -> trace (name ++ " Reading ssl port now") $ p_ssl_port
                ("server", "resourcePerRequest")        -> trace (name ++ " Reading resource per now") $ p_resource_per_req
                ("server", "index")                     -> trace (name ++ " Reading index now") $ p_index
                ("server", "passthrough")               -> trace (name ++ " Reading passthrough now") $ p_passthrough <|> p_passthrough_list
                ("server", "site")                      -> trace (name ++ " Reading site now") $ p_site
                ("server", "root")                      -> trace (name ++ " Reading root now") $ p_root
                (site, "root")                          -> trace (name ++ " Reading site root now") $ p_site_root site
                (site, "index")                         -> trace (name ++ " Reading site index now") $ p_site_index site
                (site, "passthrough")                   -> trace (name ++ " Reading site passthrough now") $ p_passthrough_site site <|> p_passthrough_site_list site
                (site, field)                           -> trace (name ++ " Failing with unexpected now") $ unexpected $ "field " ++ field ++ " in section " ++ site
        p_entries' (trace ("************Looping entries " ++ name) section) config' <|> return (trace "Return from entries config" config')
                where
                        defaultSite = configurationDefaultSite config
                        p_port = do
                                port <- p_int
                                return config { configurationSinglePort = True, configurationPorts = [port] }
                        p_list p = between (char '{' <* spaces) (char '}') $ (p <* spaces) `sepBy` (char ',' <* spaces)
                        p_port_list = do
                                ports <- p_list p_int <?> "list of numbers"
                                return config { configurationSinglePort = False, configurationPorts = ports }
                        p_ssl_port = do
                                port <- p_int
                                return config { configurationSSlPort = Just port }
                        p_resource_per_req = do
                                bool <- p_bool
                                return config { configurationResourcePerReq = bool }
                        p_index = do
                                i <- p_string
                                return config { configurationDefaultSite = defaultSite `mappend` SiteConfiguration mempty i mempty mempty }
                        p_passthrough = do
                                s <- p_string
                                return config { configurationDefaultSite = defaultSite `mappend` SiteConfiguration mempty mempty [s] mempty }
                        p_passthrough_list = do
                                ss <- p_list p_string <?> "list of strings"
                                return config { configurationDefaultSite = defaultSite `mappend` SiteConfiguration mempty mempty ss mempty }
                        p_site = do
                                site <- p_string
                                let e = M.singleton (specialSiteName site) $ SiteConfiguration mempty mempty mempty mempty
                                    sites = configurationSites config
                                return config { configurationSites = sites `mappend` e }
                        p_root = do
                                r <- p_string
                                return config { configurationDefaultSite = defaultSite `mappend` SiteConfiguration r mempty mempty mempty }
                        p_site_root site = do
                                r <- p_string
                                let
                                    r' = root defaultSite ++ r
                                    e = M.singleton (deriveKey site) $ SiteConfiguration r' mempty mempty mempty
                                    in return config { configurationSites = configurationSites config `mappend` e }
                        p_site_index site = do
                                i <- p_string
                                let e = M.singleton (deriveKey site) $ SiteConfiguration mempty i mempty mempty
                                return config { configurationSites = configurationSites config `mappend` e }
                        p_passthrough_site site = do
                                s <- p_string
                                let e = M.singleton (deriveKey site) $ SiteConfiguration mempty mempty [s] mempty
                                return config { configurationSites = configurationSites config `mappend` e }
                        p_passthrough_site_list site = do
                                ss <- p_list p_string
                                let e = M.singleton (deriveKey site) $ SiteConfiguration mempty mempty ss mempty
                                return config { configurationSites = configurationSites config `mappend` e }
                        deriveKey site = let
                                keys = M.keys $ configurationSites config
                                ssn = specialSiteName site
                                in if ssn `elem` keys then ssn else site
                        fieldName = (:) <$> letter <*> many fieldChar
                        fieldChar = letter <|> digit <|> oneOf "-_"

p_comment :: CharParser () ()
p_comment = char '#' *> manyTill anyChar newline *> spaces *> pure () <?> "comment"

comments :: CharParser () [()]
comments = many p_comment

p_bool :: CharParser () Bool
p_bool = True <$ string "true"
     <|> False <$ string "false"
     <?> "boolean"

p_int :: CharParser () Int
p_int = pick <$> naturalOrFloat
        where
                pick (Left x) = fromIntegral x
                pick (Right x) = truncate x
                naturalOrFloat = P.naturalOrFloat lexer
                lexer          = P.makeTokenParser emptyDef

p_string :: CharParser () String
p_string = between (char '\"') (char '\"') (many ch) <?> "string"
    where ch = char '\\' *> p_escape <|> satisfy (`notElem` "\"\\")

p_escape :: CharParser () Char
p_escape = choice (zipWith decode "\\\"/" "\\\"/")
    where decode c r = r <$ char c

specialSiteName :: String -> String
specialSiteName site = "DEFAULT{" ++ site ++ "}"

parseInput :: SourceName -> [Char] -> Configuration -> Either ParseError Configuration
parseInput file input config = parse (p_configuration config) file input

parseConfigFile :: FilePath -> Configuration -> IO (Either ParseError Configuration)
parseConfigFile file config = do
                        input <- readFile file
                        return $ parseInput file input config
