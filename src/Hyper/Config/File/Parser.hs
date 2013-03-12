module Hyper.Config.File.Parser
(
  parseConfigFile
, parseInput
)
where

import           Control.Applicative
import qualified Data.Map                      as M
import           Text.Parsec hiding (many, optional, (<|>))
import           Data.Monoid         (Monoid, mappend)

import           Hyper.Config.Types
import           Hyper.Constants (defaultHTTPPort, defaultSSLPort)

data ServerSetting  =  Port [Int]
                    | SSLPort Int
                    | ResourcePerReq Bool
                    | Site String
                    | ServerRoot FilePath
                    | ServerIndex String
                    | ServerPassthrough [String]
                      deriving (Show)

data SiteSetting    = SiteRoot FilePath
                    | SiteIndex String
                    | SitePassthrough [String]
                      deriving (Show)

data Section    = ServerSection [ServerSetting]
                | SiteSection String [SiteSetting]
                  deriving (Show)

p_configuration :: Parsec [Char] (String, String, Configuration) ([Section], Configuration)
p_configuration = do
            sections <- spaces *> comments *> many1 p_section <* eof
            (_, _, config) <- getState
            return (sections, config)

p_section :: Parsec [Char] (String, String, Configuration) Section
p_section = do
        section <- p_between '[' (many1 ch) ']' <?> "section statement"
        updateAll $ updateSections (t section)
        updateSite $ \_ -> section
        case section of
                "server" -> ServerSection <$> p_server_entry
                _  -> SiteSection section <$> p_site_entry
        where
                p_between left parser right = between (char left <* spaces) (char right) (parser <* spaces)
                ch = satisfy (`notElem` "]\"\\")                                    -- TODO: This should actually be "server", "default" or a web address
                updateSections "server" state           = state
                updateSections section s@(dsite, site,config)   | section `M.member` sites = s
                                                                | otherwise = (dsite, site, config { configurationSites = M.insert section ds sites })
                                 where
                                        sites = configurationSites config
                                        ds = configurationDefaultSite config


p_server_entry :: Parsec [Char] (String, String, Configuration) [ServerSetting]
p_server_entry = spaces *> comments *> many (settings <* spaces <* comments)
        where settings =
                    try (Port <$> p_port)
                <|> try (SSLPort <$> p_ssl_port)
                <|> try (ResourcePerReq <$> p_resource_per_req)
                <|> Site <$> p_site
                <|> ServerRoot <$> p_server_root               -- TODO: The path strings can actually be checked for validity, or maybe not since most things are allowed in unix.  Check the lexer module if there is a lexer for this
                <|> ServerIndex <$> p_server_index
                <|> ServerPassthrough <$> p_server_passthrough        -- TODO: Cache directory is missing and is an option only for server
                <?> "server setting"

p_site_entry :: Parsec [Char] (String, String, Configuration) [SiteSetting]
p_site_entry = spaces *> comments *> many (settings <* spaces <* comments)
        where settings =
                    SiteRoot <$> p_site_root
                <|> SiteIndex <$> p_site_index
                <|> SitePassthrough <$> p_site_passthrough
                <?> "site settings"

-- TODO: Add handling for default bare word
p_port :: Parsec [Char] (String, String, Configuration) [Int]
p_port = p_setting "port" p_port' <?> "port"
        where
             p_port' = do
                ports <- (pure <$> p_int) <|> (pure <$> p_default_int defaultHTTPPort) <|> p_list p_int <?> "port[s]|default"
                updateConfig $ \config -> config { configurationSinglePort = length ports == 1, configurationPorts = ports }
                return ports

p_ssl_port :: Parsec [Char] (String, String, Configuration) Int
p_ssl_port = p_setting "sslPort" p_ssl_port' <?> "ssl port"
        where
                p_ssl_port' = do
                        port <- p_int <|> p_default_int defaultSSLPort
                        updateConfig $ \config -> config { configurationSSlPort = Just port }
                        return port

p_resource_per_req :: Parsec [Char] (String, String, Configuration) Bool
p_resource_per_req = p_setting "resourcePerRequest" p_resource_per_req'
        where
                p_resource_per_req' = do
                        rpr <- p_bool
                        updateConfig $ \config -> config { configurationResourcePerReq = rpr }
                        return rpr

p_server_root :: Parsec [Char] (String, String, Configuration) FilePath
p_server_root = do
        r <- p_root
        updateConfig $ \config -> config { configurationDefaultSite = configurationDefaultSite config `mappend` siteConfigurationRoot r }
        return r

p_server_index :: Parsec [Char] (String, String, Configuration) String
p_server_index = do
        idx <- p_index
        updateConfig $ \config -> config { configurationDefaultSite = configurationDefaultSite config `mappend` siteConfigurationIndex idx }
        return idx

p_server_passthrough :: Parsec [Char] (String, String, Configuration) [String]
p_server_passthrough = do
        ss <- p_passthrough
        updateConfig $ \config -> config { configurationDefaultSite = configurationDefaultSite config `mappend` siteConfigurationPassthrough ss }
        return ss

p_site :: Parsec [Char] (String, String, Configuration) String
p_site = p_setting "site" p_site'
        where
                p_site' = do
                        s <- p_string
                        updateDefaultSite $ \_ -> s
                        return s

p_site_root :: Parsec [Char] (String, String, Configuration) FilePath
p_site_root = do
        r <- p_root
        updateAll $ \(dsite, site,config) -> (dsite, site, config { configurationSites = mmergeMap site (siteConfigurationRoot . r' r . configurationDefaultSite $ config) $ configurationSites config })
        return r
                where
                        r' r c = root c ++ r

p_site_index :: Parsec [Char] (String, String, Configuration) String
p_site_index = do
        idx <- p_index
        updateAll $ \(dsite, site,config) -> (dsite, site, config { configurationSites = mmergeMap site (siteConfigurationIndex idx) $ configurationSites config })
        return idx

p_site_passthrough :: Parsec [Char] (String, String, Configuration) [String]
p_site_passthrough = do
        ss <- p_passthrough
        updateAll $ \(dsite, site,config) -> (dsite, site, config { configurationSites = mmergeMap site (siteConfigurationPassthrough ss) $ configurationSites config })
        return ss

p_comment :: Parsec [Char] (String, String, Configuration) ()
p_comment = char '#' *> manyTill anyChar newline *> spaces *> pure () <?> "comment"

comments :: Parsec [Char] (String, String, Configuration) [()]
comments = many p_comment

p_list :: Parsec [Char] (String, String, Configuration) a -> Parsec [Char] (String, String, Configuration) [a]
p_list p = between (char '{' <* spaces) (char '}') $ (p <* spaces) `sepBy` (char ',' <* spaces)

p_bool :: Parsec [Char] (String, String, Configuration) Bool
p_bool = True <$ string "true"
     <|> False <$ string "false"
     <?> "boolean"

p_int :: Parsec [Char] (String, String, Configuration) Int
p_int = read <$> many1 digit

p_default_int :: Int -> Parsec [Char] (String, String, Configuration) Int
p_default_int i = string "default" *> pure i

p_string :: Parsec [Char] (String, String, Configuration) String
p_string = between (char '\"') (char '\"') (many ch) <?> "string"
    where ch = char '\\' *> p_escape <|> satisfy (`notElem` "\"\\")

p_escape :: Parsec [Char] (String, String, Configuration) Char
p_escape = choice (zipWith decode "\\\"/" "\\\"/")
    where decode c r = r <$ char c

-- helpers

p_setting :: String -> Parsec [Char] (String, String, Configuration) a -> Parsec [Char] (String, String, Configuration) a
p_setting field p = string field *> spaces *> char '=' *> spaces *> p

p_root :: Parsec [Char] (String, String, Configuration) String
p_root = p_setting "root" p_string

p_index :: Parsec [Char] (String, String, Configuration) String
p_index = p_setting "index" p_string

-- TODO: Highlight this usage of pure in the blog entry part 2
p_passthrough :: Parsec [Char] (String, String, Configuration) [String]
p_passthrough = p_setting "passthrough" $ (pure <$> p_string) <|> p_list p_string

updateDefaultSite :: (String -> String) -> Parsec [Char] (String, String, Configuration) ()
updateDefaultSite f = do
        (defaultSite, site, config) <- getState
        putState (f defaultSite, site, config)
        return ()

updateSite :: (String -> String) -> Parsec [Char] (String, String, Configuration) ()
updateSite f = do
        (defaultSite, site, config) <- getState
        putState (defaultSite, f site, config)
        return ()

updateConfig :: (Configuration -> Configuration) -> Parsec [Char] (String, String, Configuration) ()
updateConfig f = do
        (defaultSite, site, config) <- getState
        putState (defaultSite, site, f config)
        return ()

updateAll :: ((String, String, Configuration) -> (String, String, Configuration)) -> Parsec [Char] (String, String, Configuration) ()
updateAll f = do
        state <- getState
        putState $ f state
        return ()

mmergeMap :: (Ord k, Monoid a) => k -> a -> M.Map k a -> M.Map k a
mmergeMap = M.insertWith (flip mappend)

-- Interface

parseInput :: SourceName -> [Char] -> Configuration -> Either ParseError ([Section], Configuration)
parseInput file input config = runParser p_configuration ("default", "", config) file input

parseConfigFile :: FilePath -> Configuration -> IO (Either ParseError ([Section], Configuration))
parseConfigFile file config = do
                        input <- readFile file
                        return $ parseInput file input config
