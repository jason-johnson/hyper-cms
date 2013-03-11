module Hyper.Config.File.Parser
(
  parseConfigFile
, parseInput
)
where

import           Control.Applicative
import qualified Data.Map                      as M
import           Data.Monoid                   (mappend)
import           Hyper.Config.Types
import           Text.Parsec hiding (many, optional, (<|>))

p_configuration :: Parsec [Char] (String, Configuration) Configuration
p_configuration = spaces *> comments *> many1 p_section *> config
            where
                config = do
                    (_,c) <- getState
                    return c

p_section :: Parsec [Char] (String, Configuration) [()]
p_section = do
        section <- p_between '[' (many1 ch) ']'
        updateBoth $ updateSections section
        case section of
                "server" -> many p_server_entry
                _        -> many p_site_entry
        where
                p_between left parser right = between (char left <* spaces) (char right) (parser <* spaces)
                ch = satisfy (`notElem` "]\"\\")                                    -- TODO: This should actually be "server", "default" or a web address
                updateSections "server" state           = state
                updateSections section s@(site,config)  | section `elem` keys = s
                                                        | site `elem` keys = s
                                                        | otherwise = (site, config { configurationSites = M.insert section ds sites })
                                 where
                                        sites = configurationSites config
                                        ds = configurationDefaultSite config
                                        keys = M.keys sites

p_server_entry :: Parsec [Char] (String, Configuration) ()
p_server_entry = many settings *> pure ()
        where settings =
                    p_port
                <|> p_ssl_port
                <|> p_resource_per_req
                <|> p_site
                <|> p_server_root               -- TODO: The path strings can actually be checked for validity, or maybe not since most things are allowed in unix.  Check the lexer module if there is a lexer for this
                <|> p_server_index
                <|> p_server_passthrough        -- TODO: Cache directory is missing and is an option only for server
                <?> "server setting"

p_site_entry :: Parsec [Char] (String, Configuration) ()
p_site_entry = many settings *> pure ()
        where settings =
                    p_site_root
                <|> p_site_index
                <|> p_site_passthrough
                <?> "site settings"

-- TODO: Add handling for default bare word
p_port :: Parsec [Char] (String, Configuration) ()
p_port = p_setting "port" p_port' *> pure ()
        where
             p_port' = do
                ports <- (pure <$> p_int) <|> p_list p_int <?> "port[s]"
                updateConfig $ \config -> config { configurationSinglePort = True, configurationPorts = ports }

p_ssl_port :: Parsec [Char] (String, Configuration) ()
p_ssl_port = p_setting "sslPort" p_ssl_port' *> pure ()
        where
                p_ssl_port' = do
                        port <- p_int
                        updateConfig $ \config -> config { configurationSSlPort = Just port }

p_resource_per_req :: Parsec [Char] (String, Configuration) ()
p_resource_per_req = p_setting "resourcePerRequest" p_resource_per_req' *> pure ()
        where
                p_resource_per_req' = do
                        rpr <- p_bool
                        updateConfig $ \config -> config { configurationResourcePerReq = rpr }

p_server_index :: Parsec [Char] (String, Configuration) ()
p_server_index = do
        idx <- p_index
        updateConfig $ \config -> config { configurationDefaultSite = configurationDefaultSite config `mappend` siteConfigurationIndex idx }
        return ()

p_server_passthrough :: Parsec [Char] (String, Configuration) ()
p_server_passthrough = do
        ss <- p_passthrough
        updateConfig $ \config -> config { configurationDefaultSite = configurationDefaultSite config `mappend` siteConfigurationPassthrough ss }
        return ()

p_site :: Parsec [Char] (String, Configuration) ()
p_site = p_setting "site" p_site' *> pure ()
        where
                p_site' = do
                        s <- p_string
                        updateSite $ \_ -> s

p_root :: Parsec [Char] (String, Configuration) String
p_root = p_setting "root" p_string

p_server_root :: Parsec [Char] (String, Configuration) ()
p_server_root = do
        r <- p_root
        updateConfig $ \config -> config { configurationDefaultSite = configurationDefaultSite config `mappend` siteConfigurationRoot r }
        return ()

p_site_root :: Parsec [Char] (String, Configuration) ()
p_site_root = do
        r <- p_root
        updateBoth $ \(site,config) -> (site, config { configurationSites = configurationSites config `mappend` m site r (configurationDefaultSite config) })
        return ()
                where
                        m s r c = M.singleton s . siteConfigurationRoot . r' r $ c
                        r' r c = root c ++ r

p_site_index :: Parsec [Char] (String, Configuration) ()
p_site_index = do
        idx <- p_index
        updateBoth $ \(site,config) -> (site, config { configurationSites = configurationSites config `mappend` m site idx })
        return ()
                where
                        m s i = M.singleton s . siteConfigurationIndex $ i

p_site_passthrough :: Parsec [Char] (String, Configuration) ()
p_site_passthrough = do
        ss <- p_passthrough
        updateBoth $ \(site,config) -> (site, config { configurationSites = configurationSites config `mappend` m site ss })
        return ()
                where
                        m s ss = M.singleton s . siteConfigurationPassthrough $ ss

p_comment :: Parsec [Char] (String, Configuration) ()
p_comment = char '#' *> manyTill anyChar newline *> spaces *> pure () <?> "comment"

comments :: Parsec [Char] (String, Configuration) [()]
comments = many p_comment

p_list :: Parsec [Char] (String, Configuration) a -> Parsec [Char] (String, Configuration) [a]
p_list p = between (char '{' <* spaces) (char '}') $ (p <* spaces) `sepBy` (char ',' <* spaces)

p_bool :: Parsec [Char] (String, Configuration) Bool
p_bool = True <$ string "true"
     <|> False <$ string "false"
     <?> "boolean"

p_int :: Parsec [Char] (String, Configuration) Int
p_int = read <$> many1 digit

p_string :: Parsec [Char] (String, Configuration) String
p_string = between (char '\"') (char '\"') (many ch) <?> "string"
    where ch = char '\\' *> p_escape <|> satisfy (`notElem` "\"\\")

p_escape :: Parsec [Char] (String, Configuration) Char
p_escape = choice (zipWith decode "\\\"/" "\\\"/")
    where decode c r = r <$ char c

-- helpers

p_setting :: String -> Parsec [Char] (String, Configuration) a -> Parsec [Char] (String, Configuration) a
p_setting field p = string field *> spaces *> char '=' *> spaces *> p

p_index :: Parsec [Char] (String, Configuration) String
p_index = p_setting "index" p_string

-- TODO: Highlight this usage of pure in the blog entry part 2
p_passthrough :: Parsec [Char] (String, Configuration) [String]
p_passthrough = p_setting "passthrough" $ (pure <$> p_string) <|> p_list p_string

updateSite :: (String -> String) -> Parsec [Char] (String, Configuration) ()
updateSite f = do
        (site, config) <- getState
        putState (f site, config)
        return ()

updateConfig :: (Configuration -> Configuration) -> Parsec [Char] (String, Configuration) ()
updateConfig f = do
        (site, config) <- getState
        putState (site, f config)
        return ()

updateBoth :: ((String, Configuration) -> (String, Configuration)) -> Parsec [Char] (String, Configuration) ()
updateBoth f = do
        state <- getState
        putState $ f state
        return ()

-- Interface

parseInput :: SourceName -> [Char] -> Configuration -> Either ParseError Configuration
parseInput file input config = runParser p_configuration ("default", config) file input

parseConfigFile :: FilePath -> Configuration -> IO (Either ParseError Configuration)
parseConfigFile file config = do
                        input <- readFile file
                        return $ parseInput file input config
