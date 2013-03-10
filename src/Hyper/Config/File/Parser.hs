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
import           Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

p_configuration :: CharParser (String, Configuration) Configuration
p_configuration = do
        spaces *> comments *> many1 p_section
        (_,config) <- getState
        return config

p_section :: CharParser (String, Configuration) [()]
p_section = do
        section <- p_between '[' (many1 ch) ']'
        updateBoth $ updateSections section
        case section of
                "server" -> many p_server_entry
                _        -> many p_site_entry
        where
                p_between left parser right = between (char left <* spaces) (char right) (parser <* spaces)
                ch = satisfy (`notElem` "]\"\\")
                updateSections "server" state           = state
                updateSections section s@(site,config)  | section `elem` keys = s
                                                        | site `elem` keys = s
                                                        | otherwise = (site, config { configurationSites = M.insert section ds sites })
                                 where
                                        sites = configurationSites config
                                        ds = configurationDefaultSite config
                                        keys = M.keys sites

p_server_entry :: CharParser (String, Configuration) ()
p_server_entry = many serverSetting *> pure ()
        where serverSetting =
                    p_port
                <|> p_ssl_port
                <|> p_resource_per_req
                <|> p_server_index
                <|> p_server_passthrough
                <|> p_site
                <|> p_server_root
                <?> "server setting"
        
p_site_entry :: CharParser (String, Configuration) ()
p_site_entry = many serverSetting *> pure ()
        where serverSetting =
                    p_site_root
                <|> p_site_index
                <|> p_site_passthrough
                <?> "site settings" 

-- TODO: Add handling for default bare word
p_port :: CharParser (String, Configuration) ()
p_port = p_setting "port" (p_port' <|> p_port_list) *> pure ()
        where
             p_port' = do
                port <- p_int
                updateConfig $ \config -> config { configurationSinglePort = True, configurationPorts = [port] }
             p_port_list = do
                ports <- p_list p_int <?> "list of numbers"
                updateConfig $ \config -> config { configurationSinglePort = False, configurationPorts = ports }
                
p_ssl_port :: CharParser (String, Configuration) ()
p_ssl_port = p_setting "sslPort" p_ssl_port' *> pure ()
        where
                p_ssl_port' = do
                        port <- p_int
                        updateConfig $ \config -> config { configurationSSlPort = Just port }

p_resource_per_req :: CharParser (String, Configuration) ()
p_resource_per_req = p_setting "resourcePerRequest" p_resource_per_req' *> pure ()
        where
                p_resource_per_req' = do
                        rpr <- p_bool
                        updateConfig $ \config -> config { configurationResourcePerReq = rpr }

p_index :: CharParser (String, Configuration) String
p_index = p_setting "index" p_string

p_server_index :: CharParser (String, Configuration) ()
p_server_index = do
        idx <- p_index
        updateConfig $ \config -> config { configurationDefaultSite = configurationDefaultSite config `mappend` siteConfigurationIndex idx }
        return ()

-- TODO: Highlight this usage of pure in the blog entry part 2
p_passthrough :: CharParser (String, Configuration) [String]
p_passthrough = p_setting "passthrough" $ (pure <$> p_string) <|> p_list p_string

p_server_passthrough :: CharParser (String, Configuration) ()
p_server_passthrough = do
        ss <- p_passthrough
        updateConfig $ \config -> config { configurationDefaultSite = configurationDefaultSite config `mappend` siteConfigurationPassthrough ss }
        return ()

p_site :: CharParser (String, Configuration) ()
p_site = p_setting "site" p_site' *> pure ()
        where
                p_site' = do
                        s <- p_string
                        updateSite $ \_ -> s

p_root :: CharParser (String, Configuration) String
p_root = p_setting "root" p_string

p_server_root :: CharParser (String, Configuration) ()
p_server_root = do
        r <- p_root
        updateConfig $ \config -> config { configurationDefaultSite = configurationDefaultSite config `mappend` siteConfigurationRoot r }
        return ()

p_site_root :: CharParser (String, Configuration) ()
p_site_root = do
        r <- p_root
        updateBoth $ \(site,config) -> config { configurationSites = configurationSites config `mappend` m site r (configurationDefaultSite config) }
        return ()
                where
                        m s r c = M.singleton s . siteConfigurationRoot . r' r $ c
                        r' r c = root c ++ r

p_site_index :: CharParser (String, Configuration) ()
p_site_index = do
        idx <- p_index
        updateBoth $ \(site,config) -> config { configurationSites = configurationSites config `mappend` m site idx }
        return ()
                where
                        m s i = M.singleton s . siteConfigurationIndex $ i

p_site_passthrough :: CharParser (String, Configuration) ()
p_site_passthrough = do
        ss <- p_passthrough
        updateBoth $ \(site,config) -> config { configurationSites = configurationSites config `mappend` m site ss }
        return ()
                where
                        m s ss = M.singleton s . siteConfigurationPassthrough $ ss

p_comment :: CharParser (String, Configuration) ()
p_comment = char '#' *> manyTill anyChar newline *> spaces *> pure () <?> "comment"

comments :: CharParser (String, Configuration) [()]
comments = many p_comment

p_list :: CharParser (String, Configuration) a -> CharParser (String, Configuration) [a]
p_list p = between (char '{' <* spaces) (char '}') $ (p <* spaces) `sepBy` (char ',' <* spaces)

p_bool :: CharParser (String, Configuration) Bool
p_bool = True <$ string "true"
     <|> False <$ string "false"
     <?> "boolean"

p_int :: CharParser (String, Configuration) Int
p_int = read <$> many1 digit

p_string :: CharParser (String, Configuration) String
p_string = between (char '\"') (char '\"') (many ch) <?> "string"
    where ch = char '\\' *> p_escape <|> satisfy (`notElem` "\"\\")

p_escape :: CharParser (String, Configuration) Char
p_escape = choice (zipWith decode "\\\"/" "\\\"/")
    where decode c r = r <$ char c
    
-- helpers

p_setting :: String -> CharParser (String, Configuration) a -> CharParser (String, Configuration) a
p_setting field p = string field *> spaces *> char '=' *> spaces *> p

-- TODO: I think all of these are actually dropping the state and need to be fixed
updateSite f = do
        (site, config) <- getState
        return (f site, config)
        
updateConfig f = do
        (site, config) <- getState
        return (site, f config)

updateBoth f = do
        state <- getState
        return $ f state
        
-- Interface

parseInput :: SourceName -> [Char] -> Configuration -> Either ParseError Configuration
parseInput file input config = runParser p_configuration ("default", config) file input

parseConfigFile :: FilePath -> Configuration -> IO (Either ParseError Configuration)
parseConfigFile file config = do
                        input <- readFile file
                        return $ parseInput file input config
