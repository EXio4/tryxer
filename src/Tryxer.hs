module Tryxer (tryxer,Config,makeConfig) where

import Control.Exception
import           IRC
import           IRC.Commands
import           System.IO
import           Control.Applicative
import           Data.Function
import           Data.Functor
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.Maybe
import qualified Data.Text as T
import           Control.Lens
import           Data.Text (Text)
import qualified Data.Text.Read as T.R
import qualified Data.Text.Encoding as T.E
import qualified Data.Text.ICU as Regex
import           Data.Aeson
import qualified System.Random as Rand
import qualified Network.Wreq as W
import           Network.Wreq (FormParam((:=)))

data Config = Config {
     cfg_api_key     :: !Text
    ,cfg_prefix      :: !Text
    ,cfg_admins      :: !([Regex.Regex])
}

makeConfig :: Text -> Text -> [Text] -> Config
makeConfig apikey prefix admins = Config apikey prefix (map (Regex.regex []) admins)

data Loop = Loop | Quit
til :: Monad m => m Loop -> m ()
til k = fix $ \r -> k >>= \case Loop -> r
                                Quit -> return ()

admin :: Config -> Text -> Bool
admin (Config{cfg_admins=regexes}) xs = any (\x -> isJust (Regex.find x xs)) regexes


data YandexResponse = YandexResponse {
       yr_lang :: Text
      ,yr_text :: [Text]
}

instance FromJSON YandexResponse where
    parseJSON (Object v) = YandexResponse <$> v .: "lang" <*> v .: "text"
    parseJSON _ = fail "yandex_response"

translate_io :: Config -> Text -> Text -> IO Text
translate_io cfg lang text = do
      r <- try $ W.asJSON =<< W.post "https://translate.yandex.net/api/v1.5/tr.json/translate"
                               ["key"  := cfg_api_key cfg
                               ,"lang" := lang
                               ,"text" := text]
 
      return $ case  r of
          Left (e :: SomeException) -> "Error"
          Right yr -> T.intercalate ", " (yr_text (yr  ^. W.responseBody))

translate :: Config -> Channel -> Text -> Text -> IRC IO ()
translate cfg ch lang text = do
      r <- liftIO (translate_io cfg lang text)
      privmsg ch (Message r)

tryxer :: Config -> IRC IO ()
tryxer cfg = til loop where
    _cmd c = fmap (T.dropWhile (== ' ')) . T.stripPrefix (pref <> c)
    _cmds cs txt = foldr (\c r -> _cmd c txt <|> r) Nothing cs
    pref = cfg_prefix cfg
    loop = irc_read >>= \msg -> do
            case msg of
              CHMSG (User nick _ host) channel (Message msg)
                  | Just (lang : (T.unwords -> txt)) <- T.words <$> _cmds ["translate","tra","tr"] msg
                  -> Loop <$ translate cfg channel lang txt
                  | Just _ <- _cmd "ping" msg
                  -> Loop <$ privmsg channel "pong!"
                  | Just _  <- _cmd "quit" msg
                  , admin cfg host
                  -> Quit <$ cmd "QUIT" ["bai"]
              _ -> return Loop



