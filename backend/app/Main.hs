module Main where

import Network.Wai.Handler.Warp (run)
import Web.Twain
import Network.HTTP.Types.Status (unauthorized401)
import Data.Aeson
import Data.Text (Text)

main :: IO ()
main = do
  run 8001 $
    foldr ($)
      (notFound missing)
      [ post "/login" login ]

data LoginParams = LoginParams
  { username :: Text
  , password :: Text
  }

instance FromJSON LoginParams where
  parseJSON = withObject "LoginParams" $ \ o -> LoginParams
    <$> o .: "username"
    <*> o .: "password"

data LoginResponse = LoginResponse
  { name :: Text
  , id :: Text
  }

instance ToJSON LoginResponse where
  toJSON (LoginResponse name id) = object ["name" .= name, "id" .= id]

login :: ResponderM a
login = do
  LoginParams username password <- fromBody
  if username == "robin" && password == "hello"
    then respond $ json $ LoginResponse "Robin-Lynn" "123"
    else respond $ status unauthorized401 $ html ""
  where respond = send . withHeader ("Access-Control-Allow-Origin", "*")

missing :: ResponderM a
missing = send $ html "Not found..."