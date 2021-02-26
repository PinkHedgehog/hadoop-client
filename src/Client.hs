{-# LANGUAGE OverloadedStrings #-}

module Client where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Network.Wreq
import qualified Data.Text as T (Text, pack)
import qualified Data.Text.IO as T (putStrLn)
import Data.Text.Encoding
import Data.List (last)
import Control.Lens ((&), (.~), (^.))
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V

data Context = Context 
             { url    :: String
             , port   :: String
             , user   :: String
             , curDir :: String
             , locDir :: String
             } deriving (Show, Eq, Read)

type Request = String

type Filename = String

data Operation = Operation Opcode Filename deriving (Show, Eq)
data Opcode = MKDIR | CD | LS | DEL | AP Filename | LLS | LCD | PUT | GET deriving (Show, Eq)


generateRequest :: Operation -> ReaderT Context IO ()
generateRequest (Operation opcode filename) = do
    context <- ask
    user' <- asks user
    --cur <- asks cur
    let reqs = runReader requestString context
        opts = requestOpts opcode & param "user.name" .~ [T.pack $ user context]
    --liftIO $ putStrLn . show $ reqs
    --liftIO $ putStrLn . show $ opts

    case opcode of
        LS -> do
            t <- liftIO $ getWith opts reqs
            liftIO $ mapM_ T.putStrLn $ processLS t
        _  -> return ()
    return ()

processLS :: Response B.ByteString -> [T.Text]
processLS t = map (\i -> f t i) [0..n]
    where
        n = -1 + (V.length $ t ^. responseBody . key "FileStatuses" . key "FileStatus" . _Array)
        f t i = t ^. responseBody . key "FileStatuses" . key "FileStatus" . nth i . key "pathSuffix" . _String

requestString :: Reader Context String
requestString = do
    url'  <- asks url
    port' <- asks port
    cur   <- asks curDir
    return $ "http://" ++ 
        url' ++ ":" ++ 
        port' ++ "/webhdfs/v1/" ++ path cur
    where
        path "" = ""
        path str | last str == '/' = str
                 | otherwise = str ++ "/"


requestOpts :: Opcode -> Options
requestOpts opcode = defaults & param "op" .~ [parse opcode]
    where
        parse MKDIR = "MKDIRS"
        parse LS    = "LISTSTATUS"
        parse PUT   = "CREATE"
        parse GET   = "OPEN"
        parse _     = ""






someFunc :: IO ()
someFunc = putStrLn "someFunc"
