{-# LANGUAGE OverloadedStrings #-}

module Client
    ( module Client
    , module Network.Wreq
    , module Data.Aeson.Lens
    , module Control.Lens
    ) where


import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Network.Wreq
import qualified Data.Text as T (Text, pack, splitOn, concat, unpack, intercalate)
import qualified Data.Text.IO as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.List (last, sort, init)
import Control.Lens hiding (Context)
import Data.Aeson.Lens
import System.Directory
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
data Opcode = MKDIR | CD | LS | DELETE | APPEND Filename | LLS | LCD | PUT | GET deriving (Show, Eq, Read)

performOperation :: Operation -> ReaderT Context IO ()
performOperation (Operation opcode filename) = do
    context <- ask
    user' <- asks user
    cur' <- asks curDir
    url' <- asks url
    loc' <- asks locDir
    let reqs = runReader requestString context
        opts = requestOpts opcode & param "user.name" .~ [T.pack $ user context]

    case opcode of
        MKDIR -> do
            liftIO $ do
                r <- putWith opts (reqs ++ filename) ("" :: B.ByteString)
                case r ^. responseBody ^? members . _Bool of
                    Just x  -> putStrLn $ "Directory " ++ filename ++ " created: " ++ show x
                    Nothing -> putStrLn "Something went wrong..."

        LS -> do
            liftIO $ putStrLn $ "Hdfs directory: " ++ cur'
            liftIO $ putStrLn $ "----------------" ++ replicate (length cur') '-'
            t <- liftIO $ getWith opts reqs
            liftIO $ mapM_ (\(ft, fn) -> T.putStr (expand ft) >> T.putStrLn fn ) $ processLS t

        GET -> do
            availableFiles <- liftIO $ 
                map (T.unpack . snd) . filter (\(ftype, _) -> ftype == "FILE") . processLS <$> 
                    getWith (requestOpts LS) reqs
            if filename `elem` availableFiles then do
                let newOpts = opts & param "noredirect" .~ ["true"]
                t <- liftIO $ getWith newOpts (reqs ++ filename)
                let location = t ^. responseBody . key "Location" . _String
                    [protocol, mid, footer] = T.splitOn "localhost" location
                    finalAddr = T.concat [protocol, T.pack url', mid, "localhost", footer]
                liftIO $ do
                    r <- get $ T.unpack finalAddr
                    if r ^. responseStatus . statusCode == 200
                        then do
                            B.writeFile filename $ r ^. responseBody
                            putStrLn $ "File " ++ filename ++ " downloaded!"
                        else putStrLn "Something went wrong..."
                else liftIO $ putStrLn $ filename ++ ": no such file in current directory"

        PUT -> do
            let newOpts = opts & param "noredirect" .~ ["true"] 
                               & param "overwrite" .~ ["true"]
            file <- liftIO $ B.readFile filename
            let part = partLBS "file" file :: Part
            t <- liftIO $ putWith newOpts (reqs ++ filename) part
            liftIO $ print t
            let location = t ^. responseBody . key "Location" . _String
                [protocol, mid, footer] = T.splitOn "localhost" location
                finalAddr = T.concat [protocol, T.pack url', mid, "localhost", footer]
            liftIO $ do
                r <- put (T.unpack finalAddr) file
                putStrLn "File uploaded!"

        LLS -> liftIO $ do
            exist <- doesDirectoryExist $ loc'
            if exist
                then do
                    putStrLn $ "Local directory: " ++ loc'
                    putStrLn $ "-----------------" ++ replicate (length loc') '-'
                    fnames <- sort <$> listDirectory loc'
                    ftypes <- mapM doesFileExist fnames
                    zipWithM_ (\ftype fname -> if ftype 
                        then putStr "FIL " >> putStrLn fname
                        else putStr "DIR " >> putStrLn fname) ftypes fnames
                else putStrLn "No such directory..."

        DELETE -> do
            liftIO $ do
                r <- deleteWith opts (reqs ++ filename)
                case r ^. responseBody ^? members . _Bool of
                    Just x  -> putStrLn $ "File deleted: " ++ show x
                    Nothing -> putStrLn "Something went wrong..."

        APPEND filenameLoc -> do
            let newOpts = opts & param "noredirect" .~ ["true"]
            file <- liftIO $ B.readFile filenameLoc
            let part = partLBS "file" file :: Part
            t <- liftIO $ postWith newOpts (reqs ++ filename) part
            liftIO $ print t
            let location = t ^. responseBody . key "Location" . _String
                [protocol, mid, footer] = T.splitOn "localhost" location
                finalAddr = T.concat [protocol, T.pack url', mid, "localhost", footer]
            liftIO $ do
                r <- post (T.unpack finalAddr) file
                putStrLn "File appended!"


        _  -> return ()
    return ()

processLS :: Response B.ByteString -> [(T.Text, T.Text)]
processLS t = zip filetypes filenames
    where 
        filenames = t ^. responseBody ^.. members . members . values . key "pathSuffix" . _String
        filetypes = t ^. responseBody ^.. members . members . values . key "type" . _String

expand :: T.Text -> T.Text
expand text | text == "FILE"      = "FIL "
            | text == "DIRECTORY" = "DIR "
            | otherwise           = "    "

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
        parse MKDIR      = "MKDIRS"
        parse LS         = "LISTSTATUS"
        parse PUT        = "CREATE"
        parse GET        = "OPEN"
        parse DELETE     = "DELETE"
        parse (APPEND _) = "APPEND"
        parse _          = ""






someFunc :: IO ()
someFunc = putStrLn "someFunc"
