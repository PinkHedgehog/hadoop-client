module Main where

import Client
import Shell
import Control.Monad.Trans.Reader
import System.Environment
import Control.Monad
import System.Exit
import System.IO
import System.Directory
import Control.Exception
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 3) $ do
        progName <- getProgName
        die $ "Usage: " ++ progName ++ " <url> <port> <username>"
    loc' <- getCurrentDirectory
    let username = args !! 2
        cur' = "user/" ++ username
        port' = args !! 1
        url' = args !! 0

    r <- try (get $ concat [ "http://"
                           , url'
                           , ":"
                           , port'
                           , "/webhdfs/v1/"
                           , cur'
                           , "?user.name="
                           , username
                           , "&op=LISTSTATUS"]) :: IO (Either SomeException  (Response B.ByteString))
    case r of
        Left e -> do
            let e' = T.pack . show $ e
            when (T.isSuffixOf "No route to host" e') $ die "Invalid url"
            when (T.isSuffixOf "Connection refused" e') $ die "Invalid port"
            when (T.isSuffixOf "java.io.FileNotFoundException" e') $
                die "No home directory for current user"
        Right _ -> do

            putStr =<< readFile "Help.txt"
            putStrLn ""
            let initialContext = Client.Context url' port' username cur' loc'
            hFlush stdout
            runReaderT repl initialContext

