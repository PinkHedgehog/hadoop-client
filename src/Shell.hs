module Shell where

import Client
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import System.Directory
import Data.List (init)
import Data.Char (toUpper)
import qualified Data.Text as T
import System.IO (stdout, hFlush)

repl :: ReaderT Context IO ()
repl = do
    liftIO $ putStr "> " >> hFlush stdout
    command <- liftIO $ parse <$> getLine
    case command of
    
        Just (Operation CD filename) -> do
            cur' <- asks curDir
            context <- ask
            let newDir = T.unpack $ update (T.pack cur') (T.pack filename)
                reqs = runReader requestString context
                opts = requestOpts CD & param "user.name" .~ [T.pack $ user context]
            availableDirs <- liftIO $ 
                (map (T.unpack . snd) . filter (\(ftype, _) -> ftype == "DIRECTORY") . processLS) <$> 
                    getWith (requestOpts LS) reqs
            if filename `elem` availableDirs || filename == ".."
                then do
                    liftIO $ putStrLn $ "New remote directory: " ++ newDir
                    let context' = context {curDir = newDir}
                    withReaderT (const context') $ repl
                else do
                    liftIO $ putStrLn $ "No such directory: " ++ filename
                    return ()

        Just (Operation LCD filename) -> do
            cur' <- asks locDir
            context <- ask
            exist <- liftIO $ doesPathExist $ cur' ++ "/" ++ filename
            if exist
                then do
                    liftIO $ setCurrentDirectory $ cur' ++ "/" ++ filename
                    newDir <- liftIO getCurrentDirectory
                    liftIO $ putStrLn $ "New local directory: " ++ newDir
                    let context' = context {locDir = newDir}
                    withReaderT (const context') repl
                else return ()
        Just x -> performOperation x
        Nothing -> do
            liftIO $ putStrLn "Unknown operation"
    repl

update :: T.Text -> T.Text -> T.Text
update path filename | length subPaths == 2 && filename == ".." = path
                     | length subPaths > 2 && filename == ".." = T.intercalate "/" $ init $ subPaths
                     | otherwise = T.concat [path, "/", filename]
    where subPaths = T.splitOn "/" path

parse :: String -> Maybe Operation
parse str = fromTokens tokens
    where
        strToUpper = map toUpper
        tokens = words str
        fromTokens [] = Nothing
        fromTokens ["ls"] = Just $ Operation LS ""
        fromTokens ["lls"] = Just $ Operation LLS ""
        fromTokens [x] = Nothing
        fromTokens (opcode:filename:rest) 
            | elem '/' filename = Nothing
            | opcode == "append" && 
                (length rest < 1 || '/' `elem` head rest || length rest > 1) = Nothing
            | opcode == "append" = Just $ Operation (APPEND filename) $ head rest
            | length rest > 0 = Nothing
            | opcode `elem` ["cd", "lcd", "put", "get", "delete", "mkdir"] = 
                Just $ Operation (read . strToUpper $ opcode) filename
            | otherwise = Nothing
