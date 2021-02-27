module Shell where

import Client
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import System.Directory
import Data.List (init)
import Data.Char (toUpper)
import qualified Data.Text as T

repl :: ReaderT Context IO ()
repl = do
    liftIO $ putStr "> "
    command <- liftIO $ parse <$> getLine
    case command of
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
