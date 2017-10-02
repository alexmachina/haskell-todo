import System.Environment
import Actions
import System.IO
import System.Directory

type Todo = String
dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
           , ("remove", remove)
           , ("view", view)
           , ("done", todoDone)
           , ("update", update)]
main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  createTodosFileIfNotExists
  action args

createTodosFileIfNotExists :: IO ()
createTodosFileIfNotExists = do
  homeDir <- getHomeDirectory
  let fileName = homeDir ++ "/.todos"
  fileExist <- doesFileExist fileName
  if fileExist
    then return ()
    else do
      handle <- openFile fileName WriteMode
      hPutStr handle ""
      hClose handle

