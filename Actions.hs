{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Actions
  (
   add
  ,remove
  ,view
  ,todoDone
  ,update
  ) where 

import System.IO
import System.Posix.Files
import System.Posix.Types
import System.Directory
import System.Environment
import Data.List
import Control.DeepSeq
import GHC.IO
import GHC.Generics
import qualified Data.Char as Char
data Todo = Todo
  {
    todoId :: Int
  , title :: String
  , done :: Bool
  } deriving (Show, Read, Eq,Generic, NFData)

type TodoList = [Todo]

data Filter = Done | Pending deriving (Show, Read)

parseContents :: String -> TodoList
parseContents contents =
  if contents == ""
     then []
     else read contents :: TodoList
                       
add :: [String] -> IO ()
add [title] = do
 (todoList, fileName) <- extractTodoList
 let nextId = if todoList == []
       then 1
       else (todoId $ last todoList) + 1
     todo = Todo { todoId = nextId
                  , title = title
                  , done = False }
     newTodoList = todoList ++ [todo]
     message = "\nTODO Added: \n\n" ++ (showStrTodo todo) ++ "\n"

 (todoList,fileName) `deepseq` writeFile fileName $ show newTodoList
 
 putStrLn message
 
remove :: [String] -> IO ()
remove [numberString] = do
  (todoList, fileName) <- extractTodoList
  let todoId' = read numberString :: Int
      maybeTodo = getTodoById todoId' todoList
  case maybeTodo of
    Just todo -> do
      let newTodoList = delete todo todoList
          message = "\n\nTODO Removed:\n\n" ++ showStrTodo todo ++ "\n"
      (todoList, fileName) `deepseq` writeFile fileName $ show newTodoList
      putStrLn message
    Nothing -> putStrLn "TODO not found!"
parseList :: TodoList -> String
parseList [] = "Oops, no todos added yet \nUse todos add 'some task'"
parseList todoList =
  foldl (\t1 t2 -> t1 ++ "\n" ++ (showStrTodo t2)) "" todoList

filterDone :: TodoList -> TodoList
filterDone todoList =
  filter (\t -> done t == True) todoList

capitalize :: String -> String
capitalize str = Char.toUpper (head str) : tail str

toUpper :: String -> String
toUpper str = map (\c -> Char.toUpper c) str

filterPending :: TodoList -> TodoList
filterPending todoList =
  filter (\t -> done t == False) todoList
view :: [String] -> IO ()
view [] = do
  homeDir <- getHomeDirectory
  let fileName = homeDir ++ "/.todos"
  contents <- readFile fileName
  let todoList = parseContents contents
      header = "\n" ++ "TODO - Showing Todo List \n\n"
  putStr $ header ++ (parseList todoList) ++ "\n\n"
view [filterStr] = do
  (todoList, _) <- extractTodoList
  let todoFilter = read (capitalize filterStr) :: Filter
      filteredTodoList = case todoFilter of
        Done -> filterDone todoList
        Pending -> filterPending todoList
      header = "\nTODO - Showing " ++ (show todoFilter) ++ "\n\n"
      contents = header ++ (parseList filteredTodoList) ++ "\n\n"
  putStrLn contents

getTodoById :: Int -> TodoList -> Maybe Todo
getTodoById todoId' =
  find (\t -> todoId t == todoId')

updateInTodoList :: Todo -> TodoList -> TodoList
updateInTodoList todo =
  map (\t -> if todoId t == todoId todo
                then todo
                else t)
update :: [String] -> IO ()
update [] = do putStrLn "Tell me the todo ID and the new title"
update [todoId'] = do putStrLn "Tell me the new title"
update [todoIdStr, newTitle] = do
  (todoList, fileName) <- extractTodoList
  let maybeTodo = getTodoById (read todoIdStr :: Int) todoList
  case maybeTodo of
    Just todo -> do
      let newTodo = todo { title = newTitle }
          newTodoList = updateInTodoList newTodo todoList
          message = "\nTodo updated:\n\n" ++ (showStrTodo newTodo) ++ "\n"
      writeFile fileName $ show newTodoList
      putStrLn message
    Nothing -> do putStrLn "\nTODO Not found\n"
      
  
      
      

showStrTodo :: Todo -> String
showStrTodo todo =
  let todoId' = show $ todoId todo
      title' = title todo
      done' = if done todo then "Done" else "Pending"
  in  todoId' ++ " - " ++ title' ++ " [ " ++ done' ++ " ]"

extractTodoList :: IO (TodoList, FilePath)
extractTodoList = do
  homeDir <- getHomeDirectory
  let fileName = homeDir ++ "/.todos"
  contents <- readFile fileName 
  if contents == ""
    then return ([], fileName)
    else return (read contents :: TodoList, fileName)

markAsDone :: Int -> TodoList -> String -> IO ()
markAsDone todoId' todoList fileName = do
  let newTodoList = map
                    (\t -> if todoId' == todoId t
                              then t { done = True }
                              else t
                    ) todoList
  writeFile fileName $ show newTodoList
  
todoDone :: [String] -> IO ()
todoDone [] = do putStr "You need to tell me the Todo ID"
todoDone [todoIdStr] = do
  (todoList, fileName) <- extractTodoList

  let maybeTodo = find (\t -> Just (todoId t) == Just (read todoIdStr :: Int)) todoList
      todoId' = case maybeTodo of
                  Just todo -> todoId todo
                  Nothing -> 0
  if todoId' == 0 then putStrLn "Todo not found"
    else markAsDone todoId' todoList fileName
                           
  
  

