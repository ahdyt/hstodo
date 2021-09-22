module Lib
    ( someFunc
    ) where

import qualified Adapter.InMemory.Todo as A
import           ClassyPrelude
import           Control.Monad.Fail
import           Domain.Todo

type State = (TVar A.State)

newtype App a = App
    { unApp :: ReaderT State IO a
    } deriving (Functor, Applicative, Monad, MonadReader State, MonadIO, MonadFail)

someFunc :: IO ()
someFunc = do
    mState <- newTVarIO A.initialState
    run mState action

run :: State -> App a -> IO a
run state =
    flip runReaderT state
    . unApp

action :: App ()
action = do
    createTodo (mkTodo "coding" "coding haskell until bored." "new")
    putStrLn "Enter todo name: "
    tName <- getLine
    putStrLn "Enter todo description: "
    tDesc <- getLine
    createTodo (mkTodo tName tDesc "new")
    todos <- getTodos
    let prettyTodos = map (\i -> show (fst i) <> " - " <> (unpack . rawTodoName . name . snd $ i) <> " " <> (unpack . rawTodoTask . task . snd $ i)) todos
    mapM_ print $ reverse prettyTodos
    action

instance TodoRepo App where
    addTodo = A.addTodo
    findTodos = A.findTodos
    findTodoById = A.findTodoById
