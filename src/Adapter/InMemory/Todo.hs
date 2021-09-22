module Adapter.InMemory.Todo where

import           ClassyPrelude
import           Data.Has
import           Domain.Todo   as D

data State = State
  { stateTodos         :: [(D.TodoId, D.Todo)]
  , stateTodoIdCounter :: Int
  } deriving (Show, Eq, Ord)

initialState :: State
initialState = State [] 0

type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

addTodo :: (InMemory r m) => D.Todo -> m ()
addTodo todo = do
    tvar <- asks getter
    atomically $ do
        state <- readTVar tvar
        let todos = stateTodos state
            newTodoId = stateTodoIdCounter state + 1
            newTodos = (newTodoId, todo) : todos
            newState = state
                { stateTodos = newTodos
                , stateTodoIdCounter = newTodoId
                }
        writeTVar tvar newState
        return ()

findTodos :: (InMemory r m) => m [(D.TodoId, D.Todo)]
findTodos = do
    tvar <- asks getter
    atomically $ do
        state <- readTVar tvar
        let todos = stateTodos state
        return todos

findTodoById :: (InMemory r m) => D.TodoId -> m (Maybe D.Todo)
findTodoById tId = do
   tvar <- asks getter
   state <- liftIO $ readTVarIO tvar
   let mayTodo = map snd . find ((tId ==) . fst) $ stateTodos state
   return mayTodo

