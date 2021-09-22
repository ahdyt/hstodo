module Domain.Todo
    (
    -- Types
      Todo(..)
    , TodoId
    , TodoName
    , TodoDesc
    , TodoStatus
    , rawTodoName
    , rawTodoDesc
    , mkTodoName
    , mkTodoTask
    , mkTodoStatus
    , mkTodo
    -- Ports
    , TodoRepo(..)
    -- Use Case
    , createTodo
    , getTodos
    , getTodoById
    ,
    ) where

import           ClassyPrelude

type TodoId = Int
newtype TodoName = TodoName { todoNameRaw :: Text } deriving (Show, Eq, Ord)
newtype TodoDesc = TodoDesc { todoDescRaw :: Text } deriving (Show, Eq, Ord)
data TodoStatus = New | Working | Done deriving (Show, Eq, Ord)

data Todo = Todo
    { name   :: TodoName
    , task   :: TodoDesc
    , status :: TodoStatus
    } deriving (Show, Eq, Ord)

rawTodoName :: TodoName -> Text
rawTodoName = todoNameRaw

rawTodoDesc :: TodoDesc -> Text
rawTodoDesc = todoDescRaw

mkTodoName :: Text -> TodoName
mkTodoName = TodoName

mkTodoTask :: Text -> TodoDesc
mkTodoTask = TodoDesc

mkTodoStatus :: Text -> TodoStatus
mkTodoStatus t =
    case t of
      "new"     -> New
      "working" -> Working
      "done"    -> Done
      _         -> New

mkTodo :: Text -> Text -> Text -> Todo
mkTodo tName tTask tStat = Todo (mkTodoName tName) (mkTodoTask tTask) (mkTodoStatus tStat)

class (Monad m) => TodoRepo m where
    addTodo :: Todo -> m ()
    findTodos :: m [(TodoId, Todo)]
    findTodoById :: TodoId -> m (Maybe Todo)

createTodo :: (TodoRepo m) => Todo -> m ()
createTodo = addTodo

getTodos :: (TodoRepo m) => m [(TodoId, Todo)]
getTodos = findTodos

getTodoById :: (TodoRepo m) => TodoId -> m (Maybe Todo)
getTodoById = findTodoById

