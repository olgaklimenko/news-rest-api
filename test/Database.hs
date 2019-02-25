{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database where
import Queries.User (UserRepository(..))
import           Control.Monad.State
import           Control.Monad.IO.Class
import Models.User (User(..))
import Models.Author (Author(..))
import Models.Category (Category(..))
import Models.Tag (Tag(..))
import Models.News (News(..))


newtype MockDB e = MockDB {
    entities :: State e [e]
}

instance UserRepository MockDB where
    type Gateway MockDB = ()
    selectUser = undefined
    selectUserById = undefined
    insertUser = undefined
    updateUser = undefined
    deleteUser = undefined