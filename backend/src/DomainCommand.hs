module DomainCommand (DomainCommand (..)) where

import Data.Text (Text)

data DomainCommand
  = RegisterUser {userEmail :: Text}
  | BringMeSomeBeer