module RegisteredUser where

newtype Username = Username String

newtype AccountNumber = AccountNumber Integer

data User = Unregistered User
          | RegisteredUser Username AccountNumber
