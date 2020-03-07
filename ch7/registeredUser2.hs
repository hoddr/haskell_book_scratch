module RegisteredUser2 where

newtype Username = Username String

newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

printUser :: User -> IO()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username un) (AccountNumber an)) =
  putStrLn $ un ++ " " ++ show an
