import Data.Time

-- Basic data types fun
data Group = Group
  { gid      :: Int
  , students :: [Student]
  }

data Student = Student
  { name     :: String
  , surname  :: String
  , birth    :: UTCTime
  }

-- Beyond Enum class
class SafeEnum a where
  ssucc :: a -> a
  spred :: a -> a
