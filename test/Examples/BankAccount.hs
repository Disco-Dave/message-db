module Examples.BankAccount
  ( Money (..),
    Open (..),
    Opened (..),
    Close (..),
    Closed (..),
    Deposit (..),
    Deposited (..),
    Withdraw (..),
    Withdrawn (..),
    projection,
  )
where

import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import qualified MessageDb.Message as Message
import MessageDb.Projection (Projection (Projection, handlers, initial))
import qualified MessageDb.Projection.Handlers as ProjectionHandlers
import MessageDb.TypedMessage (TypedMessage (TypedMessage))
import qualified MessageDb.TypedMessage as TypedMessage


-- * Primitive types used throughout these events and commands


newtype Money = Money
  { fromMoney :: Double
  }
  deriving (Show, Eq, Ord, Num, Generic)
instance Aeson.ToJSON Money
instance Aeson.FromJSON Money


newtype AccountId = AccountId
  { fromAccountId :: UUID
  }
  deriving (Show, Eq, Ord, Generic)
instance Aeson.ToJSON AccountId
instance Aeson.FromJSON AccountId


-- * Events and commands. The commands are present tense, and events are past tense.


newtype Open = Open
  { initialDeposit :: Money
  }
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON Open
instance Aeson.FromJSON Open


newtype Opened = Opened
  { openedBalance :: Money
  }
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON Opened
instance Aeson.FromJSON Opened


data Close = Close
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON Close
instance Aeson.FromJSON Close


data Closed = Closed
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON Closed
instance Aeson.FromJSON Closed


newtype Deposit = Deposit
  { depositAmount :: Money
  }
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON Deposit
instance Aeson.FromJSON Deposit


newtype Deposited = Deposited
  { depositedAmount :: Money
  }
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON Deposited
instance Aeson.FromJSON Deposited


newtype Withdraw = Withdraw
  { withdrawAmount :: Money
  }
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON Withdraw
instance Aeson.FromJSON Withdraw


newtype Withdrawn = Withdrawn
  { withdrawnAmount :: Money
  }
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON Withdrawn
instance Aeson.FromJSON Withdrawn


newtype WithdrawRejected = WithdrawRejected
  { rejectedWithdrawAmount :: Money
  }
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON WithdrawRejected
instance Aeson.FromJSON WithdrawRejected


-- * Projection stuff


data Overdraft = Overdraft
  { overdraftAmount :: Money
  , overdraftTime :: UTCTime
  }
  deriving (Show, Eq)


-- The final state
data BankAccount = BankAccount
  { balance :: Money
  , isOpened :: Bool
  , overdrafts :: [Overdraft]
  }
  deriving (Show, Eq)


initialAccount :: BankAccount
initialAccount =
  BankAccount
    { balance = 0
    , isOpened = False
    , overdrafts = []
    }


handleOpened :: Opened -> BankAccount -> BankAccount
handleOpened payload bankAccount =
  bankAccount
    { isOpened = True
    , balance = openedBalance payload
    }


handleClosed :: Closed -> BankAccount -> BankAccount
handleClosed _ bankAccount =
  bankAccount
    { isOpened = False
    }


handleDeposited :: Deposited -> BankAccount -> BankAccount
handleDeposited payload bankAccount =
  bankAccount
    { balance = balance bankAccount + depositedAmount payload
    }


handleWithdrawn :: Withdrawn -> BankAccount -> BankAccount
handleWithdrawn payload bankAccount =
  bankAccount
    { balance = balance bankAccount - withdrawnAmount payload
    }


handleWithdrawRejected :: TypedMessage WithdrawRejected (Maybe Message.Metadata) -> BankAccount -> BankAccount
handleWithdrawRejected TypedMessage{payload, createdAtTimestamp} bankAccount =
  let overdraft =
        Overdraft
          { overdraftAmount = rejectedWithdrawAmount payload
          , overdraftTime = coerce createdAtTimestamp
          }
   in bankAccount
        { overdrafts = overdraft : overdrafts bankAccount
        }


projection :: Projection BankAccount
projection =
  let handlers =
        ProjectionHandlers.empty
          & ProjectionHandlers.attach "Opened" handleOpened
          & ProjectionHandlers.attach "Closed" handleClosed
          & ProjectionHandlers.attach "Deposited" handleDeposited
          & ProjectionHandlers.attach "Withdrawn" handleWithdrawn
          & ProjectionHandlers.attachMessage "WithdrawRejected" handleWithdrawRejected
   in Projection
        { initial = initialAccount
        , handlers = handlers
        }
