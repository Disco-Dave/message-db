module MessageDb.Projection.FailureStrategy where

import MessageDb.Handlers (HandleError)
import MessageDb.Message (Message)


newtype FailureStrategy entity = FailureStrategy
  { processFailure :: Message -> HandleError -> entity -> entity
  }
