module MessageDb
  ( -- * Units of measure

    -- ** Microseconds
    Microseconds.Microseconds (..)
  , Microseconds.microsecondsFromNatural

    -- ** Number of messages
  , NumberOfMessages.NumberOfMessages (..)
  , NumberOfMessages.numberOfMessageFromNatural

    -- * Message
  , Message.Message (..)
  , Message.UntypedMessage

    -- ** Parsing
  , Message.parseMessagePayload
  , Message.parseMessageMetadata
  , Message.ParseMessageError (..)
  , Message.parseMessage

    -- ** Created at
  , CreatedAt.CreatedAt (..)
  , CreatedAt.createdAtFromUTCTime

    -- ** Global position
  , GlobalPosition.GlobalPosition (..)
  , GlobalPosition.globalPositionFromNatural

    -- ** Message ID
  , MessageId.MessageId (..)
  , MessageId.newMessageId
  , MessageId.messageIdFromUUID

    -- ** Message type
  , MessageType.MessageType (..)
  , MessageType.messageTypeFromText

    -- ** Metadata
  , Metadata.Metadata (..)
  , Metadata.nullMetadata
  , Metadata.metadataFromValue
  , Metadata.toMetadata
  , Metadata.fromMetadata

    -- ** Payload
  , Payload.Payload (..)
  , Payload.nullPayload
  , Payload.payloadFromValue
  , Payload.toPayload
  , Payload.fromPayload

    -- ** Stream name
  , StreamName.StreamName (..)
  , StreamName.streamNameFromText
  , StreamName.streamCategory
  , StreamName.streamIdentifier
  , StreamName.addIdentifier
  , StreamName.addMaybeIdentifier

    -- *** Category
  , Category.Category
  , Category.categoryToText
  , Category.categoryFromText

    -- *** Identifier
  , Identifier.Identifier (..)
  , Identifier.identifierFromText

    -- ** Stream position
  , StreamPosition.StreamPosition (..)
  , StreamPosition.streamPositionFromNatural

    -- * Stream version
  , StreamVersion.StreamVersion (..)

    -- * Producing messages
  , Produce.produce
  , Produce.produceWithId

    -- ** Produce Record
  , ProduceRecord.ProduceRecord (..)
  , ProduceRecord.produceRecord

    -- ** Exceptions
  , UnexpectedStreamVersion.UnexpectedStreamVersion (..)

    -- * Consuming messages
  , BatchSize.BatchSize (..)
  , Condition.Condition (..)
  , HandlerError.HandlerError (..)

    -- ** Lookup a message
  , Functions.lookupById
  , Functions.lookupByPosition

    -- ** Projecting a stream of messages
  , Projection.Projection (..)
  , Projection.Projected (..)
  , Projection.emptyProjection
  , Projection.project

    -- *** Handlers
  , ProjectionHandlers.ProjectionHandlers
  , ProjectionHandlers.ProjectionHandler
  , ProjectionHandlers.emptyProjectionHandlers
  , ProjectionHandlers.addProjectionHandler
  , ProjectionHandlers.addProjectionHandler_
  , ProjectionHandlers.addProjectionHandlerAny
  , ProjectionHandlers.addProjectionHandlerAny_

    -- *** Error
  , ProjectionError.ProjectionError (..)

    -- *** Snapshots
  , Snapshots.Snapshots (..)
  , Snapshots.noSnapshots
  , Snapshots.messageSnapshots

    -- *** Fetching
  , Fetch.FetchParams (..)
  , Fetch.fetchParams
  , Fetch.fetch

    -- ** Subscribing to a category
  , Subscription.Subscription (..)
  , Subscription.subscribe
  , Subscription.startSubscription

    -- *** Consumer group
  , ConsumerGroup.ConsumerGroup (..)
  , ConsumerGroup.singleConsumer
  , ConsumerGroup.isSingleConsumer
  , ConsumerIndex.ConsumerIndex (..)
  , ConsumerIndex.consumerIndexFromNatural
  , ConsumerGroupSize.ConsumerGroupSize (..)
  , ConsumerGroupSize.consumerGroupSizeFromNatural

    -- *** Correlation
  , Correlation.Correlation (..)
  , Correlation.correlationFromText

    -- *** Dead letter queue
  , DeadLetterQueue.writeToDlq

    -- *** Handlers
  , SubscriptionHandlers.SubscriptionHandler
  , SubscriptionHandlers.SubscriptionHandlers
  , SubscriptionHandlers.emptySubscriptionHandlers
  , SubscriptionHandlers.addSubscriptionHandler
  , SubscriptionHandlers.addSubscriptionHandler_
  , SubscriptionHandlers.addSubscriptionHandlerAny
  , SubscriptionHandlers.addSubscriptionHandlerAny_

    -- *** Error
  , SubscriptionErrorReason.SubscriptionErrorReason (..)
  , SubscriptionError.SubscriptionError (..)
  )
where

import qualified MessageDb.Units.Microseconds as Microseconds
import qualified MessageDb.Units.NumberOfMessages as NumberOfMessages

import qualified MessageDb.Message as Message
import qualified MessageDb.Message.CreatedAt as CreatedAt
import qualified MessageDb.Message.GlobalPosition as GlobalPosition
import qualified MessageDb.Message.MessageId as MessageId
import qualified MessageDb.Message.MessageType as MessageType
import qualified MessageDb.Message.Metadata as Metadata
import qualified MessageDb.Message.Payload as Payload
import qualified MessageDb.Message.StreamName as StreamName
import qualified MessageDb.Message.StreamName.Category as Category
import qualified MessageDb.Message.StreamName.Identifier as Identifier
import qualified MessageDb.Message.StreamPosition as StreamPosition

import qualified MessageDb.StreamVersion as StreamVersion

import qualified MessageDb.Produce as Produce
import qualified MessageDb.Produce.Record as ProduceRecord
import qualified MessageDb.Produce.UnexpectedStreamVersion as UnexpectedStreamVersion

import qualified MessageDb.Functions as Functions

import qualified MessageDb.Consumer.BatchSize as BatchSize
import qualified MessageDb.Consumer.Condition as Condition
import qualified MessageDb.Consumer.HandlerError as HandlerError

import qualified MessageDb.Consumer.Projection as Projection
import qualified MessageDb.Consumer.Projection.Error as ProjectionError
import qualified MessageDb.Consumer.Projection.Handlers as ProjectionHandlers

import qualified MessageDb.Consumer.Fetch as Fetch
import qualified MessageDb.Consumer.Snapshots as Snapshots

import qualified MessageDb.Consumer.DeadLetterQueue as DeadLetterQueue

import qualified MessageDb.Consumer.Subscription as Subscription
import qualified MessageDb.Consumer.Subscription.ConsumerGroup as ConsumerGroup
import qualified MessageDb.Consumer.Subscription.ConsumerGroupSize as ConsumerGroupSize
import qualified MessageDb.Consumer.Subscription.ConsumerIndex as ConsumerIndex
import qualified MessageDb.Consumer.Subscription.Correlation as Correlation
import qualified MessageDb.Consumer.Subscription.Error as SubscriptionError
import qualified MessageDb.Consumer.Subscription.ErrorReason as SubscriptionErrorReason
import qualified MessageDb.Consumer.Subscription.Handlers as SubscriptionHandlers

