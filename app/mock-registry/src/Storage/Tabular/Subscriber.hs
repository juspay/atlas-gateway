{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}


{- |
Copyright 2022 Juspay Technologies Pvt Ltd

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Module      :  Storage.Tabular.Subscriber
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Tabular.Subscriber where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Base64
import qualified Beckn.Types.Registry.Subscriber as Domain
import qualified Domain.Subscriber as Domain

derivePersistField "Domain.Domain"

derivePersistField "Domain.SubscriberStatus"

derivePersistField "Domain.SubscriberType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SubscriberT sql=subscriber
      uniqueKeyId Text
      subscriberId Text
      subscriberUrl Text
      subscriberType Domain.SubscriberType sql=type
      domain Domain.Domain
      city Text Maybe
      country Text Maybe
      signingPublicKey Base64
      encrPublicKey Base64 Maybe
      validFrom UTCTime Maybe
      validUntil UTCTime Maybe
      status Domain.SubscriberStatus Maybe
      created UTCTime
      updated UTCTime

      Primary uniqueKeyId subscriberId
      deriving Generic
    |]

instance TEntityKey SubscriberT where
  type DomainKey SubscriberT = (Text, Text)
  fromKey (SubscriberTKey keyId subId) = (keyId, subId)
  toKey (keyId, subId) = SubscriberTKey keyId subId

instance TEntity SubscriberT Domain.Subscriber where
  fromTEntity entity = do
    let SubscriberT {..} = entityVal entity
    subscriberUrl_ <- parseBaseUrl subscriberUrl
    return $
      Domain.Subscriber
        { unique_key_id = uniqueKeyId,
          subscriber_id = subscriberId,
          subscriber_url = subscriberUrl_,
          signing_public_key = signingPublicKey,
          encr_public_key = encrPublicKey,
          valid_from = validFrom,
          valid_until = validUntil,
          _type = subscriberType,
          ..
        }
  toTType Domain.Subscriber {..} = do
    SubscriberT
      { uniqueKeyId = unique_key_id,
        subscriberId = subscriber_id,
        subscriberUrl = showBaseUrl subscriber_url,
        signingPublicKey = signing_public_key,
        encrPublicKey = encr_public_key,
        validFrom = valid_from,
        validUntil = valid_until,
        subscriberType = _type,
        ..
      }
  toTEntity a = do
    Entity (toKey (a.unique_key_id, a.subscriber_id)) $ toTType a
