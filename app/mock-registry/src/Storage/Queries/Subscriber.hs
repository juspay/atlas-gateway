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

Module      :  Storage.Queries.Subscriber
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Storage.Queries.Subscriber where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Domain.Subscriber
import Storage.Tabular.Subscriber

findByAll :: Transactionable m => Maybe Text -> Maybe Text -> Maybe Domain -> Maybe SubscriberType -> m [Subscriber]
findByAll mbKeyId mbSubId mbDomain mbSubType =
  Esq.findAll $ do
    parkingLocation <- from $ table @SubscriberT
    where_ $
      whenJust_ mbKeyId (\keyId -> parkingLocation ^. SubscriberUniqueKeyId ==. val keyId)
        &&. whenJust_ mbSubId (\subId -> parkingLocation ^. SubscriberSubscriberId ==. val subId)
        &&. whenJust_ mbDomain (\domain -> parkingLocation ^. SubscriberDomain ==. val domain)
        &&. whenJust_ mbSubType (\subType -> parkingLocation ^. SubscriberSubscriberType ==. val subType)
    return parkingLocation

create :: Subscriber -> SqlDB ()
create = create'

deleteByKey :: (Text, Text) -> SqlDB ()
deleteByKey = deleteByKey' @SubscriberT

findAll :: Transactionable m => m [Subscriber]
findAll =
  Esq.findAll $ do
    from $ table @SubscriberT
