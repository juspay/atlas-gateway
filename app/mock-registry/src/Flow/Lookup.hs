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

Module      :  Flow.Lookup
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Flow.Lookup where

import App.Types (FlowHandler)
import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Core.Ack
import Beckn.Types.Registry.API (LookupRequest, LookupResponse)
import Beckn.Utils.Error (withFlowHandlerAPI)
import Domain.Subscriber
import Storage.Queries.Subscriber as Sub

lookup :: LookupRequest -> FlowHandler LookupResponse
lookup req = withFlowHandlerAPI $ do
  findByAll req.unique_key_id req.subscriber_id req.domain req._type

create :: Subscriber -> FlowHandler AckResponse
create sub = withFlowHandlerAPI $ do
  runTransaction $ Sub.create sub
  return Ack

delete :: Text -> Text -> FlowHandler AckResponse
delete uniqueKeyId subscriberId = withFlowHandlerAPI $ do
  runTransaction $ Sub.deleteByKey (uniqueKeyId, subscriberId)
  return Ack
