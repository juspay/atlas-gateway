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

Module      :  Product.ProviderRegistry

Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Product.ProviderRegistry
  ( lookup,
  )
where

import Beckn.Types.Common
import Beckn.Types.Registry as Registry
import qualified Beckn.Types.Registry.API as Registry
import qualified Beckn.Types.Registry.Domain as Registry
import qualified Beckn.Utils.Registry as Registry
import EulerHS.Prelude
import Tools.Metrics
import qualified Types.Beckn.Context as B
import qualified Types.Beckn.Domain as B

lookup ::
  ( MonadReader r m,
    MonadFlow m,
    Registry m,
    CoreMetrics m,
    HasField "registryUrl" r BaseUrl
  ) =>
  B.Context ->
  m [Registry.Subscriber]
lookup context = do
  case context.domain of
    B.MOBILITY -> listDomainProviders Registry.MOBILITY
    B.LOCAL_RETAIL -> listDomainProviders Registry.LOCAL_RETAIL
    B.FOOD_AND_BEVERAGE -> listDomainProviders Registry.FOOD_AND_BEVERAGE
    B.HEALTHCARE -> listDomainProviders Registry.HEALTHCARE
    B.METRO -> listDomainProviders Registry.METRO
    B.PARKING -> listDomainProviders Registry.PARKING
    B.LOGISTICS -> listDomainProviders Registry.LOGISTICS
    B.PUBLIC_TRANSPORT -> listDomainProviders Registry.PUBLIC_TRANSPORT
  where
    listDomainProviders domain =
      Registry.registryFetch
        Registry.emptyLookupRequest{_type = Just Registry.BPP,
                                    domain = Just domain
                                   }
