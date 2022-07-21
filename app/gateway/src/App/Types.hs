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

Module      :  App.Types
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module App.Types where

import Beckn.Types.App
import Beckn.Types.Cache
import Beckn.Types.Common hiding (id)
import Beckn.Types.Flow
import Beckn.Types.Registry
import Beckn.Utils.CacheRedis as Cache
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import qualified Beckn.Utils.Registry as Registry
import Beckn.Utils.Servant.Client (HttpClientOptions)
import Beckn.Utils.Servant.SignatureAuth
import qualified Data.Cache as C
import qualified Data.Text as T
import EulerHS.Prelude
import qualified EulerHS.Types as T
import System.Environment (lookupEnv)
import Tools.Metrics

data AppCfg = AppCfg
  { redisCfg :: T.RedisConfig,
    port :: Int,
    metricsPort :: Int,
    selfId :: Text,
    hostName :: Text,
    nwAddress :: BaseUrl,
    authEntity :: AuthenticatingEntity',
    loggerConfig :: LoggerConfig,
    searchTimeout :: Maybe Seconds,
    coreVersions :: CoreVersions,
    mobilityDomainVersion :: Text,
    graceTerminationPeriod :: Seconds,
    httpClientOptions :: HttpClientOptions,
    registryUrl :: BaseUrl,
    disableSignatureAuth :: Bool
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { redisCfg :: T.RedisConfig,
    hostName :: Text,
    nwAddress :: BaseUrl,
    authEntity :: AuthenticatingEntity',
    searchTimeout :: Maybe Seconds,
    coreVersions :: CoreVersions,
    mobilityDomainVersion :: Text,
    httpClientOptions :: HttpClientOptions,
    registryUrl :: BaseUrl,
    disableSignatureAuth :: Bool,
    gwId :: Text, -- why we can't use selfId?
    cache :: C.Cache Text Text,
    isShuttingDown :: TMVar (),
    coreMetrics :: CoreMetricsContainer,
    loggerEnv :: LoggerEnv
  }
  deriving (Generic)

data CoreVersions = CoreVersions
  { mobility :: Text,
    logistics :: Text,
    localRetail :: Text,
    foodAndBeverage :: Text
  }
  deriving (Generic, FromDhall)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  hostname <- map T.pack <$> lookupEnv "POD_NAME"
  cache <- C.newCache Nothing
  isShuttingDown <- newEmptyTMVarIO
  coreMetrics <- registerCoreMetricsContainer
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  return $
    AppEnv
      { gwId = selfId,
        ..
      }

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer r api = FlowServerR AppEnv api

type Flow = FlowR AppEnv

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.authEntity.signingKey)
  getSignatureExpiry = (.authEntity.signatureExpiry)

instance Registry Flow where
  registryLookup = Registry.withSubscriberCache Registry.registryLookup

instance Cache Subscriber Flow where
  type CacheKey Subscriber = SimpleLookupRequest
  getKey = Cache.getKey "gateway:registry" . lookupRequestToRedisKey
  setKey = Cache.setKey "gateway:registry" . lookupRequestToRedisKey
  delKey = Cache.delKey "gateway:registry" . lookupRequestToRedisKey

instance CacheEx Subscriber Flow where
  setKeyEx ttl = Cache.setKeyEx "gateway:registry" ttl . lookupRequestToRedisKey
