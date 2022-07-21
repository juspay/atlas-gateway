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

module App.Types
  ( Env,
    FlowHandler,
    FlowServer,
    Flow,
    AppCfg (),
    AppEnv (..),
    buildAppEnv,
    releaseAppEnv,
  )
where

import Beckn.Storage.Esqueleto.Config
import Beckn.Tools.Metrics.CoreMetrics (CoreMetricsContainer, registerCoreMetricsContainer)
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Flow
import Beckn.Utils.App (getPodName)
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.IOLogging
import Beckn.Utils.Shutdown
import EulerHS.Prelude

data AppCfg = AppCfg
  { port :: Int,
    esqDBCfg :: EsqDBConfig,
    signatureExpiry :: Seconds,
    graceTerminationPeriod :: Seconds,
    loggerConfig :: LoggerConfig,
    autoMigrate :: Bool,
    migrationPath :: Maybe FilePath
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { port :: Int,
    signatureExpiry :: Seconds,
    graceTerminationPeriod :: Seconds,
    loggerConfig :: LoggerConfig,
    isShuttingDown :: Shutdown,
    coreMetrics :: CoreMetricsContainer,
    loggerEnv :: LoggerEnv,
    esqDBEnv :: EsqDBEnv
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  isShuttingDown <- mkShutdown
  coreMetrics <- registerCoreMetricsContainer
  hostname <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  return AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type Env = EnvR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv
