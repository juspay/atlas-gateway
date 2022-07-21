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

Module      :  App
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module App where

import App.Routes (registryAPI, registryFlow)
import App.Types
import Beckn.Exit (exitDBMigrationFailure)
import Beckn.Prelude
import Beckn.Storage.Esqueleto.Migration
import Beckn.Utils.App
import Beckn.Utils.Dhall (readDhallConfigDefault)
import Beckn.Utils.Servant.Server (runServerWithHealthCheck)
import Servant (Context (..))

runRegistryService :: (AppCfg -> AppCfg) -> IO ()
runRegistryService configModifier = do
  config <- readDhallConfigDefault "mock-registry" <&> configModifier
  appEnv <- buildAppEnv config
  runServerWithHealthCheck appEnv registryAPI registryFlow middleware identity EmptyContext releaseAppEnv $ \flowRt -> do
    migrateIfNeeded config.migrationPath config.autoMigrate config.esqDBCfg
      >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
    return flowRt
  where
    middleware = hashBodyForSignature
