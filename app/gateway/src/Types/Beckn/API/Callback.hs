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

Module      :  Types.Beckn.API.Callback
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

module Types.Beckn.API.Callback where

import Beckn.Types.Core.Error
import Data.Aeson
import EulerHS.Prelude hiding ((.=))
import Types.Beckn.Context

-- Creating own gateway CallbackReq to support Context for 0.8 and 0.9

data CallbackReq a = CallbackReq
  { context :: Context,
    contents :: Either Error a
  }
  deriving (Generic, Show)

instance ToJSON a => ToJSON (CallbackReq a) where
  toJSON (CallbackReq context contents) = object allFields
    where
      contextField = "context" .= context
      allFields = case contents of
        Left err -> contextField : ["error" .= err]
        Right message -> contextField : ["message" .= message]

instance FromJSON a => FromJSON (CallbackReq a) where
  parseJSON = withObject "CallbackReq" $ \o ->
    CallbackReq
      <$> o .: "context"
      <*> (Left <$> o .: "error" <|> Right <$> o .: "message")
