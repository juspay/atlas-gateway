let common = ./common.dhall
let sec = ./secrets/beckn-gateway.dhall

let rcfg =
  { connectHost = "localhost"
  , connectPort = 6579
  , connectAuth = None Text
  , connectDatabase = +0
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = None Integer
  }

let coreVersions =
  { mobility = "0.8.2"
  , localRetail = "0.9.1"
  , foodAndBeverage = "0.9.1"
  , logistics  = "0.9.3"
  }

in

{ redisCfg = rcfg
, port = +8015
, metricsPort = +9998
, selfId = "JUSPAY.BG.1"
, hostName = "localhost"
, nwAddress = "http://localhost:8015/v1/"  -- public address of a node
, authEntity =
  { signingKey = sec.signingKey
  , uniqueKeyId = "juspay-bg-1-key"
  , signatureExpiry = common.signatureExpiry
  }
, searchTimeout = None Integer
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/beckn-gateway.log"}
, coreVersions = coreVersions
, mobilityDomainVersion = "0.9.3"
, graceTerminationPeriod = +90
, httpClientOptions = common.httpClientOptions
, registryUrl = common.registryUrl
, registrySecrets = sec.registrySecrets
, disableSignatureAuth = False
}
