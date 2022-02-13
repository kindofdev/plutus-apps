{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Ledger.Value
  ( module Export
  , noAdaValue
  , TokenNameM(..)
  , tokenNameM
  ) where

import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Value as Export

import Prelude qualified as Haskell

import Codec.Serialise.Class (Serialise)
import Control.DeepSeq (NFData)
import Data.ByteString qualified as BS
import Data.Hashable (Hashable)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as E
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Bytes (LedgerBytes (LedgerBytes))
import Plutus.V1.Ledger.Orphans ()
import PlutusTx qualified
import PlutusTx.Prelude as PlutusTx hiding (sort)

{-# INLINABLE noAdaValue #-}
-- | Value without any Ada.
noAdaValue :: Value -> Value
noAdaValue v = v - Ada.toValue (Ada.fromValue v)

-- | ByteString of a name of a token, shown as UTF-8 string when possible
newtype TokenNameM = TokenNameM { unTokenNameM :: PlutusTx.BuiltinByteString }
    deriving (Serialise) via LedgerBytes
    deriving stock (Generic)
    deriving newtype (Haskell.Eq, Haskell.Ord, Eq, Ord, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    deriving anyclass (Hashable, NFData)
    -- deriving Pretty via (PrettyShow TokenName)

instance IsString TokenNameM where
    {-# INLINABLE fromString #-}
    fromString = fromTextM . Text.pack

{-# INLINABLE tokenNameM #-}
-- | Creates `TokenName` from raw `ByteString`.
tokenNameM :: BS.ByteString -> TokenNameM
tokenNameM = TokenNameM . PlutusTx.toBuiltin

fromTextM :: Text -> TokenNameM
fromTextM = tokenNameM . E.encodeUtf8
