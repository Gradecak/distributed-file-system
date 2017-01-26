{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Utils.InternalAuth where

import Servant

-- | requires an internal-token head to be set.
type ProtectInternal = Header "internal-token" String
