{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Types and constants to describe the HTTP version.
--
-- There are no parsing functions, as the formats are fairly different
-- per version of HTTP. And seeing as there are only a handful of versions,
-- it is easier to manually parse the version ad hoc.
--
-- For example, when you are expecting HTTP v1, try to match @HTTP/1.1@ or
-- @HTTP/1.0@. If you're expecting HTTP v2 or v3, you'd be using ALPN tokens,
-- which would be @http/1.1@, @h2@ or @h3@.
module Network.HTTP.Types.Version (
    HttpVersion (
        -- DO NOT use "..," as GHC 7.10.3 doesn't parse that
        HttpVersion,
        httpMajor,
        httpMinor,
        Http09,
        Http10,
        Http11,
        Http20,
        Http30
    ),
    http09,
    http10,
    http11,
    http20,
    http30,
) where

import Data.Data (Data)
import GHC.Generics (Generic)

-- | HTTP Version.
--
-- Note that the 'Show' instance is intended merely for debugging.
data HttpVersion = HttpVersion
    { httpMajor :: !Int
    , httpMinor :: !Int
    }
    deriving
        ( Eq
        , Ord
        , -- | @since 0.12.4
          Data
        , -- | @since 0.12.4
          Generic
        )

-- | >>> show http11
-- "HTTP/1.1"
-- >>> show http20
-- "HTTP/2.0"
--
-- This should not be used to render the HTTP version, as different versions
-- have different ways of rendering (i.e. HTTP v2 uses @"h2"@ instead of @"HTTP/2.0"@)
instance Show HttpVersion where
    show (HttpVersion major minor) = "HTTP/" ++ show major ++ "." ++ show minor

-- | HTTP 0.9
http09 :: HttpVersion
http09 = HttpVersion 0 9

-- | HTTP 1.0
http10 :: HttpVersion
http10 = HttpVersion 1 0

-- | HTTP 1.1
http11 :: HttpVersion
http11 = HttpVersion 1 1

-- | HTTP 2.0
--
-- @since 0.10
http20 :: HttpVersion
http20 = HttpVersion 2 0

-- | HTTP 3.0
--
-- @since 0.12.5
http30 :: HttpVersion
http30 = HttpVersion 3 0

----------------------
-- Pattern Synonyms --
----------------------

-- DO NOT put these on one line with commas, as GHC 7.10.3 doesn't parse that
pattern Http09 :: HttpVersion
pattern Http10 :: HttpVersion
pattern Http11 :: HttpVersion
pattern Http20 :: HttpVersion
pattern Http30 :: HttpVersion

-- | @since 0.12.6
pattern Http09 = HttpVersion 0 9

-- | @since 0.12.6
pattern Http10 = HttpVersion 1 0

-- | @since 0.12.6
pattern Http11 = HttpVersion 1 1

-- | @since 0.12.6
pattern Http20 = HttpVersion 2 0

-- | @since 0.12.6
pattern Http30 = HttpVersion 3 0
