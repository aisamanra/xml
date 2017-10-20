--------------------------------------------------------------------
-- |
-- Module    : Text.XML.Light.Types
-- Copyright : (c) Galois, Inc. 2007
-- License   : BSD3
--
-- Maintainer: Iavor S. Diatchki <diatchki@galois.com>
-- Stability : provisional
-- Portability:
--
-- Basic XML types.
--

{-# LANGUAGE DeriveDataTypeable #-}
module Text.XML.Light.Types where

import Data.Typeable(Typeable)
import Data.Data(Data)
import Data.Text(Text)

-- | A line is an Integer
type Line     = Integer

-- | XML content
data Content  = Elem Element
              | Text CData
              | CRef Text
                deriving (Show, Typeable, Data)

-- | XML elements
data Element  = Element {
                  elName      :: QName,
                  elAttribs   :: [Attr],
                  elContent   :: [Content],
                  elLine      :: Maybe Line
                } deriving (Show, Typeable, Data)

-- | XML attributes
data Attr     = Attr {
                  attrKey :: QName,
                  attrVal :: Text
                } deriving (Eq, Ord, Show, Typeable, Data)

-- | XML CData
data CData    = CData {
                  cdVerbatim  :: CDataKind,
                  cdData      :: Text,
                  cdLine      :: Maybe Line
                } deriving (Show, Typeable, Data)

data CDataKind
 = CDataText      -- ^ Ordinary character data; pretty printer escapes &, < etc.
 | CDataVerbatim  -- ^ Unescaped character data; pretty printer embeds it in <![CDATA[..
 | CDataRaw       -- ^ As-is character data; pretty printer passes it along without any escaping or CDATA wrap-up.
   deriving ( Eq, Show, Typeable, Data )

-- | XML qualified names
data QName    = QName {
                  qName   :: Text,
                  qURI    :: Maybe Text,
                  qPrefix :: Maybe Text
                } deriving (Show, Typeable, Data)


instance Eq QName where
  q1 == q2  = compare q1 q2 == EQ

instance Ord QName where
  compare q1 q2 =
    case compare (qName q1) (qName q2) of
      EQ  -> case (qURI q1, qURI q2) of
               (Nothing,Nothing) -> compare (qPrefix q1) (qPrefix q2)
               (u1,u2)           -> compare u1 u2
      x   -> x


-- blank elements --------------------------------------------------------------

-- | Blank names
blank_name :: QName
blank_name = QName { qName = mempty, qURI = Nothing, qPrefix = Nothing }

-- | Blank cdata
blank_cdata :: CData
blank_cdata = CData { cdVerbatim = CDataText, cdData = mempty, cdLine = Nothing }

-- | Blank elements
blank_element :: Element
blank_element = Element
                  { elName    = blank_name
                  , elAttribs = mempty
                  , elContent = mempty
                  , elLine    = Nothing
                  }


