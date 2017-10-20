{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

--------------------------------------------------------------------
-- |
-- Module    : Text.XML.Light.Output
-- Copyright : (c) Galois, Inc. 2007
-- License   : BSD3
--
-- Maintainer: Iavor S. Diatchki <diatchki@galois.com>
-- Stability : provisional
-- Portability:
--
-- Output handling for the lightweight XML lib.
--

module Text.XML.Light.Output
  ( showTopElement, showContent, showElement, showCData, showQName, showAttr
  , ppTopElement, ppContent, ppElement
  , ppcTopElement, ppcContent, ppcElement
  , ConfigPP
  , defaultConfigPP, prettyConfigPP
  , useShortEmptyTags, useExtraWhiteSpace
  , tagEnd, xml_header
  ) where

import Text.XML.Light.Types
import Data.Char
import Data.List ( intersperse )
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB

-- | The XML 1.0 header
xml_header :: BL.ByteString
xml_header = "<?xml version='1.0' ?>"


--------------------------------------------------------------------------------
data ConfigPP = ConfigPP
  { shortEmptyTag :: QName -> Bool
  , prettify      :: Bool
  }

-- | Default pretty printing configuration.
--  * Always use abbreviate empty tags.
defaultConfigPP :: ConfigPP
defaultConfigPP = ConfigPP { shortEmptyTag = const True
                           , prettify      = False
                           }

-- | The predicate specifies for which empty tags we should use XML's
-- abbreviated notation <TAG />.  This is useful if we are working with
-- some XML-ish standards (such as certain versions of HTML) where some
-- empty tags should always be displayed in the <TAG></TAG> form.
useShortEmptyTags :: (QName -> Bool) -> ConfigPP -> ConfigPP
useShortEmptyTags p c = c { shortEmptyTag = p }


-- | Specify if we should use extra white-space to make document more readable.
-- WARNING: This adds additional white-space to text elements,
-- and so it may change the meaning of the document.
useExtraWhiteSpace :: Bool -> ConfigPP -> ConfigPP
useExtraWhiteSpace p c  = c { prettify = p }

-- | A configuration that tries to make things pretty
-- (possibly at the cost of changing the semantics a bit
-- through adding white space.)
prettyConfigPP     :: ConfigPP
prettyConfigPP      = useExtraWhiteSpace True defaultConfigPP


--------------------------------------------------------------------------------


-- | Pretty printing renders XML documents faithfully,
-- with the exception that whitespace may be added\/removed
-- in non-verbatim character data.
ppTopElement       :: Element -> BL.ByteString
ppTopElement        =
  ppcTopElement prettyConfigPP

-- | Pretty printing elements
ppElement          :: Element -> BL.ByteString
ppElement           =
  ppcElement prettyConfigPP

-- | Pretty printing content
ppContent          :: Content -> BL.ByteString
ppContent           =
  ppcContent prettyConfigPP



-- | Pretty printing renders XML documents faithfully,
-- with the exception that whitespace may be added\/removed
-- in non-verbatim character data.
ppcTopElement      :: ConfigPP -> Element -> BL.ByteString
ppcTopElement c e   =
  xml_header <> ppcElement c e

-- | Pretty printing elements
ppcElement         :: ConfigPP -> Element -> BL.ByteString
ppcElement c e      =
  BB.toLazyByteString (ppElementS c "" e)

-- | Pretty printing content
ppcContent         :: ConfigPP -> Content -> BL.ByteString
ppcContent c x      =
  BB.toLazyByteString (ppContentS c "" x)





-- | Pretty printing content using TextS
ppContentS         :: ConfigPP -> T.Text -> Content -> BB.Builder
ppContentS c i x = case x of
                     Elem e -> ppElementS c i e
                     Text t -> ppCDataS c i t
                     CRef r -> showCRefS r

ppElementS         :: ConfigPP -> T.Text -> Element -> BB.Builder
ppElementS c i e = TE.encodeUtf8Builder i <> tagStart (elName e) (elAttribs e) <>
  case elContent e of
    [] | "?" `T.isPrefixOf` qName name ->
           BB.string7 " ?>"
       | shortEmptyTag c name ->
           BB.string7 " />"
    [Text t] ->
      BB.char8 '>' <> ppCDataS c "" t <> tagEnd name
    cs -> BB.char8 '>' <> nl <> mconcat (map ppSub cs) <>
          TE.encodeUtf8Builder i <>
          tagEnd name
      where ppSub e1 = ppContentS c (sp <> i) e1 <> nl
            (nl,sp)
              | prettify c = (BB.char8 '\n', "  ")
              | otherwise  = (mempty,mempty)
  where name = elName e

ppCDataS           :: ConfigPP -> T.Text -> CData -> BB.Builder
ppCDataS c i t      =
  TE.encodeUtf8Builder i <>
  if cdVerbatim t /= CDataText || not (prettify c)
    then showCDataS t mempty
    else showCDataS t i



--------------------------------------------------------------------------------

-- | Adds the <?xml?> header.
showTopElement     :: Element -> BL.ByteString
showTopElement c    = xml_header <> showElement c

showContent        :: Content -> BL.ByteString
showContent c       =
  BB.toLazyByteString (ppContentS defaultConfigPP "" c)

showElement        :: Element -> BL.ByteString
showElement c       =
  BB.toLazyByteString (ppElementS defaultConfigPP "" c)

showCData          :: CData -> BL.ByteString
showCData c         =
  BB.toLazyByteString (ppCDataS defaultConfigPP "" c)

-- Note: crefs should not contain '&', ';', etc.
showCRefS          :: T.Text -> BB.Builder
showCRefS r         =
  BB.char8 '&' <> TE.encodeUtf8Builder r <> BB.char8 ';'

-- | Convert a text element to characters.
showCDataS         :: CData -> T.Text -> BB.Builder
showCDataS cd i =
 case cdVerbatim cd of
   CDataText     -> escStr (reindent (cdData cd))
   CDataVerbatim -> BB.string7 "<![CDATA[" <>
                    escCData (reindent (cdData cd)) <>
                    BB.string7 "]]>"
   CDataRaw      -> TE.encodeUtf8Builder (reindent (cdData cd))
  where reindent = T.concatMap go
        go '\n' = T.singleton '\n' <> i
        go c    = T.singleton c

--------------------------------------------------------------------------------
escCData           :: T.Text -> BB.Builder
escCData t
  | Just cs <- T.stripPrefix "]]>" t =
      BB.string7 "]]]]><![CDATA[>" <> escCData cs
  | Just (c, cs) <- T.uncons t =
      BB.char8 c <> escCData cs
  | otherwise = mempty

escChar            :: Char -> BB.Builder
escChar c = case c of
  '<'   -> BB.string7 "&lt;"
  '>'   -> BB.string7 "&gt;"
  '&'   -> BB.string7 "&amp;"
  '"'   -> BB.string7 "&quot;"
  -- we use &#39 instead of &apos; because IE apparently has difficulties
  -- rendering &apos; in xhtml.
  -- Reported by Rohan Drape <rohan.drape@gmail.com>.
  '\''  -> BB.string7 "&#39;"

  -- NOTE: We escape '\r' explicitly because otherwise they get lost
  -- when parsed back in because of then end-of-line normalization rules.
  _ | isPrint c || c == '\n' -> BB.char8 c
    | otherwise -> BB.string7 "&#" <> BB.intDec oc <> BB.char8 ';'
      where oc = ord c

escStr             :: T.Text -> BB.Builder
escStr cs        = mconcat [ escChar c | c <- T.unpack cs ]

tagEnd             :: QName -> BB.Builder
tagEnd qn           =
  BB.char8 '<' <>
  BB.char8 '/' <>
  showQName qn <>
  BB.char8 '>'

tagStart           :: QName -> [Attr] -> BB.Builder
tagStart qn as   = BB.char8 '<' <> showQName qn <> as_str
 where as_str =
         if null as then mempty else BB.char8 ' ' <>
         mconcat (intersperse (BB.char8 ' ') (map showAttr as))

showAttr           :: Attr -> BB.Builder
showAttr (Attr qn v) =
  showQName qn <> BB.char8 '=' <> BB.char8 '"' <> escStr v <> BB.char8 '"'

showQName          :: QName -> BB.Builder
showQName q         = pre <> TE.encodeUtf8Builder (qName q)
  where pre = case qPrefix q of
                Nothing -> mempty
                Just p  -> TE.encodeUtf8Builder p <> BB.char8 ':'
