module Emanote.Pandoc.BuiltinFilters (
  preparePandoc,
) where

import Emanote.Pandoc.ExternalLink (setExternalLinkIcon)
import Emanote.Pandoc.Markdown.Syntax.HashTag qualified as HT
import Emanote.Route (encodeRoute)
import Emanote.Route.SiteRoute.Type (encodeTagIndexR)
import Relude
import Text.Pandoc.Definition qualified as B
import Text.Pandoc.Walk qualified as W

preparePandoc :: W.Walkable B.Inline b => b -> b
preparePandoc =
  linkifyInlineTags
    >>> fixEmojiFontFamily
    >>> setExternalLinkIcon

-- HashTag.hs generates a Span for inline tags.
-- Here, we must link them to the special tag index page.
linkifyInlineTags :: W.Walkable B.Inline b => b -> b
linkifyInlineTags =
  W.walk $ \case
    inline@(B.Span attr is) ->
      if
          | Just inlineTag <- HT.getTagFromInline inline ->
              B.Span attr [B.Link mempty is (tagUrl inlineTag, "Tag")]
          | otherwise ->
              inline
    x ->
      x
  where
    tagUrl =
      toText . encodeRoute . encodeTagIndexR . toList . HT.deconstructTag

-- Undo font-family on emoji spans, so the browser uses an emoji font.
-- Ref: https://github.com/jgm/commonmark-hs/blob/3d545d7afa6c91820b4eebf3efeeb80bf1b27128/commonmark-extensions/src/Commonmark/Extensions/Emoji.hs#L30-L33
fixEmojiFontFamily :: W.Walkable B.Inline b => b -> b
fixEmojiFontFamily =
  W.walk $ \case
    B.Span (id', classes, attrs) is
      | classes == ["emoji"] ->
          let emojiFontAttr = ("style", "font-family: emoji")
              newAttrs = attrs <> one emojiFontAttr
           in B.Span (id', classes, newAttrs) is
    x -> x

removeLinksAndTagsNestedInLinks :: W.Walkable B.Inline b => b -> b
removeLinksAndTagsNestedInLinks =
  W.walk $ \case
    B.Link (id, classes, attrs) inlines (url, title)
      -> B.Link (id, classes, newAttrs) (removeLinksAndTags inlines) (url, title)
    x -> x
  where
    removeLinksAndTags :: W.Walkable [B.Inline] b => b -> b
    removeLinksAndTags = W.walk (concatMap linkOrTagToContents)
    linkOrTagToContents :: B.Inline -> [B.Inline]
    linkOrTagToContents (B.Link (_,_,_) is (_, _)) = is
    linkOrTagToContents span@(B.Span _ is)
      | Just _ <- HT.getTagFromInline span -> is
    linkOrTagToContents i = [i]


