{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.String.Interpolate (i)
import Data.Text (Text, pack, replace, unpack)
import Data.Time (toGregorian)
import Data.Time.Clock
import Hakyll (Context, Identifier, Item (Item), MonadMetadata (getMatches), Rules, applyAsTemplate, compile, composeRoutes, compressCssCompiler, constField, constRoute, copyFileCompiler, defaultContext, getResourceBody, getUnderlying, gsubRoute, hakyll, hasVersion, idRoute, listField, loadAndApplyTemplate, match, pandocCompiler, preprocess, route, setExtension, templateBodyCompiler, toFilePath, version, (.&&.))
import Hakyll.Images (compressJpgCompiler, loadImage, scaleImageCompiler)

baseTemplate :: Identifier
baseTemplate = "content/templates/base-template.html"

pictureTemplate :: Identifier
pictureTemplate = "content/templates/picture-template.html"

main :: IO ()
main = hakyll do
  loadTemplates
  compileCssFiles
  copyFullQualityPictures
  compilePagesForPictures
  generateThumbnailsForPictures
  generateMainPortfolioPage
  generatePages
  generatePosts
  compileImages

-- TODO: generate pages for tags

loadTemplates :: Rules ()
loadTemplates = match "content/templates/*" $ compile templateBodyCompiler

compileCssFiles :: Rules ()
compileCssFiles = match "content/css/*" do
  route idRoute
  compile compressCssCompiler

copyFullQualityPictures :: Rules ()
copyFullQualityPictures = match "content/media/pictures/*.jpg" $ version "original" do
  route idRoute
  compile copyFileCompiler

compilePagesForPictures :: Rules ()
compilePagesForPictures = match "content/media/pictures/*.jpg" $ version "picturePage" do
  route $ composeRoutes (gsubRoute "/media/pictures/" (const "/portfolio-entries/")) (setExtension "html")
  ctx <- getContext
  compile do
    url <- getUnderlying
    let pageContext = ctx <> constField "picture_url" (getPictureUrl $ toFilePath url)
    getResourceBody
      >>= loadAndApplyTemplate pictureTemplate pageContext
      >>= loadAndApplyTemplate baseTemplate pageContext

generateThumbnailsForPictures :: Rules ()
generateThumbnailsForPictures = match "content/media/pictures/*.jpg" $ version "thumbnail" do
  route $ gsubRoute "/pictures/" $ const "/pictures/thumbs/"
  compile $
    loadImage
      >>= scaleImageCompiler 800 800
      >>= compressJpgCompiler (90 :: Integer)

generateMainPortfolioPage :: Rules ()
generateMainPortfolioPage = match "content/templates/index-template.html" $ version "page" do
  route $ constRoute "content/index.html"
  ctx <- getContext
  compile do
    -- TODO: ensure ordering of images based on number in name
    photoIds <- getMatches ("content/media/pictures/*.jpg" .&&. hasVersion "thumbnail")
    let photoHtmls = map (\iden -> (iden, getGalleryHTML (getThumbnailUrl (toFilePath iden)) (getHtmlUrl (toFilePath iden)))) photoIds
    let photos = fmap (uncurry Item) photoHtmls
    let photosContext =
          listField "photos" ctx (pure photos)
            <> constField "title" "Portfolio"
            <> ctx
    getResourceBody
      >>= applyAsTemplate photosContext
      >>= loadAndApplyTemplate baseTemplate photosContext

generatePages :: Rules ()
generatePages = match "content/pages/*.md" do
  route $ composeRoutes (gsubRoute "content/pages/" $ const "content/") (setExtension "html")
  ctx <- getContext
  compile do
    pandocCompiler
      >>= loadAndApplyTemplate baseTemplate ctx

generatePosts :: Rules ()
generatePosts = match "content/posts/*.md" do
  route $ setExtension "html"
  ctx <- getContext
  compile do
    pandocCompiler
      >>= loadAndApplyTemplate baseTemplate ctx

compileImages :: Rules ()
compileImages = match "content/media/images/*" do
  route idRoute
  compile do
    loadImage
      >>= compressJpgCompiler (80 :: Integer)

getContext :: Rules (Context String)
getContext = preprocess $ do
  (year, _, _) <- toGregorian . utctDay <$> getCurrentTime
  pure (defaultContext <> constField "year" (show year))

getPictureUrl :: String -> String
getPictureUrl = replaceStr "content/" "../"

getThumbnailUrl :: String -> String
getThumbnailUrl = replaceStr "content/media/pictures/" "./media/pictures/thumbs/"

getHtmlUrl :: String -> String
getHtmlUrl = replaceStr ".jpg" ".html" . replaceStr "content/media/pictures/" "./portfolio-entries/"

replaceStr :: Text -> Text -> String -> String
replaceStr needle replacement haystack = unpack $ replace needle replacement $ pack haystack

getGalleryHTML :: String -> String -> String
getGalleryHTML thumbUrl pageUrl =
  [i|
  <div class="box">
      <div class="boxInner">
          <a href="#{pageUrl}">
              <img src="#{thumbUrl}" loading="lazy" />
          </a>
      </div>
  </div>
  |]