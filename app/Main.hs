{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.String.Interpolate (i)
import Data.Text (Text, pack, replace, unpack)
import Hakyll (Identifier, Item (Item), MonadMetadata (getMatches), Rules, applyAsTemplate, compile, composeRoutes, compressCssCompiler, constField, constRoute, copyFileCompiler, defaultContext, getResourceBody, getUnderlying, gsubRoute, hakyll, hasVersion, idRoute, listField, loadAndApplyTemplate, match, pandocCompiler, route, setExtension, templateBodyCompiler, toFilePath, version, (.&&.))
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
  compile do
    url <- getUnderlying
    let pageContext = defaultContext <> constField "picture_url" (getPictureUrl $ toFilePath url)
    getResourceBody
      >>= loadAndApplyTemplate pictureTemplate pageContext
      >>= loadAndApplyTemplate baseTemplate pageContext

generateThumbnailsForPictures :: Rules ()
generateThumbnailsForPictures = match "content/media/pictures/*.jpg" $ version "thumbnail" do
  route $ gsubRoute "/pictures/" $ const "/pictures/thumbs/"
  compile $
    loadImage
      >>= scaleImageCompiler 800 800
      >>= compressJpgCompiler (50 :: Integer)

generateMainPortfolioPage :: Rules ()
generateMainPortfolioPage = match "content/templates/index-template.html" $ version "page" do
  route $ constRoute "index.html"
  compile do
    photoIds <- getMatches ("content/media/pictures/*.jpg" .&&. hasVersion "thumbnail")
    let photoHtmls = map (\iden -> (iden, getGalleryHTML (getThumbnailUrl (toFilePath iden)) (getHtmlUrl (toFilePath iden)))) photoIds
    let photos = fmap (uncurry Item) photoHtmls
    let photosContext =
          listField "photos" defaultContext (pure photos)
            <> constField "title" "Portfolio"
            <> defaultContext
    getResourceBody
      >>= applyAsTemplate photosContext
      >>= loadAndApplyTemplate baseTemplate photosContext

generatePages :: Rules ()
generatePages = match "content/pages/*.md" do
  route $ composeRoutes (gsubRoute "content/pages/" $ const "./") (setExtension "html")
  compile do
    pandocCompiler
      >>= loadAndApplyTemplate baseTemplate defaultContext

generatePosts :: Rules ()
generatePosts = match "content/posts/*.md" do
  route $ composeRoutes (gsubRoute "content/posts/" $ const "./post/") (setExtension "html")
  compile do
    pandocCompiler
      >>= loadAndApplyTemplate baseTemplate defaultContext

getPictureUrl :: String -> String
getPictureUrl = replaceStr "content/" "../"

getThumbnailUrl :: String -> String
getThumbnailUrl = replaceStr "/pictures/" "/pictures/thumbs/"

getHtmlUrl :: String -> String
getHtmlUrl = replaceStr ".jpg" ".html" . replaceStr "/media/pictures/" "/portfolio-entries/"

replaceStr :: Text -> Text -> String -> String
replaceStr needle replacement haystack = unpack $ replace needle replacement $ pack haystack

getGalleryHTML :: String -> String -> String
getGalleryHTML thumbUrl pageUrl =
  [i|
  <div class="box">
      <div class="boxInner">
          <a href="#{pageUrl}">
              <img src="#{thumbUrl}">
          </a>
      </div>
  </div>
  |]