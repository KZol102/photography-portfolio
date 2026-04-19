{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.List (sortBy)
import Data.String.Interpolate (i)
import Data.Text (Text, pack, replace, unpack)
import Data.Time (toGregorian)
import Data.Time.Clock
import Hakyll (Context, Identifier, Item (Item), MonadMetadata (getMatches), Rules, applyAsTemplate, compile, composeRoutes, compressCssCompiler, constField, constRoute, copyFileCompiler, defaultContext, getResourceBody, getUnderlying, gsubRoute, hakyll, hasVersion, idRoute, listField, loadAndApplyTemplate, match, pandocCompiler, preprocess, route, setExtension, templateBodyCompiler, toFilePath, version, (.&&.))
import Hakyll.Images (compressJpgCompiler, loadImage)
import System.FilePath (splitFileName)

baseTemplate :: Identifier
baseTemplate = "content/templates/base-template.html"

pictureTemplate :: Identifier
pictureTemplate = "content/templates/picture-template.html"

main :: IO ()
main = hakyll do
  loadTemplates
  compileCssFiles
  copyFonts
  copyFullQualityPictures
  compilePagesForPictures
  copyThumbnailsForPictures
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

copyFonts :: Rules ()
copyFonts = match "content/fonts/*" do
  route idRoute
  compile copyFileCompiler

copyFullQualityPictures :: Rules ()
copyFullQualityPictures = match "content/media/pictures/*.jpg" $ version "original" do
  route idRoute
  compile copyFileCompiler

copyThumbnailsForPictures :: Rules ()
copyThumbnailsForPictures = match "content/media/pictures/thumbnails/*.jpg" $ version "thumbnail" do
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

generateMainPortfolioPage :: Rules ()
generateMainPortfolioPage = match "content/templates/index-template.html" $ version "page" do
  route $ constRoute "content/index.html"
  ctx <- getContext
  compile do
    photoIds <- getMatches ("content/media/pictures/thumbnails/*.jpg" .&&. hasVersion "thumbnail")
    let photoHtmls = map (\iden -> (iden, getGalleryHTML (getThumbnailUrl (toFilePath iden)) (getHtmlUrl (toFilePath iden)))) photoIds
    let orderedPhotoHtmls = sortBy (\x y -> reverseOrderByFileNameNumber (toFilePath (fst x)) (toFilePath (fst y))) photoHtmls
    let photos = fmap (uncurry Item) orderedPhotoHtmls
    let photosContext =
          listField "photos" ctx (pure photos)
            <> constField "title" "Portfolio"
            <> ctx
    getResourceBody
      >>= applyAsTemplate photosContext
      >>= loadAndApplyTemplate baseTemplate photosContext
  where
    reverseOrderByFileNameNumber :: String -> String -> Ordering
    reverseOrderByFileNameNumber x y = compare (pathToNumber y) (pathToNumber x)

    pathToNumber :: String -> Int
    pathToNumber = read . takeWhile (/= '_') . snd . splitFileName

    getThumbnailUrl :: String -> String
    getThumbnailUrl = replaceStr "content/media/pictures/" "./media/pictures/"

    getHtmlUrl :: String -> String
    getHtmlUrl = replaceStr ".jpg" ".html" . replaceStr "content/media/pictures/thumbnails/" "./portfolio-entries/"

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

replaceStr :: Text -> Text -> String -> String
replaceStr needle replacement haystack = unpack $ replace needle replacement $ pack haystack

getGalleryHTML :: String -> String -> String
getGalleryHTML thumbUrl pageUrl =
  [i|
  <div class="box">
      <a href="#{pageUrl}">
          <img src="#{thumbUrl}" />
      </a>
  </div>
  |]