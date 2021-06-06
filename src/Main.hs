{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

import Control.Applicative ((<|>))
import Data.Char (isAlphaNum, isSpace, toLower)
import Data.Functor.Identity (runIdentity)
import Data.List
  ( find,
    intercalate,
    isPrefixOf,
    isSuffixOf,
  )
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isJust, maybeToList)
import Data.Monoid (mappend)
import qualified Data.Set as S
import qualified Data.Text as T
import Debug.Trace
import GHC.Generics
import Hakyll
import qualified Hakyll.Core.Store as Store
import System.Environment.Blank
import System.Envy
import System.FilePath (addTrailingPathSeparator)
import System.FilePath.Posix
  ( joinPath,
    normalise,
    splitDirectories,
    splitPath,
    takeBaseName,
    takeDirectory,
    takeFileName,
    (</>),
  )
import System.IO.Unsafe (unsafePerformIO)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import qualified Text.Pandoc.Templates as PT (Template, compileTemplate)
import Text.Pandoc.Walk

--------------------------------------------------------------------------------

data HakyllConfig = HakyllConfig
  { hakyllTitle :: String, -- "HAKYLL_TITLE",
    hakyllBaseURL :: String, -- "HAKYLL_BASE_URL"
    hakyllDestinationDirectory :: String, -- "HAKYLL_DESTINATION_DIRECTORY"
    hakyllProviderDirectory :: String, -- "HAKYLL_PROVIDER_DIRECTORY"
    hakyllStoreDirectory :: String, -- "HAKYLL_STORE_DIRECTORY"
    hakyllDeployCommand :: String -- "HAKYLL_DEPLOY_COMMAND"
  }
  deriving (Generic, Show)

instance FromEnv HakyllConfig

instance DefConfig HakyllConfig where
  defConfig = HakyllConfig "site untitled" "" "../public" ".." ".hakyll-cache" "bash deploy.sh"

hakyllConfig :: HakyllConfig
{-# NOINLINE hakyllConfig #-}
hakyllConfig = unsafePerformIO $ decodeWithDefaults defConfig

config :: Configuration
config =
  trace (show hakyllConfig) $
    defaultConfiguration
      { ignoreFile = ignoreFile',
        destinationDirectory = destinationDirectory',
        providerDirectory = hakyllProviderDirectory hakyllConfig,
        storeDirectory = storeDirectory',
        deployCommand = hakyllDeployCommand hakyllConfig
      }
  where
    destinationDirectory' = hakyllDestinationDirectory hakyllConfig
    storeDirectory' = hakyllStoreDirectory hakyllConfig
    -- I normally use ../public as destinationDirectory, here I ignore anyfiles starts with public.
    -- There are some false positives. It's good enough for now.
    ignoreFile' p = ignoreFile defaultConfiguration p || destinationDirectoryBase `isPrefixOf` p || storeDirectoryBase `isPrefixOf` p
      where
        destinationDirectoryBase = takeFileName destinationDirectory'
        storeDirectoryBase = takeFileName storeDirectory'

trimString :: String -> String
trimString = f . f
  where
    f = reverse . dropWhile isSpace

main :: IO ()
main = hakyllWith config $ do
  match "posts/*.org" $ do
    route tempRoute
    compile $ getResourceString >>= orgCompiler
  match (("posts/*" .&&. complement "posts/*.org") .||. "_temp/posts/*.org") $ do
    route $ metadataRoute titleRoute `composeRoutes` cleanRoute
    compile $
      customPandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
        >>= cleanIndexUrls

  match ("pages/*" .&&. complement "pages/index.html") $ do
    route $ setExtension "html" `composeRoutes` gsubRoute "pages/" (const "") `composeRoutes` cleanRoute
    compile $
      customPandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" myDefaultContext
        >>= relativizeUrls
        >>= cleanIndexUrls
  match "pages/index.html" $ do
    route $ constRoute "index.html"
    compile $ do
      posts <- recentFirst =<< loadAll (("posts/*" .&&. complement "posts/*.org") .||. "_temp/posts/*.org")
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Home"
              `mappend` myDefaultContext
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls
        >>= cleanIndexUrls

  create ["404.html"] $ do
    route idRoute
    compile $ do
      let archiveCtx =
            constField "body" "404 Not Found"
              `mappend` constField "title" "404 Not Found"
              `mappend` myDefaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls
  create ["403.html"] $ do
    route idRoute
    compile $ do
      let archiveCtx =
            constField "body" "403 Forbidden"
              `mappend` constField "title" "403 Forbidden"
              `mappend` myDefaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls
  create ["archive.html"] $ do
    route $ idRoute `composeRoutes` cleanRoute
    compile $ do
      posts <- recentFirst =<< loadAll (("posts/*" .&&. complement "posts/*.org") .||. "_temp/posts/*.org")
      let archiveCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Archive"
              `mappend` myDefaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/post-list.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls
        >>= cleanIndexUrls

  -- Assume the hakyll program lies within the subdirectory.
  match "*/css/*" $ do
    route $ customRoute $ joinPath . drop 1 . splitPath . toFilePath
    compile compressCssCompiler
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler
  match "*/js/**" $ do
    route $ customRoute $ joinPath . drop 1 . splitPath . toFilePath
    compile copyFileCompiler
  match "js/**" $ do
    route idRoute
    compile copyFileCompiler
  match "*/fonts/**" $ do
    route $ customRoute $ joinPath . drop 1 . splitPath . toFilePath
    compile copyFileCompiler
  match "fonts/**" $ do
    route idRoute
    compile copyFileCompiler
  match "*/templates/*" $ compile templateBodyCompiler
  match "templates/*" $ compile templateBodyCompiler

  match "assets/**" $ do
    route idRoute
    compile copyFileCompiler
  match "documents/**" $ do
    route idRoute
    compile copyFileCompiler
  match "pictures/**" $ do
    route idRoute
    compile copyFileCompiler
  match "images/**" $ do
    route idRoute
    compile copyFileCompiler

  match ("*/node_modules/katex/dist/katex.*" .||. "*/node_modules/katex/dist/contrib/auto-render.*" .||. "*/node_modules/katex/dist/fonts/**") $ do
    route $ gsubRoute "^[^/]+/node_modules/katex/dist/" (const "vendor/katex/")
    compile copyFileCompiler
  match ("node_modules/katex/dist/katex.*" .||. "node_modules/katex/dist/contrib/auto-render.*" .||. "node_modules/katex/dist/fonts/**") $ do
    route $ gsubRoute "node_modules/katex/dist/" (const "vendor/katex/")
    compile copyFileCompiler

-- Copied from https://www.rohanjain.in/hakyll-clean-urls/
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = getFileNameFromSlug $ toFilePath ident

getFileNameFromSlug :: FilePath -> FilePath
getFileNameFromSlug p
  | idx `isSuffixOf` p = p
  | otherwise = takeDirectory p </> takeBaseName p </> "index.html"
  where
    idx = "index.html"

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
  where
    pattern = "/index.html"
    replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
  | idx `isSuffixOf` url = take (length url - length idx) url
  | otherwise = url
  where
    idx = "index.html"

--------------------------------------------------------------------------------
myDefaultContext :: Context String
myDefaultContext =
  constField "siteTitle" (hakyllTitle hakyllConfig)
    <> defaultContext

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` mathCtx
    `mappend` myDefaultContext

enableKatexMin :: Bool
enableKatexMin = True

enableMathml :: Bool
enableMathml = False

mathjaxScript :: String
mathjaxScript = "<script type=\"text/javascript\" async src=\"https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML\"></script>"

katexScript :: String
katexScript =
  let min = if enableKatexMin then ".min" else ""
   in "<link rel=\"stylesheet\" href=\"/vendor/katex/katex" ++ min ++ ".css\">\n<script defer type=\"text/javascript\" src=\"/vendor/katex/katex" ++ min ++ ".js\"></script>\n<script defer type=\"text/javascript\" src=\"/vendor/katex/contrib/auto-render" ++ min ++ ".js\" onload=\"renderMathInElement(document.body);\"></script>"

mathCtx :: Context String
mathCtx = field "mathRender" $ \item -> do
  metadata <- getMetadata $ itemIdentifier item
  return $
    case map toLower <$> lookupString "math" metadata of
      Just "mathjax" -> mathjaxScript
      Just "mathml" -> ""
      Just "no" -> ""
      Just "off" -> ""
      Just "0" -> ""
      _ -> katexScript

readBoolOption :: String -> Bool
readBoolOption x = map toLower x `elem` ["on", "yes", "true", "1"]

enableTOC :: Bool
enableTOC = False

enableNumberSections :: Bool
enableNumberSections = False

customPandocCompiler :: Compiler (Item String)
customPandocCompiler = do
  let mathExtensions =
        extensionsFromList
          [ Ext_tex_math_dollars,
            Ext_tex_math_single_backslash,
            Ext_tex_math_double_backslash,
            Ext_latex_macros,
            Ext_citations,
            Ext_footnotes
          ]
  let defaultExtensions = writerExtensions defaultHakyllWriterOptions
  let newExtensions = defaultExtensions <> mathExtensions
  identifier <- getUnderlying
  mathOption <- getMetadataField identifier "math"
  tocOption <- getMetadataField identifier "enabletoc"
  numberSectionsOption <- getMetadataField identifier "enablenumbersections"
  let enableToc = maybe enableTOC readBoolOption tocOption
  path <- getResourceFilePath
  let writerOptions =
        defaultHakyllWriterOptions
          { writerExtensions = newExtensions,
            writerHTMLMathMethod = case fmap (map toLower) mathOption of
              Just "mathml" -> MathML
              _ | enableMathml -> MathML
              _ -> MathJax "",
            writerNumberSections = maybe enableNumberSections readBoolOption numberSectionsOption,
            writerTableOfContents = enableToc,
            writerTOCDepth = 3,
            writerTemplate = if enableToc then Just tocTemplate else Nothing
          }
  pandocCompilerWithTransformM defaultHakyllReaderOptions writerOptions (internalLinkTransform path)

tocTemplate :: PT.Template T.Text
tocTemplate = case runIdentity $ PT.compileTemplate "" tmpl of
  Left err -> error err
  Right template -> template
  where
    tmpl =
      "\n<div class=\"toc\"><div class=\"header\">Table of Contents</div>\n$toc$\n</div>\n$body$"

slugify :: String -> String
slugify =
  T.unpack . T.intercalate (T.singleton '-') . T.words . T.toLower . clean . T.pack
  where
    clean :: T.Text -> T.Text
    clean = T.map keepAlphaNum . T.replace "'" "" . T.replace "&" "and"
    keepAlphaNum :: Char -> Char
    keepAlphaNum x
      | isAlphaNum x = x
      | otherwise = ' '

titleRoute :: Metadata -> Routes
titleRoute =
  constRoute . fileNameFromMetadata

fileNameFromMetadata :: Metadata -> String
fileNameFromMetadata =
  getFileNameFromSlug . getSlugFromMeta

getSlugFromMeta :: Metadata -> String
getSlugFromMeta m =
  fromJust $ lookupString "slug" m <|> lookupString "title" m

-- | Internal link transformation
internalLinkTransform :: FilePath -> Pandoc -> Compiler Pandoc
internalLinkTransform route = walkM (internalLinkTransform' route)

internalLinkTransform' :: FilePath -> Inline -> Compiler Inline
internalLinkTransform' route orig@(Link attr inl (url, title)) = do
  res <- getIdentifierURLCompiler route (fromFilePath $ T.unpack url)
  return $ go res
  where
    go :: Store.Result String -> Inline
    go (Store.Found newUrl) = Link attr inl (T.pack newUrl, title)
    go _ = orig
internalLinkTransform' _ x = return x

-- The url was saved to the store with the original file path as key, and the url as value.
-- The path passed here is of the form `*/_temp/posts/*.org`. We drop the path before `_temp`.
getIdentifierURLCompiler :: FilePath -> Identifier -> Compiler (Store.Result String)
getIdentifierURLCompiler path identifier = unsafeCompiler $ do
  let p = toFilePath identifier
      splittedPath = splitPath $ normalise $ takeDirectory path </> p
      tempDir = addTrailingPathSeparator "_temp/"
      newPath = joinPath $ if tempDir `elem` splittedPath then drop 1 $ dropWhile (/= tempDir) splittedPath else splittedPath
  store <- Store.new False $ storeDirectory config
  url <- Store.get store [newPath]
  return (trace ("Obtaining URL for identifier: " ++ show identifier ++ ", path: " ++ path ++ ", newPath: " ++ newPath ++ ", URL: " ++ show url) url)

setIdentifierURLCompiler :: Identifier -> String -> Compiler ()
setIdentifierURLCompiler identifier url = unsafeCompiler $ do
  store <- Store.new False $ storeDirectory config
  let path = normalise $ toFilePath identifier
  trace ("Saving path-URL mapping, path: " ++ path ++ ", URL: " ++ url) Store.set store [path] url

-- | From org get metadatas.
orgCompiler :: Item String -> Compiler (Item String)
orgCompiler i = do
  identifier <- getUnderlying
  withItemBody (go identifier) i
  where
    go :: Identifier -> String -> Compiler String
    go identifier body =
      let dashes = "----------"
          metadata = getOrgMetadata body
          slug = fromJust $ M.lookup "slug" metadata
          filename = getFileNameFromSlug slug
          metadataLines = map (\x -> intercalate ": " [fst x, snd x]) $ M.toList metadata
          result = unlines ([dashes] ++ metadataLines ++ [dashes]) ++ body
       in fmap (const result) $ setIdentifierURLCompiler identifier $ toUrl filename

getOrgMetadata :: String -> M.Map String String
getOrgMetadata body =
  let getMap = M.fromList . map getTuple . getGoodLines . lines
      m = getMap body
      slug = slugify $ fromJust $ M.lookup "slug" m <|> M.lookup "title" m
   in M.insert "slug" slug m
  where
    getGoodLines = filter (/= "") . takeWhile (\x -> isPrefixOf "#+" x || x == "")
    clean :: String -> String
    clean = concat . splitOn "#+"
    format :: String -> (String, String)
    format s = case splitOn ":" s of
      [] -> error ("Invalid metadata for line " ++ s)
      x : xs -> (map toLower x, trimString $ intercalate ":" xs)
    getTuple = format . clean

tempRoute :: Routes
tempRoute = customRoute tempRoute'
  where
    tempRoute' i = ".." </> "_temp" </> takeDirectory p </> takeFileName p
      where
        p = toFilePath i
