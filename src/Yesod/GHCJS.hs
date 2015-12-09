{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | GHCJS support for Yesod.

module Yesod.GHCJS
    ( CompileOptions(..)
    , compileGhcjs
    ) where

import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString as BS
import           Data.Char
import           Data.Conduit
import qualified Data.Conduit.Combinators as CL
import           Data.Conduit.Process
import           Data.FileEmbed (embedFile)
import           Data.Maybe
import           Data.String (fromString)
import           Language.Haskell.TH
import           Language.Haskell.TH.Lift
import           Language.Haskell.TH.Syntax
import           Prelude
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath (hasExtension, takeExtension, (</>))
import           System.IO
import           System.IO.Temp
import           Yesod.Core

data CompileOptions = CompileOptions
    { compileDev :: Bool
    -- ^ When @True@, this indicates development mode. In this mode,
    -- compilation is done at runtime. When @False@, compilation of the
    -- client is done at compile time. The combined js file (@all.js@) is
    -- then read and turned into a literal value included in the generated
    -- code.
    , compileFolders :: [FilePath]
    -- ^ Folders which contain the client source files. This is
    -- necessary for non-development mode, so that 'addDependentFile'
    -- can be used to cause recompilation when the files change.
    --
    -- Also, with 'PlainCompileMode', these folders are provided as
    -- directories to search for source files (@-i@).
    , compileJsWanted :: FilePath
    -- ^ Which GHCJS output file is wanted. Common values include
    -- @all.js@ and @out.js@.
    , compileMode :: CompileMode
    -- ^ Which compilation mode to use.
    }

data CompileMode
    = StackCompileMode
        { stackOptions :: [String]
        , stackYaml :: FilePath
        , stackPkg :: String
        , stackExe :: FilePath
        }
    | PlainCompileMode
        { ghcjsOptions :: [String]
        , ghcjsMain :: FilePath
        }

$(deriveLiftMany [''CompileOptions, ''CompileMode])

-- | Make a widget by compiling the module to JS to a temporary file
-- and then loading in the JS. Results in a @Handler TypedContent@
-- expression.
compileGhcjs :: CompileOptions -> Q Exp
compileGhcjs opts =
    if compileDev opts
        then [| liftIO $ do
            genfp <- runGhcjs opts
            content <- BS.readFile genfp
            return $ TypedContent typeJavascript $ toContent content |]
        else do
            genfp <- runIO $ runGhcjs opts
            sourceFiles <- runIO $ runResourceT $
                mapM_ (CL.sourceDirectoryDeep False . fromString) (compileFolders opts) $=
                CL.filter (("hs" ==) . takeExtension) $$
                CL.sinkList
            mapM_ qAddDependentFile sourceFiles
            bs <- embedFile genfp
            f <- [|return . TypedContent typeJavascript . toContent|]
            return $ f `AppE` bs

-- | Run ghcjs on a file and return the all.js content.
runGhcjs :: CompileOptions -> IO FilePath
runGhcjs opts = do
    tempDir <- getTemporaryDirectory
    withTempDirectory tempDir "run-ghcjs." $ \tmpdir -> do
        let args = case compileMode opts of
                PlainCompileMode{..} ->
                    [ghcjsMain, "-outputdir", "dist/ghcjs", "-o", tmpdir, "-i"] ++
                    map (\folder -> "-i" ++ folder) (compileFolders opts)++
                    ghcjsOptions
                StackCompileMode{..} ->
                    ["build", "--stack-yaml", stackYaml] ++ stackOptions
        path <- case compileMode opts of
            PlainCompileMode{} -> fmap (fromMaybe "ghcjs") (lookupEnv "GHCJS_PATH")
            StackCompileMode{} -> fmap (fromMaybe "stack") (lookupEnv "STACK_EXE")
        (Just inh,Nothing,Nothing,p) <- createProcess
            (proc path args)
            { close_fds = True
            , std_in = CreatePipe
            }
        hClose inh
        let display = path ++ " " ++ unwords args
        putStrLn display
        code <- waitForProcess p
        case code of
            ExitSuccess -> do
                (srcfp, genfp) <- case compileMode opts of
                    PlainCompileMode{..} ->
                        return ( tmpdir </> compileJsWanted opts
                               , "dist/ghcjs-cache/" ++ slugize ghcjsMain ++ ".js")
                    StackCompileMode{..} -> do
                        distDir <-
                            reverse . dropWhile isSpace . reverse <$>
                            readProcess path ["path", "--dist-dir", "--stack-yaml", stackYaml] ""
                        return ( distDir </> stackPkg </> (stackExe ++ ".jsexe") </> compileJsWanted opts
                               , distDir </> ("ghcjs-cache/" ++ slugize stackYaml ++ ".js") )
                putStrLn ("Copying " ++ srcfp ++ " to " ++ genfp ++ " ...")
                copyFile srcfp genfp
                return genfp
            ExitFailure e -> error ("The GHCJS process failed with exit code " ++ show e ++ ".")

-- | Make a slug from a file path.
slugize :: FilePath -> String
slugize = map replace
  where replace c
          | isLetter c || isDigit c = toLower c
          | otherwise = '_'
