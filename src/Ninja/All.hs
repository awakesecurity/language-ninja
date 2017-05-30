-- Copyright Neil Mitchell 2011-2017.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the name of Neil Mitchell nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- | FIXME: doc
module Ninja.All (runNinja) where

import           Data.Bifunctor
import           Data.Monoid

import qualified Data.ByteString            as BS8
import qualified Data.ByteString.Char8      as BS
import           Development.Shake          hiding (addEnv)
import           Ninja.Env
import           Ninja.Parse
import           Ninja.Type

import           Control.Applicative
import           Control.Exception.Extra
import           Control.Monad
import           Data.Char
import           Data.List.Extra
import           Data.Maybe
import           Prelude
import           System.Directory
import           System.Info.Extra

import           Data.Hashable              (Hashable)

import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HM

import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HS

-- Internal imports
-- import           Development.Shake.Internal.Errors          (errorStructured)
-- import           Development.Shake.Internal.FileName        (filepathNormalise)
-- import           Development.Shake.Internal.Rules.File      (needBS, neededBS)
-- import           Development.Shake.Internal.Rules.OrderOnly (orderOnlyBS)
-- import           General.Makefile                           (parseMakefile)
-- import           General.Timing                             (addTiming)

import           Development.Shake.FilePath (normaliseEx, toStandard)
import           Development.Shake.Util     (parseMakefile)

--------------------------------------------------------------------------------
-- STUBS

addTiming :: String -> IO ()
addTiming _ = pure ()

filepathNormalise :: Str -> Str
filepathNormalise = BS.pack . toStandard . normaliseEx . BS.unpack

needBS :: [Str] -> Action ()
needBS = need . map BS.unpack

neededBS :: [Str] -> Action ()
neededBS = needed . map BS.unpack

orderOnlyBS :: [Str] -> Action ()
orderOnlyBS = orderOnly . map BS.unpack

--------------------------------------------------------------------------------

-- | FIXME: doc
runNinja :: FilePath -> [String] -> Maybe String -> IO (Maybe (Rules ()))
runNinja file args (Just "compdb") = do
  dir <- getCurrentDirectory
  (MkNinja {..}) <- newEnv >>= parse file
  rules <- pure $ HM.fromList [r | r <- rules, BS.unpack (fst r) `elem` args]
  -- the build items are generated in reverse order, hence the reverse
  let xs = [ (a, b, file, rule)
           | (a, (b@(MkBuild {..}))) <- reverse (multiples <> map (first pure) singles)
           , Just rule <- [HM.lookup ruleName rules], file:_ <- [depsNormal]
           ]
  xs <- forM xs $ \(out, (MkBuild {..}), file, (MkRule {..})) -> do
    -- the order of adding new environment variables matters
    env <- scopeEnv env
    addEnv env (BS.pack "out") (BS.unwords $ map quote out)
    addEnv env (BS.pack "in") (BS.unwords $ map quote depsNormal)
    addEnv env (BS.pack "in_newline") (BS.unlines depsNormal)
    forM_ buildBind $ \(a, b) -> addEnv env a b
    addBinds env ruleBind
    commandline <- fmap BS.unpack $ askVar env $ BS.pack "command"
    pure $ MkCompDB dir commandline $ BS.unpack $ head depsNormal
  putStr $ printCompDB xs
  pure Nothing

runNinja file args (Just x)
  = errorIO $ "Unknown tool argument, expected 'compdb', got " <> x

runNinja file args tool = do
  addTiming "Ninja parse"
  (ninja@(MkNinja{..})) <- parse file =<< newEnv
  pure $ Just $ do
    needDeps  <- pure $ needDeps ninja -- partial application
    phonys    <- pure $ HM.fromList phonys
    singles   <- pure $ HM.fromList (first filepathNormalise <$> singles)
    multiples <- pure $ HM.fromList
                 [ (x, (xs, b))
                 | (xs, b) <- first (map filepathNormalise) <$> multiples
                 , x <- xs ]
    rules     <- pure $ HM.fromList rules
    pools     <- fmap HM.fromList
                 $ forM ((BS.pack "console", 1) : pools)
                 $ \(name, depth) ->
                     (name,) <$> newResource (BS.unpack name) depth

    action $ needBS $ concatMap (resolvePhony phonys) $
      if      not $ null args     then map BS.pack args
      else if not $ null defaults then defaults
      else                             HM.keys singles <> HM.keys multiples

    (\x -> (map BS.unpack . fst) <$> HM.lookup (BS.pack x) multiples) &?> \out -> do
      let out2 = map BS.pack out
      build needDeps phonys rules pools out2 $ snd $ multiples HM.! head out2

    (flip HM.member singles . BS.pack) ?> \out -> do
      let out2 = BS.pack out
      build needDeps phonys rules pools [out2] $ singles HM.! out2

resolvePhony :: HashMap Str [Str] -> Str -> [Str]
resolvePhony mp = f $ Left 100
  where
    f (Left 0) x = f (Right []) x
    f (Right xs) x | x `elem` xs = error $ "Recursive phony involving " ++ BS.unpack x
    f a x = case HM.lookup x mp of
              Nothing -> [x]
              Just xs -> concatMap (f $ bimap (subtract 1) (x:) a) xs

quote :: Str -> Str
quote x
  | BS.any isSpace x = let q = BS.singleton '\"' in BS.concat [q, x, q]
  | otherwise        = x


build :: (Build -> [Str] -> Action ())
      -> HashMap Str [Str]
      -> HashMap Str Rule
      -> HashMap Str Resource
      -> [Str]
      -> Build
      -> Action ()
build needDeps phonys rules pools out (build@(MkBuild {..})) = do
  needBS $ concatMap (resolvePhony phonys) $ depsNormal ++ depsImplicit
  orderOnlyBS $ concatMap (resolvePhony phonys) depsOrderOnly
  case HM.lookup ruleName rules of
    Nothing -> do
      liftIO $ errorIO $ mconcat
        [ "Ninja rule named ", BS.unpack ruleName
        , " is missing, required to build ", BS.unpack (BS.unwords out)
        ]
    Just (MkRule {..}) -> do
      env <- liftIO $ scopeEnv env
      liftIO $ do
        -- the order of adding new environment variables matters
        addEnv env "out"        $ BS.unwords $ map quote out
        addEnv env "in"         $ BS.unwords $ map quote depsNormal
        addEnv env "in_newline" $ BS.unlines depsNormal
        forM_ buildBind (uncurry (addEnv env))
        addBinds env ruleBind

      applyRspfile env $ do
        let askVarStr :: String -> Action String
            askVarStr var = liftIO (BS.unpack <$> askVar env (BS.pack var))

        commandline <- askVarStr "command"
        depfile     <- askVarStr "depfile"
        deps        <- askVarStr "deps"
        description <- askVarStr "description"
        pool        <- BS.pack <$> askVarStr "pool"

        let withPool act = case HM.lookup pool pools of
                             _ | BS.null pool -> act
                             Nothing -> liftIO $ errorIO $ mconcat
                                        [ "Ninja pool named "
                                        , BS.unpack pool
                                        , " not found, required to build "
                                        , BS.unpack (BS.unwords out) ]
                             (Just r) -> withResource r 1 act

        when (description /= "") $ putNormal description
        let (cmdOpts, cmdProg, cmdArgs) = toCommand commandline
        if deps == "msvc"
          then do Stdout stdout <- withPool $ command cmdOpts cmdProg cmdArgs
                  prefix <- liftIO $ fmap (fromMaybe "Note: including file: ")
                            $ askEnv env $ BS.pack "msvc_deps_prefix"
                  needDeps build $ parseShowIncludes prefix $ BS.pack stdout
          else withPool $ command_ cmdOpts cmdProg cmdArgs
        when (depfile /= "") $ do
          when (deps /= "gcc") $ need [depfile]
          depsrc <- liftIO $ BS.readFile depfile
          needDeps build
            $ map BS.pack
            $ concatMap snd
            $ parseMakefile
            $ BS.unpack depsrc
          -- correct as per the Ninja spec, but breaks --skip-commands
          -- when (deps == "gcc") $ liftIO $ removeFile depfile


needDeps :: Ninja -> Build -> [Str] -> Action ()
needDeps (MkNinja {..}) = \build xs -> do
  -- eta reduced so 'builds' is shared
  opts <- getShakeOptions
  let dontLint = isNothing $ shakeLint opts
  if dontLint then needBS xs else do
    neededBS xs
    -- now try and statically validate needed will never fail
    -- first find which dependencies are generated files
    xs <- pure $ filter (\b -> HM.member b builds) xs
    -- now try and find them as dependencies
    let bad = xs `difference` allDependencies build
    case bad of
      []    -> pure ()
      (x:_) -> liftIO $ errorIO $ BS.unpack $ mconcat
               [ "Lint checking error in ", x, ": "
               , "file in deps is generated and not a pre-dependency"
               ]
  where
    builds :: HashMap FileStr Build
    builds = HM.fromList $ singles ++ [(x,y) | (xs,y) <- multiples, x <- xs]

    -- do list difference, assuming a small initial set, most of which occurs
    -- early in the list
    difference :: [Str] -> [Str] -> [Str]
    difference [] ys = []
    difference xs ys = f (HS.fromList xs) ys
      where
        f xs []              = HS.toList xs
        f xs (y:ys) | y ∈ xs = let xs2 = HS.delete y xs
                               in (if HS.null xs2 then [] else f xs2 ys)
        f xs (y:ys)          = f xs ys

    -- find all dependencies of a rule, no duplicates, with all dependencies of
    -- this rule listed first
    allDependencies :: Build -> [FileStr]
    allDependencies rule = f HS.empty [] [rule]
      where
        f _    []     []                 = []
        f seen []     (x:xs)             = let paths = map filepathNormalise
                                                       $ mconcat
                                                       [ depsNormal x
                                                       , depsImplicit x
                                                       , depsOrderOnly x
                                                       ]
                                           in f seen paths xs
        f seen (x:xs) rest   | x ∈ seen  = f seen xs rest
                             | otherwise = let seen' = HS.insert x seen
                                               rest' = (++ rest)
                                                       $ maybeToList
                                                       $ HM.lookup x builds
                                           in x : f seen' xs rest'

    (∈) :: (Eq a, Hashable a) => a -> HashSet a -> Bool
    (∈) = HS.member

applyRspfile :: Env Str Str -> Action a -> Action a
applyRspfile env act = do
  rspfile <- liftIO (BS.unpack <$> askVar env "rspfile")
  rspfile_content <- liftIO (askVar env "rspfile_content")
  if rspfile == ""
    then act
    else (liftIO (BS.writeFile rspfile rspfile_content) >> act)
         `actionFinally`
         ignore (removeFile rspfile)

parseShowIncludes :: Str -> Str -> [FileStr]
parseShowIncludes prefix out = do
  x <- BS.lines out
  guard (prefix `BS.isPrefixOf` x)
  let y = BS.dropWhile isSpace $ BS.drop (BS.length prefix) x
  guard (not (isSystemInclude y))
  pure y

-- Dodgy, but ported over from the original Ninja
isSystemInclude :: FileStr -> Bool
isSystemInclude x = (    BS.isInfixOf bsProgFiles tx
                      || BS.isInfixOf bsVisStudio tx
                    )
  where
    tx = BS8.map (\c -> if c >= 97 then c - 32 else c) x
    -- optimised toUpper that only cares about letters and spaces

bsProgFiles, bsVisStudio :: Str
bsProgFiles = BS.pack "PROGRAM FILES"
bsVisStudio = BS.pack "MICROSOFT VISUAL STUDIO"

data CompDB
  = MkCompDB
    { cdbDirectory :: !String
    , cdbCommand   :: !String
    , cdbFile      :: !String
    }
  deriving (Show)

printCompDB :: [CompDB] -> String
printCompDB xs = unlines $ ["["] ++ concat (zipWith f [1..] xs) ++ ["]"]
  where
    n = length xs
    f i (MkCompDB {..}) = [ "  {"
                          , "    \"directory\": " <> g cdbDirectory ++ ","
                          , "    \"command\": " <> g cdbCommand ++ ","
                          , "    \"file\": " <> g cdbFile
                          , "  }" <> (if i == n then "" else ",") ]
    g = show

toCommand :: String -> ([CmdOption], String, [String])
toCommand s
  -- On POSIX, Ninja does a /bin/sh -c, and so does Haskell in Shell mode.
  | not isWindows
  = ([Shell], s, [])
  -- On Windows, Ninja passes the string directly to CreateProcess,
  -- but Haskell applies some escaping first. We try and get back as close to
  -- the original as we can, but it's very hacky
  | length s < 8000
  -- Using the "cmd" program adds overhead (I measure 7ms), and a limit of
  -- 8191 characters, but is the most robust, requiring no additional escaping.
  = ([Shell], s, [])
  | (cmd, s) <- word1 s
  , map toUpper cmd `elem` ["CMD","CMD.EXE"]
  , ("/c", s) <- word1 s
  -- Given "cmd.exe /c <something>" we translate to Shell, which adds cmd.exe
  -- (looked up on the current path) and /c to the front.
  -- CMake uses this rule a lot.
  -- Adding quotes around pieces are /c goes very wrong.
  = ([Shell], s, [])
  | otherwise
  -- It's a long command line which doesn't call "cmd /c".
  -- We reverse the escaping Haskell applies, but each argument will still gain
  -- quotes around it.
  = let xs = splitArgs s in ([], head $ xs ++ [""], drop 1 xs)


data State
  = Gap  -- ^ Current in the gap between words
  | Word -- ^ Currently inside a space-separated argument
  | Quot -- ^ Currently inside a quote-surrounded argument

-- | The process package contains a translate function, reproduced below.
--   The aim is that after command line parsing we should get out mostly
--   the same answer.
splitArgs :: String -> [String]
splitArgs = f Gap
  where
    f Gap  (x:xs)    | isSpace x                   = f Gap xs
    f Gap  ('\"':xs)                               = f Quot xs
    f Gap  []                                      = []
    f Gap  xs                                      = f Word xs
    f Word (x:xs)    | isSpace x                   = [] : f Gap xs
    f Quot ('\"':xs)                               = [] : f Gap xs
    f s    ('\\':xs) | (a, b) <- span (== '\\') xs = g s (length a) b
    f s    (x:xs)                                  = add [x] $ f s xs
    f _    []                                      = [[]]

    g s a ('\"':xs) | even a = add (escape (a `div` 2) ++ "\"") (f s xs)
    g s a ('\"':xs)          = add (escape ((a + 1) `div` 2))   (f s ('\"':xs))
    g s a xs                 = add (escape (a + 1))             (f s xs)

    escape n = replicate n '\\'

    add a (b:c) = (a ++ b):c
    add a []    = [a]
