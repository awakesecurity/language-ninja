-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Shake.hs
--
-- License:
--     Copyright Neil Mitchell 2011-2017.
--     All rights reserved.
--
--     Redistribution and use in source and binary forms, with or without
--     modification, are permitted provided that the following conditions are
--     met:
--
--         * Redistributions of source code must retain the above copyright
--           notice, this list of conditions and the following disclaimer.
--
--         * Redistributions in binary form must reproduce the above
--           copyright notice, this list of conditions and the following
--           disclaimer in the documentation and/or other materials provided
--           with the distribution.
--
--         * Neither the name of Neil Mitchell nor the names of other
--           contributors may be used to endorse or promote products derived
--           from this software without specific prior written permission.
--
--     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--     "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--     LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--     A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--     OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--     SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
--     LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
--     DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
--     THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
--     (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
--     OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{-# OPTIONS_GHC #-}
{-# OPTIONS_HADDOCK #-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

-- |
--   Module      : Language.Ninja.Shake
--   Copyright   : Copyright 2011-2017 Neil Mitchell
--   License     : BSD3
--   Maintainer  : opensource@awakenetworks.com
--   Stability   : experimental
--
--   FIXME: doc
module Language.Ninja.Shake
  ( runNinja
  ) where

import           Data.Bifunctor
import           Data.Monoid

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.IO               as T

import qualified Data.ByteString            as BS

import           Development.Shake          (Action, Rules, (&?>), (?>))
import qualified Development.Shake          as Shake
import qualified Development.Shake.FilePath as Shake (normaliseEx, toStandard)
import qualified Development.Shake.Util     as Shake (parseMakefile)

import           Language.Ninja.Env         (Env)
import           Language.Ninja.Types

import qualified Language.Ninja.Env         as Ninja
import qualified Language.Ninja.Parse       as Ninja
import qualified Language.Ninja.Types       as Ninja

import           Control.Applicative
import           Control.Exception.Extra
import           Control.Monad
import           Control.Monad.IO.Class

import           Control.Lens               hiding ((.>), (<.), (<|), (|>))

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

import           Flow

--------------------------------------------------------------------------------
-- STUBS

filepathNormalise :: Text -> Text
filepathNormalise = T.unpack
                    .> Shake.normaliseEx
                    .> Shake.toStandard
                    .> T.pack

needT :: [Text] -> Action ()
needT = Shake.need . map T.unpack

neededT :: [Text] -> Action ()
neededT = Shake.needed . map T.unpack

orderOnlyT :: [Text] -> Action ()
orderOnlyT = Shake.orderOnly . map T.unpack

--------------------------------------------------------------------------------

-- | FIXME: doc
runNinja :: FilePath
         -> [String]
         -> Maybe String
         -> IO (Maybe (Rules ()))
runNinja file args tool = do
  options <- parseNinjaOptions file args tool
  ninjaDispatch options

data NinjaOptions
  = NinjaOptionsBuild   !PNinja ![Text]
  | NinjaOptionsCompDB  !PNinja ![Text]
  | NinjaOptionsUnknown !Text

parseNinjaOptions :: FilePath -> [String] -> Maybe String -> IO NinjaOptions
parseNinjaOptions file (map T.pack -> args) tool = do
  ninja <- Ninja.parse file
  pure $ case tool of
    Nothing         -> NinjaOptionsBuild  ninja args
    (Just "compdb") -> NinjaOptionsCompDB ninja args
    (Just    owise) -> NinjaOptionsUnknown (T.pack owise)

ninjaDispatch :: NinjaOptions -> IO (Maybe (Rules ()))
ninjaDispatch (NinjaOptionsBuild  n args) = ninjaBuild  n args
ninjaDispatch (NinjaOptionsCompDB n args) = ninjaCompDB n args
ninjaDispatch (NinjaOptionsUnknown tool)  = [ "Unknown tool: ", tool
                                            ] |> mconcat |> T.unpack |> errorIO

computeRuleEnv :: HashSet Text -> PBuild -> PRule -> Env Text Text
computeRuleEnv out b r = do
  let deps = b ^. pbuildDeps . pdepsNormal
  -- the order of adding new environment variables matters

  let compose :: [a -> a] -> (a -> a)
      compose = map Endo .> mconcat .> appEndo

  Ninja.scopeEnv (b ^. pbuildEnv)
    |> Ninja.addEnv "out"        (T.unwords (map quote (HS.toList out)))
    |> Ninja.addEnv "in"         (T.unwords (map quote (HS.toList deps)))
    |> Ninja.addEnv "in_newline" (T.unlines (HS.toList deps))
    |> compose (map (uncurry Ninja.addEnv) (HM.toList (b ^. pbuildBind)))
    |> Ninja.addBinds (HM.toList (r ^. pruleBind))

ninjaCompDB :: PNinja -> [Text] -> IO (Maybe (Rules ()))
ninjaCompDB ninja args = do
  dir <- getCurrentDirectory

  -- Compute the set of rules
  let argumentSet = HS.fromList args
  let inArgs rule = HS.member (fst rule) argumentSet
  let rules = HM.filterWithKey (curry inArgs) rules

  -- the build items are generated in reverse order, hence the reverse
  let itemsToBuild :: [(HashSet Text, PBuild, Text, PRule)]
      itemsToBuild = do
        let multiples = ninja ^. pninjaMultiples
        let singles = ninja ^. pninjaSingles
        (outputs, build) <- [ HM.toList multiples
                            , map (first HS.singleton) (HM.toList singles)
                            ] |> mconcat |> reverse
        (Just rule) <- [HM.lookup (build ^. pbuildRule) rules]
        (file:_) <- [build ^. pbuildDeps . pdepsNormal . to HS.toList]
        pure (outputs, build, file, rule)

  compDB <- forM itemsToBuild $ \(out, build, file, rule) -> do
    let env = computeRuleEnv out build rule
    let commandLine = T.unpack (Ninja.askVar env "command")
    let deps = build ^. pbuildDeps . pdepsNormal
    pure $ MkCompDB dir commandLine (T.unpack (head (HS.toList deps)))

  putStr $ printCompDB compDB

  pure Nothing

ninjaBuild :: PNinja -> [Text] -> IO (Maybe (Rules ()))
ninjaBuild ninja args = pure $ Just $ do
  let normMultiples = ninja
                      ^. pninjaMultiples
                      .  to HM.toList
                      .  to (fmap (first (HS.map filepathNormalise)))

  let poolToResource (name, depth) = Shake.newResource name depth

  poolList <- HM.traverseWithKey
              (\k v -> Shake.newResource (T.unpack k) v)
              (HM.insert "console" 1 (ninja ^. pninjaPools))

  let phonys    = ninja ^. pninjaPhonys
  let singles   = ninja ^. pninjaSingles
                  |> HM.toList
                  |> fmap (first filepathNormalise)
                  |> HM.fromList
  let multiples = [(x, (xs, b)) | (xs, b) <- normMultiples, x <- HS.toList xs]
                  |> HM.fromList
  let rules     = ninja ^. pninjaRules
  let pools     = poolList
  let defaults  = ninja ^. pninjaDefaults

  let build :: HashSet Text -> PBuild -> Action ()
      build = runBuild (needDeps ninja) phonys rules pools

  let targets :: HashSet Text
      targets | not (null args)        = HS.fromList args
              | not (HS.null defaults) = defaults
              | otherwise              = HS.fromList
                                         (HM.keys singles <> HM.keys multiples)

  Shake.action $ needT
    $ HS.toList $ HS.unions $ fmap (resolvePhony phonys) $ HS.toList targets

  let relatedOutputs :: FilePath -> Maybe [FilePath]
      relatedOutputs (T.pack -> out)
        = HM.lookup out multiples
          |> fmap (fst .> HS.toList)
          |> fromMaybe (if HM.member out singles then [out] else [])
          |> map T.unpack
          |> Just

  relatedOutputs &?> \(map T.pack -> outputs) -> do
    build (HS.fromList outputs) (snd (multiples HM.! head outputs))

  (flip HM.member singles . T.pack) ?> \(T.pack -> output) -> do
    build (HS.singleton output) (singles HM.! output)

resolvePhony :: HashMap Text (HashSet Text) -> Text -> HashSet Text
resolvePhony mp = f (Left 100)
  where
    f :: Either Int (HashSet Text) -> Text -> HashSet Text
    f (Left 0)   x          = f (Right HS.empty) x
    f (Right xs) x | x ∈ xs = [ "Recursive phony involving "
                              , T.unpack x
                              ] |> mconcat |> error
    f a          x          = case HS.toList <$> HM.lookup x mp of
                                Nothing -> HS.singleton x
                                Just xs -> bimap (subtract 1) (HS.insert x) a
                                           |> \a' -> HS.unions (map (f a') xs)

    (∈) :: (Eq a, Hashable a) => a -> HashSet a -> Bool
    (∈) = HS.member

quote :: Text -> Text
quote x | T.any isSpace x = let q = T.singleton '\"' in mconcat [q, x, q]
quote x                   = x


runBuild :: (PBuild -> [Text] -> Action ())
         -> HashMap Text (HashSet Text)
         -> HashMap Text PRule
         -> HashMap Text Shake.Resource
         -> HashSet Text
         -> PBuild
         -> Action ()
runBuild needD phonys rules pools out build = do
  let errorA :: Text -> Action a
      errorA = T.unpack .> errorIO .> liftIO

  let ruleName      = build ^. pbuildRule
  let depsNormal    = build ^. pbuildDeps . pdepsNormal
  let depsImplicit  = build ^. pbuildDeps . pdepsImplicit
  let depsOrderOnly = build ^. pbuildDeps . pdepsOrderOnly

  let setConcatMap :: (Eq b, Hashable b) => (a -> HashSet b) -> HashSet a -> [b]
      setConcatMap f = HS.map f .> HS.toList .> HS.unions .> HS.toList

  needT      $ setConcatMap (resolvePhony phonys) $ depsNormal <> depsImplicit
  orderOnlyT $ setConcatMap (resolvePhony phonys) depsOrderOnly

  let ruleNotFound = [ "Ninja rule named ", ruleName
                     , " is missing, required to build "
                     , T.unwords (HS.toList out)
                     ] |> mconcat |> errorA

  rule <- HM.lookup ruleName rules |> maybe ruleNotFound pure

  let env = computeRuleEnv out build rule

  applyRspfile env $ do
    let askVarString :: Text -> String
        askVarString = Ninja.askVar env .> T.unpack

    let commandline = askVarString     "command"
    let depfile     = askVarString     "depfile"
    let deps        = askVarString     "deps"
    let description = askVarString     "description"
    let pool        = Ninja.askVar env "pool"

    let withPool act = if T.null pool
                       then act
                       else case HM.lookup pool pools of
                              Nothing -> [ "Ninja pool named ", pool
                                         , " not found, required to build "
                                         , T.unwords (HS.toList out)
                                         ] |> mconcat |> errorA
                              (Just r) -> Shake.withResource r 1 act

    when (description /= "") $ Shake.putNormal description

    let (opts, prog, args) = toCommand commandline
      in if deps == "msvc"
         then do stdout <- withPool (Shake.command opts prog args)
                 let prefix = Ninja.askEnv env "msvc_deps_prefix"
                              |> fromMaybe "Note: including file: "
                 let outStr = T.pack (Shake.fromStdout stdout)
                 needD build (parseShowIncludes prefix outStr)
         else withPool (Shake.command_ opts prog args)

    when (depfile /= "") $ do
      when (deps /= "gcc") $ Shake.need [depfile]

      depsrc <- T.readFile depfile
                |> fmap T.unpack
                |> liftIO

      let parsed = map T.pack (concatMap snd (Shake.parseMakefile depsrc))

      needD build parsed

      -- correct as per the Ninja spec, but breaks --skip-commands
      -- when (deps == "gcc") $ liftIO $ removeFile depfile

needDeps :: PNinja -> PBuild -> [Text] -> Action ()
needDeps ninja = \build xs -> do
  let errorA :: Text -> Action a
      errorA = T.unpack .> errorIO .> liftIO
  -- eta reduced so 'builds' is shared
  opts <- Shake.getShakeOptions
  let dontLint = isNothing $ Shake.shakeLint opts
  if dontLint then needT xs else do
    neededT xs
    -- now try and statically validate needed will never fail
    -- first find which dependencies are generated files
    xs <- pure $ filter (\b -> HM.member b builds) xs
    -- now try and find them as dependencies
    let bad = xs `difference` allDependencies build
    case bad of
      []    -> pure ()
      (x:_) -> [ "Lint checking error in ", x, ": "
               , "file in deps is generated and not a pre-dependency"
               ] |> mconcat |> errorA
  where
    builds :: HashMap Text PBuild
    builds = let singles   = ninja ^. pninjaSingles
                 multiples = ninja ^. pninjaMultiples
             in singles <> explodeHM multiples

    explodeHM :: (Eq k, Hashable k) => HashMap (HashSet k) v -> HashMap k v
    explodeHM = HM.toList
                .> map (first HS.toList)
                .> explode
                .> HM.fromList

    explode :: [([k], v)] -> [(k, v)]
    explode m = [(x, y) | (xs, y) <- m, x <- xs]

    -- do list difference, assuming a small initial set, most of which occurs
    -- early in the list
    difference :: [Text] -> [Text] -> [Text]
    difference [] ys = []
    difference xs ys = f (HS.fromList xs) ys
      where
        f xs []              = HS.toList xs
        f xs (y:ys) | y ∈ xs = let xs2 = HS.delete y xs
                               in (if HS.null xs2 then [] else f xs2 ys)
        f xs (y:ys)          = f xs ys

    -- find all dependencies of a rule, no duplicates, with all dependencies of
    -- this rule listed first
    allDependencies :: PBuild -> [Text]
    allDependencies rule = f HS.empty [] [rule]
      where
        f _    []     []                = []
        f seen []     (x:xs)            = let fpNorm = filepathNormalise
                                              pdeps  = x ^. pbuildDeps
                                              deps   = [ pdeps ^. pdepsNormal
                                                       , pdeps ^. pdepsImplicit
                                                       , pdeps ^. pdepsOrderOnly
                                                       ] |> mconcat
                                              paths  = HS.toList deps
                                                       |> map fpNorm
                                          in f seen paths xs
        f seen (x:xs) rest   | x ∈ seen = f seen xs rest
        f seen (x:xs) rest              = let seen' = HS.insert x seen
                                              rest' = HM.lookup x builds
                                                      |> maybe rest (: rest)
                                          in x : f seen' xs rest'

    (∈) :: (Eq a, Hashable a) => a -> HashSet a -> Bool
    (∈) = HS.member

applyRspfile :: Env Text Text -> Action a -> Action a
applyRspfile env act = do
  let rspfile         = Ninja.askVar env "rspfile"
  let rspfile_content = Ninja.askVar env "rspfile_content"
  if rspfile == ""
    then act
    else (liftIO (T.writeFile (T.unpack rspfile) rspfile_content) >> act)
         `Shake.actionFinally`
         ignore (removeFile (T.unpack rspfile))

parseShowIncludes :: Text -> Text -> [Text]
parseShowIncludes prefix out = do
  x <- T.lines out
  guard (prefix `T.isPrefixOf` x)
  let y = T.dropWhile isSpace $ T.drop (T.length prefix) x
  guard (not (isSystemInclude y))
  pure y

-- Dodgy, but ported over from the original Ninja
isSystemInclude :: Text -> Bool
isSystemInclude (T.encodeUtf8 -> x)
  = (    BS.isInfixOf "PROGRAM FILES"           tx
      || BS.isInfixOf "MICROSOFT VISUAL STUDIO" tx
    )
  where
    tx = BS.map (\c -> if c >= 97 then c - 32 else c) x
    -- optimised toUpper that only cares about letters and spaces

data CompDB
  = MkCompDB
    { cdbDirectory :: !String
    , cdbCommand   :: !String
    , cdbFile      :: !String
    }
  deriving (Eq, Show)

printCompDB :: [CompDB] -> String
printCompDB xs = unlines (["["] <> concat (zipWith f [1..] xs) <> ["]"])
  where
    n = length xs
    f i (MkCompDB {..}) = [ "  {"
                          , "    \"directory\": " <> g cdbDirectory <> ","
                          , "    \"command\": " <> g cdbCommand <> ","
                          , "    \"file\": " <> g cdbFile
                          , "  }" <> (if i == n then "" else ",") ]
    g = show

toCommand :: String -> ([Shake.CmdOption], String, [String])
toCommand s
  -- On POSIX, Ninja does a /bin/sh -c, and so does Haskell in Shell mode.
  | not isWindows
  = ([Shake.Shell], s, [])
  -- On Windows, Ninja passes the string directly to CreateProcess,
  -- but Haskell applies some escaping first. We try and get back as close to
  -- the original as we can, but it's very hacky
  | length s < 8000
  -- Using the "cmd" program adds overhead (I measure 7ms), and a limit of
  -- 8191 characters, but is the most robust, requiring no additional escaping.
  = ([Shake.Shell], s, [])
  | (cmd, s') <- word1 s
  , map toUpper cmd `elem` ["CMD","CMD.EXE"]
  , ("/c", s'') <- word1 s'
  -- Given "cmd.exe /c <something>" we translate to Shell, which adds cmd.exe
  -- (looked up on the current path) and /c to the front.
  -- CMake uses this rule a lot.
  -- Adding quotes around pieces are /c goes very wrong.
  = ([Shake.Shell], s'', [])
  | otherwise
  -- It's a long command line which doesn't call "cmd /c".
  -- We reverse the escaping Haskell applies, but each argument will still gain
  -- quotes around it.
  = let xs = splitArgs s in ([], head (xs <> [""]), drop 1 xs)

-- | The state while splitting arguments.
data State
  = Gap  -- ^ Currently in the gap between words
  | Word -- ^ Currently inside a space-separated argument
  | Quot -- ^ Currently inside a quote-surrounded argument

-- | The @process@ package contains a translate function, reproduced below.
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

    g s a ('\"':xs) | even a = add (escape (a `div` 2) <> "\"") (f s xs)
    g s a ('\"':xs)          = add (escape ((a + 1) `div` 2))   (f s ('\"':xs))
    g s a xs                 = add (escape (a + 1))             (f s xs)

    escape n = replicate n '\\'

    add :: String -> [String] -> [String]
    add a (b:c) = (a <> b) : c
    add a []    = [a]
