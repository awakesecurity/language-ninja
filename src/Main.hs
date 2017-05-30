{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Lens        as Lens

import           Data.Makefile       as Makefile
import qualified Data.Makefile.Parse as Makefile

import           Turtle

import           Control.Arrow
import           Data.Either
import           Data.Maybe

import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T

import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map

import           Data.Set            (Set)
import qualified Data.Set            as Set

import qualified Ninja.All           as Ninja
import qualified Ninja.Env           as Ninja
import qualified Ninja.Lexer         as Ninja
import qualified Ninja.Parse         as Ninja
import qualified Ninja.Pretty        as Ninja
import qualified Ninja.Type          as Ninja

main :: IO ()
main = pure ()
