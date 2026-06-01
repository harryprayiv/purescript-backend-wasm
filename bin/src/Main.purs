module Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Data.Array as Array
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console as Console
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Version as Version

type BuildOption =
  { input :: FilePath
  , outDir :: FilePath
  }

buildOptionsParser :: ArgParser BuildOption
buildOptionsParser =
  ArgParser.fromRecord
    { input:
        ArgParser.argument [ "-I", "--input" ]
          "Path to input directory containing PureScript compilers artifacs (namely, corefn.json and externs.cbor)\n\
          \Defaults to './outout'."
          # ArgParser.default (Path.concat [ ".", "outpu" ])
    , outDir:
        ArgParser.argument [ "-o" ]
          "The output directory bundled wasm placed in."
          # ArgParser.default (Path.concat [ ".", "output-wasm" ])
    }

data Command = Build BuildOption

commandParser :: ArgParser Command
commandParser =
  ArgParser.choose "command"
    [ ArgParser.command [ "build" ]
        "Build wasm module from PureScript compiler's intermediate artifacts"
        do
          Build <$> buildOptionsParser <* ArgParser.flagHelp
    ]
    <* ArgParser.flagHelp
    <* ArgParser.flagInfo [ "--version", "-v" ] "Show version" Version.versionString

parseArgs :: Effect (Either ArgParser.ArgError Command)
parseArgs = do
  cliArgs <- Array.drop 2 <$> Process.argv
  pure $ ArgParser.parseArgs "purs-backend-wasm"
    "A PureScript backend for WebAssembly (with GC)"
    commandParser
    cliArgs

main :: FilePath -> Effect Unit
main cliRoot = do
  parseArgs >>= case _ of
    Left err ->
      Console.error $ ArgParser.printArgError err
    Right (Build args) -> launchAff_ do
      buildCmd args

  where
  buildCmd :: BuildOption -> Aff _
  buildCmd args = do
    Console.logShow cliRoot
    Console.logShow args
    pure unit