{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}

module Main where

import Options.Applicative.Simple

newtype Options = Options
  { verbose :: Bool
  -- ,
  --   hello :: String,
  --   enthusiasm :: Int
  }

newtype XagOptControl = XagOptControl
  { xagOptControlVerbose :: Bool
  }

getCtlVerbose :: (HasXagOptControl) => Bool
getCtlVerbose = xagOptControlVerbose ?xagOptControl

type HasXagOptControl = (?xagOptControl :: XagOptControl)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> switch
      ( long "verbose"
          <> short 'v'
          <> help "Enable verbose logging"
      )

-- <*> strOption
--   ( long "hello"
--       <> metavar "TARGET"
--       <> help "Target for the greeting"
--   )
-- <*> option
--   auto
--   ( long "enthusiasm"
--       <> help "How enthusiastically to greet"
--       <> showDefault
--       <> value 1
--       <> metavar "INT"
--   )

doHello :: (HasXagOptControl) => IO ()
doHello = do
  if getCtlVerbose
    then putStrLn "Hellooooo!"
    else putStrLn "Hi"

main :: IO ()
main = do
  (options, ()) <-
    simpleOptions
      "Version 0.1"
      "xag-opt: optimize an XOR-AND graph"
      "" -- description
      optionsParser
      empty
  let ?xagOptControl = XagOptControl {xagOptControlVerbose = verbose options}
   in doHello
