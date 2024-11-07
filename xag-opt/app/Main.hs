module Main where

import Options.Applicative.Simple

data Options = Options
  { verbose :: Bool
  -- ,
  --   hello :: String,
  --   enthusiasm :: Int
  } deriving (Show)

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

main :: IO ()
main = do
  (options, ()) <-
    simpleOptions
      "Version 0.1"
      "xag-opt: optimize an XOR-AND graph"
      "" -- description
      optionsParser
      empty
  print $ "options: " ++ show options
