module CmdParams (
    cmdLineParser,
    Params,
    windowSize,
    pointCount,
    useLinear,
    useLagrang,
) where

import Options.Applicative

data Params = Params
    { windowSize :: Int
    , pointCount :: Int
    , useLinear :: Bool
    , useLagrang :: Bool
    -- , method :: Maybe String
    }

mkParams :: Parser Params
mkParams =
    Params
        <$> argument auto (metavar "WINDOW_SIZE" <> help "The number of points in the window")
        <*> argument auto (metavar "POINT_COUNT" <> help "The number of calucalted points in the window")
        -- <*> optional (strArgument (metavar "METHOD" <> help "Name of used method (if not present, then use both)"))
        <*> switch (long "linear" <> short 'i' <> help "compute by linear method (if no method, then use all)")
        <*> switch (long "lagrang" <> short 'a' <> help "compute by lagrang method (if no method, then use all)")

cmdLineParser :: IO Params
cmdLineParser = execParser opts
  where
    opts =
        info
            (mkParams <**> helper)
            (fullDesc <> progDesc "Interpolation with streaming processing")
