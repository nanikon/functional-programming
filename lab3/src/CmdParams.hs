module CmdParams(
  cmdLineParser,
  Params,
) where

import Options.Applicative

data Params = Params {
      windowSize :: Int
    , pointCount :: Int
    , method :: Maybe String
}

mkParams :: Parser Params
mkParams = 
    Params <$>
               argument auto (metavar "WINDOW_SIZE" <> help "The number of points in the window")
           <*> argument auto (metavar "POINT_COUNT" <> help "The number of calucalted points in the window")
           <*> optional (strArgument (metavar "METHOD" <> help "Name of used method (if not present, then use both)"))

cmdLineParser :: IO Params 
cmdLineParser = execParser opts 
  where
    opts = info (mkParams <**> helper)
                (fullDesc <> progDesc "Interpolation with streaming processing")
