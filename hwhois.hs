import Data.Maybe (fromMaybe)
import Network.Whois
import Options.Applicative

data WhoisOptions = WhoisOptions {
  host :: String
} deriving Show

parse :: Parser WhoisOptions
parse = WhoisOptions
        <$> argument str
            ( metavar "HOST"
           <> help "The host to whois" )

runWhois :: WhoisOptions -> IO ()
runWhois (WhoisOptions h) = do
    x <- whois h
    putStrLn $ unlines $ fmap (fromMaybe "") [fst x, snd x]

opts :: ParserInfo WhoisOptions
opts = info (parse <**> helper)
       ( fullDesc
      <> header "hwhois - a very simple whois application" )

main :: IO ()
main = execParser opts >>= runWhois
