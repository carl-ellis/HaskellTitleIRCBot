-- Toy bot in Haskell, Author Carl Ellis 2011

import Network
import System.IO
import Text.Printf
import Data.List
import System.Exit
import Network.Curl
import Text.Regex.Posix
import Text.Regex.Base.RegexLike
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BSC
import Web.Encodings

server  = "irc.freenode.org"
port    = 6667

-- Remember to fill this in!
chan    = ""
nick    = ""

_LIMIT = 200

pattern = makeRegexOpts (defaultCompOpt - compNewline) defaultExecOpt ".*<title>(.+)</title>.*"

-- Conenct to a server and print out what ever gets sent
main = do
  h <-  connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  write h "NICK" nick
  write h "USER" (nick++" 0 * :haskell title bot")
  write h "JOIN" chan
  listen h

-- Handles writing to the stream handle
write :: Handle -> String -> String -> IO()
write h s t = do
  hPrintf h "%s %s\r\n" s t
  printf    "> %s %s\n" s t

-- listens for input
listen :: Handle -> IO()
listen h = forever $ do
  t <- hGetLine h
  let s = init t
  if ping s then pong s
  else eval h (clean s)
  putStrLn s
  where
    forever a     = a >> forever a
    clean         = drop 1. dropWhile(/= ':') . drop 1
    ping x        = "PING :" `isPrefixOf` x
    pong x        = write h "PONG" (':' : drop 6 x)

-- Handles bot commands
eval :: Handle -> String -> IO()
eval h "!quit1234"                  = write h "QUIT" ":Exiting" >> exitWith ExitSuccess
eval h x 
  | "!say" `isPrefixOf` x       = privmsg h (drop 4 x)
  | "http://" `isPrefixOf` x    = grabtitle h (takeWhile (/= ' ') x)
eval _ _                        = return () -- ignore anything else

-- Sends amessage to the channel
privmsg :: Handle -> String -> IO()
privmsg h s = write h "PRIVMSG" (chan ++ " :" ++ s)

-- Will grab the title of a link
grabtitle :: Handle -> String -> IO()
grabtitle h s = do { header <- curlHead s [];
                     printtitle h s (getContent $ getFields header)}

printtitle :: Handle -> String -> String -> IO()
printtitle h url "text/html" =  do 
                                  --resp <- curlGetResponse url []
                                  resp <- curlGetString url []
                                  --let title = getTitle (respBody resp)
                                  let title = take _LIMIT $ getTitle (snd resp)
                                  putStrLn title
                                  if title /= "" then
                                    privmsg h ("Title: " ++ title )
                                  else
                                    return ()
printtitle h url _          = return ()

-- Gets the content data
getContent :: [(String, String)] -> String
getContent []                           = "Eh"
getContent (x:xs) 
          | fst x == "Content-Type"    = tail(takeWhile (/= ';') (snd x))
          | otherwise                   = getContent xs

-- Gets the field
getFields :: (String, [(String, String)]) -> [(String, String)]
getFields (_, field) = field

-- Gets the title
getTitle :: String -> String
getTitle html
        | result == []  = ""
        | otherwise     = dropWhile (<= '1') $ BSC.unpack $ BSC.pack $ decodeHtml $ T.unpack $ T.strip $ T.pack $ stripNewLine $ last(head(result))
        where
          result = match pattern html :: [[String]]

--Strips out newlines
stripNewLine :: [Char] -> [Char]
stripNewLine ""           = ""
stripNewLine (x:xs)
            | x == '\n'   = ' ' : stripNewLine xs
            | otherwise   = x : stripNewLine xs 
