{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.STM
import Control.Concurrent (forkIO)
import Data.Attoparsec.ByteString.Char8 hiding (takeTill)
import Data.ByteString.Char8 (ByteString, pack, putStrLn)
import qualified Data.ByteString as S
import Data.Maybe (mapMaybe)
import Data.Map (singleton, findWithDefault, Map, insert, delete)
import Prelude hiding (lookup, take, putStrLn)
import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO (Handle, hSetBinaryMode, hSetBuffering, BufferMode(..))

version :: ByteString
version = "0.1.0.0"

type Key    = ByteString
type Value  = Maybe ByteString
type DB     = Map Key Value

data Command = Get Key
             | Set Key Value
             | Del [Key]
             | Unknown
             deriving (Eq, Show)

data Reply = Bulk (Maybe ByteString)
           | MultiBulk (Maybe [Reply])
           deriving (Eq, Show)

fromBulks :: [Reply] -> [ByteString]
fromBulks bulks = mapMaybe (\(Bulk mb) -> mb) bulks

parseReply :: Reply -> Maybe Command
parseReply (MultiBulk (Just xs)) =
  case xs of
    [Bulk (Just "get"), Bulk (Just a)]                -> Just $ Get a
    [Bulk (Just "set"), Bulk (Just a), Bulk (Just b)] -> Just $ Set a (Just b)
    ((Bulk (Just "del")):bulks)                       -> Just $ Del (fromBulks bulks)
    _                                                 -> Just Unknown
parseReply _ = Nothing

replyParser :: Parser Reply
replyParser = choice [bulk, multiBulk]

bulk :: Parser Reply
bulk = Bulk <$> do
    len <- char '$' *> signed decimal <* endOfLine
    if len < 0
        then return Nothing
        else Just <$> take len <* endOfLine

multiBulk :: Parser Reply
multiBulk = MultiBulk <$> do
    len <- char '*' *> signed decimal <* endOfLine
    if len < 0
        then return Nothing
        else Just <$> count len replyParser

hGetReplies :: Handle -> Parser a -> IO a
hGetReplies h parser = go S.empty
  where
    go rest = do
        parseResult <- parseWith readMore parser rest
        case parseResult of
            Fail _ _ s   -> error s
            Partial{}    -> error "error: partial"
            Done _ r     -> return r

    readMore = S.hGetSome h (4*1024)

crlf :: ByteString
crlf = "\r\n"

ok :: ByteString
ok = S.concat ["+OK", crlf]

err :: ByteString
err = S.concat ["-ERR ", "unknown command", crlf]

deleted :: Int -> ByteString
deleted rows = S.concat [":", (pack . show) rows, crlf]

get :: Value -> ByteString
get value = S.concat ["$", valLength value, crlf, valStr value, crlf]

valStr :: Value -> ByteString
valStr Nothing  = ""
valStr (Just a) = a

valLength :: Value -> ByteString
valLength Nothing  = "-1"
valLength (Just a) = (pack . show . S.length) a

sockHandler :: Socket -> TVar DB -> IO ()
sockHandler sock db = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    hSetBinaryMode handle True
    _ <- forkIO $ commandProcessor handle db
    sockHandler sock db

runCommand :: Handle -> Maybe Command -> TVar DB -> IO ()
runCommand handle (Just (Get key)) db = do
    m <- atomRead db
    let value = getValue m key
    S.hPutStr handle (get value)
    putStrLn $ S.concat ["get (", key, ", ", valStr value, ")"]

runCommand handle (Just (Set key value)) db = do
    updateValue (insert key value) db
    S.hPutStr handle ok
    putStrLn $ S.concat ["set (", key, ", ", valStr value, ")"]

runCommand handle (Just (Del keysToDelete)) db = do
    let len = length keysToDelete
    mapM_ (\key -> updateValue (delete key) db) keysToDelete
    S.hPutStr handle (deleted len)
    putStrLn $ S.concat ["del (", (pack . show) len, ")"]

runCommand handle (Just Unknown) _ = S.hPutStr handle err

runCommand _ Nothing _ = return ()

commandProcessor :: Handle -> TVar DB -> IO ()
commandProcessor handle db = do
  reply <- hGetReplies handle replyParser
  let command = parseReply reply
  runCommand handle command db

atomRead :: TVar a -> IO a
atomRead = atomically . readTVar

updateValue :: (DB -> DB) -> TVar DB -> IO ()
updateValue fn x = atomically $ modifyTVar x fn

getValue :: DB -> Key -> Value
getValue db k = findWithDefault Nothing k db

main :: IO ()
main = withSocketsDo $ do
    database <- atomically $ newTVar $ singleton "__version__" (Just version)
    sock <- listenOn $ PortNumber 7777
    putStrLn "Listening on localhost 7777"
    sockHandler sock database
