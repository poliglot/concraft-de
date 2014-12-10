{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NLP.Concraft.German.MorphAnalysis(
	analyseParagraph
) where

import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Lazy as L

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8

import           NLP.Concraft.German.Morphosyntax hiding (restore)
import qualified NLP.Concraft.German.Format.Plain as Plain

readResponse :: Socket -> IO String
readResponse conn = do
	msg <- recv conn 4096
	let msgStr = B8.unpack msg
	if ((B8.length msg) < 4096) then return msgStr else do
		next <- readResponse conn
		return $ msgStr ++ next

requestParagraph :: Socket -> T.Text -> IO String
requestParagraph sock paragraph = do
	send sock (TextEncoding.encodeUtf8 paragraph)
	readResponse sock

-- From https://gist.github.com/1100407/77f43a49bb68bd7817f6fcae6b661275ac19c0d7
request :: String -> Int -> T.Text -> IO String
request host port paragraph = withSocketsDo $ do
	addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
	let serverAddr = head addrInfo
	sock <- socket (addrFamily serverAddr) Stream defaultProtocol
	connect sock (addrAddress serverAddr)
	response <- requestParagraph sock paragraph
	sClose sock
	return response

analyseParagraph :: String -> Int -> T.Text -> IO [Sent Tag]
analyseParagraph host port paragraph = do
	let filteredParagraph = T.filter C.isPrint paragraph
	response <- request host port filteredParagraph
	return $ Plain.parsePara (L.pack response)
