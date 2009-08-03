-- compile with 'ghc --make udp.hs'
module Main () where
import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Applicative -- for <*> and <$>

type HandlerFunc = SockAddr -> String -> IO ()

serveUDP :: String -> HandlerFunc -> IO ()
serveUDP port handlerfunc = withSocketsDo $
         do
                addrinfos <- getAddrInfo
                             (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                             Nothing (Just port)
                let serveraddr = head addrinfos
                
                sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
                bindSocket sock (addrAddress serveraddr)

                procMessages sock

         where procMessages sock = do
               (msg, _, addr) <- recvFrom sock 10240
               handlerfunc addr msg
               procMessages sock

plainHandler :: HandlerFunc
plainHandler addr msg = do
             putStrLn (show p)
             putStrLn $ "From " ++ show addr ++ " length: " ++ show (length msg) ++ " seqno: " ++ show (seqno p)
             sendResp addr "8801" (seqno p)
             where p = decode (L.pack msg)

sendResp :: SockAddr -> String -> Word32 -> IO ()
sendResp addr port ackno = 
  do
        sock <- socket AF_INET Datagram defaultProtocol
        n <- sendTo sock p addr
        return ()
  where p = L.unpack $ encode $ Packet 0x8301 0 0 ackno 0 0 0 0 0 
         
main = serveUDP "8801" plainHandler

data Packet = Packet {
     idstr :: Word16,
     typ :: Word8,
     count :: Word8,
     seqno :: Word32,
     md5h :: Word64,
     md5l :: Word64,
     encoding :: Word8,
     flags :: Word8,
     version :: Word16
} deriving (Show, Eq)

instance Binary Packet where
         get = Packet <$> 
             get <*>
             get <*>
             get <*>
             get <*>
             get <*>
             get <*>
             get <*>
             get <*>
             get
--         get = do
--              idstr <- getWord16le
--              typ <- get :: Get Word8
--              count <- get :: Get Word8
--              seqno <- getWord32le
--              md5h <- getWord64le
--              md5l <- getWord64le
--              encoding <- get :: Get Word8
--              flags <- get :: Get Word8
--              version <- getWord16le
--              return (Packet idstr typ count seqno md5h md5l encoding flags 
         
         put (Packet idstr _ _ s _ _ _ _ _) = 
             putWord16le idstr >> 
             put (1 :: Word8) >>
             put (0 :: Word8) >>
             putWord32le s >>
             putWord16le 1 >> -- result location
             putWord16le 2 >> -- eoi version
             putWord16le 200 >> -- RESP
             putWord8 17 >> -- HTTP v1.1
             putWord8 0 >> -- encoding
             putWord32le 0 >> -- lastmodified
             putWord32le 0 >> -- expires
             putWord32le 0 >> -- max_ages
             putWord32le 0 >> -- date
             putWord32le 0 >> -- length
             putWord8 0 >> -- flag1
             putWord8 0 >> -- flag2
             putWord8 0x98 >> -- signature valid mark
             putWord8 0 >> -- num_vary
             putWord32le 0 >> -- vary_type
             putWord64le 0 >> -- vary md5
             putWord64le 0 >> -- etag1
             putWord64le 0 >> -- etag2
             putWord64le 0 >> -- etag3
             putWord64le 0 -- etag4

