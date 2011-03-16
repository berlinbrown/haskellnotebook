--
-- Laughing Man Project v0.0
-- Copyright (C) 2003, 2007 Berlin Brown
-- 11/26/2007
--
-- Description:
-- The laughing man project is a simple program/bot api
-- for communicating with users on web user forums through
-- the use of simple HTTP requests.
--  
-- You will need to set an environment variable
-- 'hs_http_proxy' if you are behind a web proxy.
--
-- shell$ export hs_http_proxy=http://THE_PROXY:9999
-- 
-- Libraries Required:
-- HTTP LIBRARY version: HTTP-3001.0.2
--
-- To compile this haskell module (or use 'make'):
-- ghc --make -package HTTP ManClient.hs
--
-- Based on get source from
-- http://www.haskell.org/http/
-- by bjorn at bringert.net
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
-- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import Data.Char (intToDigit)
import Data.Maybe

import Network.HTTP
import Network.URI

-- Due to modification to HTTP feaures, I am using a custom Browser.hs
-- library.
import NewHttpBrowser (browse, Proxy(..), setProxy, request)

-- Use the pre-existing Network.Browser HTTP library
--import Network.Browser (defaultGETRequest, request,
--                        setErrHandler, setOutHandler,
--                        browse, setProxy, Proxy(..))

import System.Environment (getArgs, getEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

forumConnectURI :: URI
forumConnectURI = fromJust $ parseURI "http://reddit.com/api/login"

userAgentMimicIE = "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0; .NET CLR 1.1.4322; .NET CLR 2.0.50727; InfoPath.1)"

forumRequestBody :: String
forumRequestBody = "uh="
                   ++ "&op=login_main"
                   ++ "&user_login_main=laughingman03"
                   ++ "&passwd_login_main=PASSWORD"
                   ++ "&_="

-- Forum request data to send
-- field=op login_main
-- field=user_login_main
-- field=passwd_login_main

main = do args <- getArgs
          proxy_setting <- getEnv "hs_http_proxy"
          putStrLn $ "Proxy Setting>>>" ++ proxy_setting
	  cont <- get forumConnectURI
          putStr cont          

err :: String -> IO a
err msg = do 
	  hPutStrLn stderr msg
	  exitFailure

ignoreMsg :: String -> IO ()
ignoreMsg _ = return ()

--
-- Simple HTTP request with support for proxying
simpleHTTPProxy :: Request -> IO (Result Response)
simpleHTTPProxy req = do
     proxy <- catch
                 (getEnv "hs_http_proxy" >>= return . (flip Proxy Nothing))
                 (\_ -> return NoProxy)
     (_, resp) <- browse (setProxy proxy >> request req)
     return (Right resp)

get :: URI -> IO String
get uri =
    do
    eresp <- simpleHTTPProxy (manRequest uri)
    resp <- handleErr (err . show) eresp
    case rspCode resp of
                      (2,0,0) -> return (rspBody resp)
                      _ -> err (httpError resp)
    where
    showRspCode (a,b,c) = map intToDigit [a,b,c]
    httpError resp = 
        showRspCode (rspCode resp) ++ " " ++ rspReason resp

--
-- Make a laughing man POST request to the forum URL, 
-- also set the content length and new user agent.
manRequest :: URI -> Request
manRequest uri = Request { rqURI = uri,
                       rqMethod = POST,
                       rqHeaders = [ 
                           Header HdrContentLength 
                                      (show (length forumRequestBody)),
                           Header HdrUserAgent userAgentMimicIE
                          ],
                       rqBody = forumRequestBody }

--
-- Handle Connection Errors
handleErr :: Monad m => (ConnError -> m a) -> Either ConnError a -> m a
handleErr h (Left e) = h e
handleErr _ (Right v) = return v

-- End of File

