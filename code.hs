{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies, MultiParamTypeClasses, QuasiQuotes, FlexibleContexts, GADTs, ScopedTypeVariables #-}

{- TODO:
  J login page override
  J jquery theming  
  J fix Blob  
  J compression
  J datatables input as json
  J save header too, when csv export
  - incremental table creation
  - fit content to window size
  J faster json generation
  - search bar for each column
  J better json
  J better postgresql
  - table columns crop text
-}

-- generic
import Control.Monad
import Control.Monad.Trans (liftIO)
import qualified Data.Map as M
import qualified Data.List as L
import Data.Enumerator.List (consume)
import Data.IORef
-- yesod
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.HashDB
-- dbase
import Database.Persist.Sqlite
import qualified Database.HDBC as P
import qualified Database.HDBC.PostgreSQL as P
-- text
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text (Text)
import qualified Text.JSON as J
import Text.Hamlet (hamletFile)
import Text.Lucius (luciusFile)
import Text.Julius (juliusFile)
--import qualified Data.Aeson as A
-- system
import System.IO
import System.Environment (getArgs)
-- wai
import Network.Wai
import Network.HTTP.Types (status200)
import qualified Network.Wai.Handler.Warp as W

staticFiles "static"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Person
  username Text
  password Text
  salt Text
  UniquePerson username
  deriving Show
|]

data MyAuthSite =
  MyAuthSite { getAuthConn :: Connection
             , getPConn :: P.Connection
             , getDateIntervalStatement :: P.Statement
             , getStatic :: Static
             , getFieldNames :: [String]
             , getRestOfQuery :: IORef (M.Map Text [P.SqlValue]) }

mkYesod "MyAuthSite" [parseRoutes|
/ RootR GET
/query QueryR POST
/query2 Query2R POST
/auth AuthR Auth getAuth
/static StaticR Static getStatic
|]

instance Yesod MyAuthSite where
  approot = ApprootStatic "" --"http://localhost:3000"
  authRoute _ = Just (AuthR LoginR)
  isAuthorized h _ | h == RootR || h == QueryR = do
    mauth <- maybeAuthId
    return $ case mauth of
      Nothing -> AuthenticationRequired
      Just _ -> Authorized
  isAuthorized _ _ = return Authorized
  defaultLayout widget = do
    pc <- widgetToPageContent widget
    hamletToRepHtml [hamlet|
$doctype 5
<html>
  <head>
    <title>#{pageTitle pc}
    <meta charset=utf-8>
    <style>body { font-family:verdana; font-size:14px; }
    ^{pageHead pc}
  <body>
    ^{pageBody pc}
|]   

authPlugin = 
  case authHashDB (Just . UniquePerson) of
    AuthPlugin apname apdispatch aplogin ->
      AuthPlugin apname apdispatch $ \input -> do
        [header, logo, title, login, logintable] <- lift $ replicateM 5 newIdent
        let html = $(hamletFile "login.hamlet")
            css = $(luciusFile "login.css")
        toWidget css
        [whamlet| ^{html} |]

instance YesodAuth MyAuthSite where
    type AuthId MyAuthSite = PersonId
    loginDest _ = RootR
    logoutDest _ = RootR    
    getAuthId = getAuthIdHashDB AuthR (Just . UniquePerson)
    authPlugins _ = [authPlugin]
    authHttpManager = error "no need for http manager"

instance YesodPersist MyAuthSite where
    type YesodPersistBackend MyAuthSite = SqlPersist
    runDB f = do
      site <- getYesod
      runSqlConn f (getAuthConn site)

instance HashDBUser (PersonGeneric backend) where
  userPasswordHash = Just . personPassword
  userPasswordSalt = Just . personSalt
  setSaltAndPasswordHash s h p = p { personSalt = s
                                   , personPassword = h }

instance RenderMessage MyAuthSite FormMessage where
  renderMessage _ _ = defaultFormMessage

-- turn query into json in sql
selectByDateInterval :: MyAuthSite -> String -> String -> Handler B.ByteString
selectByDateInterval site from to = do
  uname <- getUsername
  liftIO $ P.execute stmt [P.toSql from, P.toSql to]
  results <- liftIO $ fmap (map head) $ P.fetchAllRows stmt
  let firstPart = take 100 results
      secondPart = drop 100 results
  liftIO $ atomicModifyIORef' (getRestOfQuery site) (\m -> (M.insert uname secondPart m, ()))
  return $ B.concat
    [ "{ \"header\" : " 
    , B.pack $ show $ getFieldNames site
    , ", \"body\" : " 
    , "["
    , B.intercalate "," $ map fs firstPart
    , "] }" ]
  where fs :: P.SqlValue -> B.ByteString
        fs s = P.fromSql s
        stmt = getDateIntervalStatement site

postQueryR :: Handler RepPlain
postQueryR = do
  site <- getYesod
  from <- runInputPost $ ireq textField "from"
  to <- runInputPost $ ireq textField "to"
  query <- selectByDateInterval site (T.unpack from) (T.unpack to)
  return $ RepPlain $ toContent $ query

postQuery2R :: Handler RepPlain
postQuery2R = do
  site <- getYesod
  uname <- getUsername
  secondPart <- liftIO $ atomicModifyIORef' (getRestOfQuery site) (\m -> (M.delete uname m, m M.! uname))
  return $ RepPlain $ toContent $ B.concat
    [ "["
    , B.intercalate "," $ map fs secondPart
    , "]" ]
  where fs :: P.SqlValue -> B.ByteString
        fs s = P.fromSql s

getUsername = do
  session <- getSession
  let (id :: Int) = read $ BS.unpack $ session M.! "_ID"
  Just person <- runDB $ get (Key (toPersistValue id) :: PersonId)
  return $ personUsername person

getRootR :: Handler RepHtml
getRootR = do
  site <- getYesod
  uname <- getUsername
  
  let rows :: [[String]] = []
  
  [ header, logo, datatable, logoutid, content, csv, thead, select, from, to, datepicker, results ] <- replicateM 12 newIdent
  let css = $(luciusFile "main.css")
      html = $(hamletFile "main.hamlet")
      js = $(juliusFile "main.js")
  defaultLayout $ do
    setTitle "DMU"
    toWidgetHead css
    toWidgetHead [hamlet| <link rel=stylesheet href=@{StaticR css_jquery_ui_css}> |]
    toWidgetHead [hamlet| <link rel=stylesheet href=@{StaticR css_jquery_dataTables_css}> |]
    toWidgetHead [hamlet| <script src=@{StaticR js_jquery_1_6_4_min_js}> |]    
    toWidgetHead [hamlet| <script src=@{StaticR js_jquery_ui_1_9_2_custom_min_js}> |]
    toWidgetHead [hamlet| <script src=@{StaticR js_jquery_dataTables_min_js}> |]
    toWidgetHead [hamlet| <script src=@{StaticR js_FixedHeader_min_js}> |]
    toWidgetHead js
    [whamlet| ^{html} |]

main :: IO ()
main = do
  args <- getArgs
  let user = case args of
        [] -> "mcstar"
        [u] -> u
  field_names <- getFieldNames "config.json"
  withSqliteConn "auth.db3" $ \conn -> do
    runSqlConn (runMigration migrateAll) conn    
--    pconn <- P.connect "127.0.0.1 : 5432 " "radosys" user ""
    pconn <- P.connectPostgreSQL $ "hostaddr=127.0.0.1 port=5432 dbname=radosys user=" ++ user
    stmt <- P.prepare pconn $ 
            "SELECT array_to_json(array[" 
            ++ L.concat (L.intersperse "," (map (\fn -> "cast(" ++ fn ++ " as text)") field_names))
            ++ "]) FROM radometer_results WHERE date_short >= ? and date_short <= ?"
    
    static' <- static "static"
    ioref <- newIORef M.empty
    warpDebug 12345 $ MyAuthSite { getAuthConn = conn 
                                 , getPConn = pconn
                                 , getDateIntervalStatement = stmt
                                 , getStatic = static'
                                 , getFieldNames = field_names
                                 , getRestOfQuery = ioref }
    -- app <- toWaiApp $ MyAuthSite { getAuthConn = conn 
    --                              , getPConn = pconn
    --                              , getDateIntervalStatement = statement
    --                              , getStatic = static'
    --                              , getFieldNames = field_names }
    -- W.run 12345 app

    P.disconnect pconn
  where getFieldNames :: String -> IO [String]
        getFieldNames fn = do
          contents <- readFile fn
          case J.decode contents of 
            J.Error s -> error "json decode failed on config.json" 
            J.Ok jobj -> 
              case lookup "fields" (J.fromJSObject jobj) of
                Just field_names -> return $ map T.unpack field_names
                Nothing -> error "no attribute 'fields' in config.json"
