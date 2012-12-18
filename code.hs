{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies, MultiParamTypeClasses, QuasiQuotes, FlexibleContexts, GADTs, ScopedTypeVariables #-}

{- TODO:
  J login page override
  J jquery theming  
  J fix Blob  
  J compression
  J datatables input as json
  J save header too, when csv export
  J incremental table creation
  - fit content to window size
  J faster json generation
  J search bar for each column
  J better json
  J better postgresql
  - table columns crop text
  - fix search box width
  - send default query automatically at start
-}

-- generic
import Control.Monad
import Control.Monad.Trans (liftIO)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.HashMap.Strict as HM
import Data.IORef
-- yesod
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.HashDB
-- dbase
import qualified Database.Persist.Sqlite as S
import qualified Database.HDBC as P
import qualified Database.HDBC.PostgreSQL as P
-- text
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
-- import Data.Text (Text)
--import qualified Text.JSON as J
import Text.Hamlet (hamletFile)
import Text.Lucius (luciusFileReload)
import qualified Text.Julius as TJ
import qualified Data.Aeson as A
-- system
import System.IO
import Text.Printf (printf)
--import System.Environment (getArgs)
-- wai
import Network.Wai
import Network.HTTP.Types (status200)
import qualified Network.Wai.Handler.Warp as W

staticFiles "static"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Person
  username T.Text
  password T.Text
  salt T.Text
  UniquePerson username
  deriving Show
|]

data MyAuthSite =
  MyAuthSite { getAuthConn :: S.Connection
             , getPConn :: P.Connection
             , getDateIntervalStatement :: P.Statement
             , getStatic :: Static
             , getFieldNames :: [String]
             , getRestOfQuery :: IORef (M.Map T.Text [P.SqlValue]) }

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
  isAuthorized h _ | h == RootR || h == QueryR || h == Query2R = do
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
            css = $(luciusFileReload "login.css")
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
    type YesodPersistBackend MyAuthSite = S.SqlPersist
    runDB f = do
      site <- getYesod
      S.runSqlConn f (getAuthConn site)

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
  Just person <- runDB $ S.get (S.Key (S.toPersistValue id) :: PersonId)
  return $ personUsername person

getRootR :: Handler RepHtml
getRootR = do
  site <- getYesod
  uname <- getUsername
  
  let rows :: [[String]] = []
  
  [ header, logo, datatable, logoutid, content, csv, thead, select, from, to, datepicker, results ] <- replicateM 12 newIdent
  let css = $(luciusFileReload "main.css")
      html = $(hamletFile "main.hamlet")
      mainjs = $(TJ.juliusFileReload "main.js")
      savejs = $(TJ.juliusFileReload "save.js")
  defaultLayout $ do
    setTitle "DMU"
    toWidgetHead css
    toWidgetHead [hamlet| <link rel=stylesheet href=@{StaticR css_jquery_ui_css}> |]
    toWidgetHead [hamlet| <link rel=stylesheet href=@{StaticR css_jquery_dataTables_css}> |]
    toWidgetHead [hamlet| <script src=@{StaticR js_jquery_1_6_4_min_js}> |]    
    toWidgetHead [hamlet| <script src=@{StaticR js_jquery_ui_1_9_2_custom_min_js}> |]
    toWidgetHead [hamlet| <script src=@{StaticR js_jquery_dataTables_min_js}> |]
    toWidgetHead [hamlet| <script src=@{StaticR js_FixedHeader_min_js}> |]
    toWidgetHead [hamlet| <script src=@{StaticR js_jquery_dataTables_columnFilter_js}> |]
    toWidgetHead savejs
    toWidgetHead mainjs
    [whamlet| ^{html} |]

instance TJ.ToJavascript T.Text where
  toJavascript = TJ.toJavascript . TJ.rawJS

main :: IO ()
main = do
  config <- do
    contents <- BL.readFile "config.json"
    case A.decode contents of 
      Nothing -> error "json decode failed on config.json"
      Just (A.Object hm) -> return hm

  let field_names = getFromConfig config "fields"
      user = getFromConfig config "user"
      password = getFromConfig config "password"
  
  S.withSqliteConn "auth.db3" $ \conn -> do
    S.runSqlConn (S.runMigration migrateAll) conn    

    pconn <- P.connectPostgreSQL (printf "hostaddr=127.0.0.1 port=5432 dbname=radosys user=%s password=%s" (T.unpack user) (T.unpack password))
    P.run pconn "DROP FUNCTION IF EXISTS a_to_j(text[]);" []
    P.run pconn array_to_json []
    
    stmt <- P.prepare pconn $ 
            "SELECT a_to_j(array[" 
            ++ L.concat (L.intersperse "," (map (\fn -> "cast(" ++ fn ++ " as text)") field_names))
            ++ "]) FROM radometer_results WHERE date_short >= ? and date_short <= ?"
    
    static' <- static "static"
    ioref <- newIORef M.empty
    -- warpDebug 12345 $ MyAuthSite { getAuthConn = conn 
    --                              , getPConn = pconn
    --                              , getDateIntervalStatement = stmt
    --                              , getStatic = static'
    --                              , getFieldNames = field_names
    --                              , getRestOfQuery = ioref }
    
    app <- toWaiApp $ MyAuthSite { getAuthConn = conn 
                                 , getPConn = pconn
                                 , getDateIntervalStatement = stmt
                                 , getStatic = static'
                                 , getFieldNames = field_names
                                 , getRestOfQuery = ioref }
    W.run 12345 app
    

    P.disconnect pconn
  where -- getFromConfig :: A.Object -> Text -> a
        getFromConfig obj asc =
          case  HM.lookup asc obj of 
            Nothing -> error (printf "no attribute %s in config.json" (T.unpack asc))
            Just res -> case A.fromJSON res of
              A.Error s -> error s
              A.Success a -> a
        array_to_json = "CREATE OR REPLACE FUNCTION a_to_j(in text[], out text) AS $$ SELECT E'[\"' || array_to_string($1,E'\",\"') || E'\"]' $$ LANGUAGE SQL;"
--          "CREATE OR REPLACE FUNCTION a_to_j(ar text[]) RETURNS text AS $$ BEGIN RETURN '[\"' || array_to_string(ar,'\",\"') || '\"]'; END; $$ LANGUAGE plpgsql;"