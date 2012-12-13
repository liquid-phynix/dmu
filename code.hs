{-# LANGUAGE OverloadedStrings, 
             TemplateHaskell, 
             TypeFamilies, 
             MultiParamTypeClasses, 
             QuasiQuotes, 
             FlexibleContexts, 
             GADTs,
             ScopedTypeVariables #-}

{-
TODO:
  J login page override
  J jquery theming  
  J fix Blob  
  - compression
  J datatables input as json
  J save header too, when csv export
  - incremental table creation
  - fit content to window size
  - faster json generation
  - search bar for each column
  - better json
  - better postgresql
-}

import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.HashDB
import Database.Persist.Sqlite
import Data.Text (Text)
import qualified Data.Text as T

import qualified Database.HDBC as P
import qualified Database.HDBC.PostgreSQL as P

import Control.Monad
import Control.Monad.Trans (liftIO)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Text.JSON as J
import qualified Data.Aeson as A
import System.IO
import System.Environment (getArgs)
import qualified Data.List as L

import Data.Enumerator.List (consume)
-- import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai -- (Response (ResponseSource))
import Network.HTTP.Types (status200)


staticFiles "static"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Person
  username Text
  password Text
  salt Text
  UniquePerson username
  deriving Show
|]

data MyAuthSite = MyAuthSite { getAuthConn :: Connection
                             , getPConn :: P.Connection
                             , getDateIntervalStatement :: P.Statement
                             , getStatic :: Static
                             , getFieldNames :: [String] }

mkYesod "MyAuthSite" [parseRoutes|
/ RootR GET
/query QueryR POST
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
--        aplogin input        
        toWidget [lucius|
body,html { margin:0px; padding:0px; height:100%; width:100%; }
body {
  background-color:#E9E9E9;
  background-image:url(@{StaticR bbody_png});
  display:table;
}                  
##{header} {
  background-color : #363434;
  background-image:url(@{StaticR hbody_png});
  color:white;

  display:table-cell;
  vertical-align:middle;
  height:60px;
  text-align:center;

  position:relative;
}
##{login} {
  display:table-cell;
  vertical-align:middle;
  width:50%;
}
##{logintable} { margin:auto; }
##{logo} { position:absolute; right:5px; top:5px }
##{title} { float:center; }
|]

        [whamlet|
<div style="display: table-row;">
  <div ##{header}>
    <div ##{title}> <h3> Log in to DMU
    <div ##{logo}> <img src=@{StaticR logo_png}>
<div style="display: table-row;">
  <div ##{login}>
    <form method=post action="/auth/page/hashdb/login">
      <table ##{logintable}>
        <tr>
          <th>Username:
          <td>
            <input id="x" name="username" autofocus="" required>
        <tr>
          <th>Password:
          <td>
            <input type="password" name="password" required>
        <tr>
          <td>&nbsp;
          <td>
            <input type="submit" value="Login">
      <script> if (!("autofocus" in document.createElement("input"))) {document.getElementById("x").focus();}

|]

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

selectByDateInterval :: MyAuthSite -> String -> String -> Handler B.ByteString
selectByDateInterval site from to = do
  _ <- liftIO $ P.execute stmt [P.toSql from, P.toSql to]
  results <- liftIO $ P.fetchAllRows stmt
  return $ A.encode $ A.object 
    [ "header" A..= getFieldNames site
    , "body" A..= body (take 100 results) ]
  where stmt = getDateIntervalStatement site
        body :: [[P.SqlValue]] -> [[B.ByteString]]
        body results = map (map P.fromSql) results

postQueryR :: Handler RepJson
postQueryR = do
  site <- getYesod
  from <- runInputPost $ ireq textField "from"
  to <- runInputPost $ ireq textField "to"
  query <- selectByDateInterval site (T.unpack from) (T.unpack to)
  return $ RepJson $ toContent $ query

getRootR :: Handler RepHtml
getRootR = do
  site <- getYesod
  session <- getSession
  
  let (id :: Int) = read $ BS.unpack $ session M.! "_ID"
  Just person <- runDB $ get (Key (toPersistValue id) :: PersonId)

  let rows :: [[String]] = []
  
  [ header, logo, datatable, logoutid, content, csv, thead, select, from, to, datepicker, results ] <- replicateM 12 newIdent
  
  defaultLayout $ do
    setTitle "DMU"
    
    toWidgetHead [hamlet| <link rel=stylesheet href=@{StaticR css_jquery_ui_css}> |]
    toWidgetHead [hamlet| <link rel=stylesheet href=@{StaticR css_jquery_dataTables_css}> |]
    -- toWidgetHead [hamlet| <link rel=stylesheet href=@{StaticR jquerydataTablesthemeroller_css}> |]
    toWidgetHead [hamlet| <script src=@{StaticR js_jquery_1_6_4_min_js}> |]    
    toWidgetHead [hamlet| <script src=@{StaticR js_jquery_ui_1_9_2_custom_min_js}> |]
    toWidgetHead [hamlet| <script src=@{StaticR js_jquery_dataTables_min_js}> |]
    toWidgetHead [hamlet| <script src=@{StaticR js_FixedHeader_min_js}> |]
    
    toWidget [lucius|
body,html { margin:0px; padding:0px; height:100%; width:100%; }
body { background-color:#E9E9E9; background-image:url(@{StaticR bbody_png}); display:table; }
h1 { text-align:center; }
p,h1 { font-size:16px; }
.ui-red-button { background: #da4b4b; }
.ui-black-button { background: #0C0C0C; color:white; }
##{header} {
background-color:#363434;
background-image:url(@{StaticR hbody_png}); 
color:white; 
display:table-cell; 
vertical-align:middle; 
height:60px; 
text-align:center; 
position:relative; 
}
##{content} { display:table-cell; vertical-align:middle; width:100%; }
##{logo} { position:absolute; right:5px; top:5px }
##{logoutid} { float:left; left:10px; }
##{thead} { background-color: #92A6FA; }
##{datepicker} { float:left; margin-left:30px; }
##{datatable} { width:100%; }
|]

    [whamlet|

<div style="display: table-row;">
  <div ##{header}>
    <button ##{logoutid} onclick="document.location.href='@{AuthR LogoutR}';"> Log out user: #{personUsername person}

    <div ##{datepicker}>
      <button ##{select}>Select
      from
      <input ##{from} type=text style="width: 20%;" readonly>
      to
      <input ##{to} type=text style="width: 20%;" readonly>

    <button ##{csv}>CSV
    <div ##{logo}> <img src=@{StaticR logo_png}>
<div style="display: table-row;">
  <div ##{content}>
    <h1 ##{results}>
    <table ##{datatable}> 
      <thead ##{thead}>
        $forall header <- (getFieldNames site)
          <th> #{header}
      $forall row <- rows
        <tr>
          $forall col <- row
            <td>
              #{col}

|]

    toWidgetHead [julius|

window.screen.availWidth = 500;

var dtable;
$(document).ready(function (){
  const MIME_TYPE = 'text/plain';                     
                     
  $('##{logoutid}').button();
  $('##{csv}').button();
  $('##{select}').button();
  $('##{from}').button();
  $('##{to}').button();
  $('##{logoutid}').removeClass('ui-corner-all');
  $('##{csv}').removeClass('ui-corner-all');
  $('##{select}').removeClass('ui-corner-all');
  $('##{from}').removeClass('ui-corner-all');
  $('##{to}').removeClass('ui-corner-all');

  $('##{logoutid}').addClass('ui-red-button');

// setting initial date on date selectors
  setInitialDate();
  



  dtable = $('##{datatable}').dataTable({ 'bPaginate':false });
  dtable.header = [];
  
  $('##{datatable}_info').css('position','absolute');
  $('##{datatable}_info').css('top','0px');    
//  new FixedHeader(dtable); //, { 'bottom':true });

  dtable.$('tr').click( function () {
    var data = dtable.fnGetData( this );
    alert(data);
  } );

  function tableToCsv(){
    var data = dtable.fnGetData();
    var out = dtable.header.join(';') + '\n';
    for(r in data){ out += data[r].join(';') + '\n'; }
    return out;
  };

  $('##{csv}').click(function(){
    window.URL = window.webkitURL || window.URL;

    var a = document.createElement('a');
    a.hidden = true;
    a.download = 'table.csv';
    a.href = window.URL.createObjectURL(new Blob([tableToCsv()], {type:MIME_TYPE}));
    a.textContent = '';
    a.dataset.downloadurl = [MIME_TYPE, a.download, a.href].join(':');
    a.click();
  });
  
  function setInitialDate(){
    var date = new Date();
    var dateString = date.getFullYear().toString() + '-' + (date.getMonth() + 1).toString() + '-' + date.getDate().toString();
    document.getElementById('#{from}').value = dateString;
    document.getElementById('#{to}').value = dateString;
    $('##{from}').datepicker({ "changeMonth":true, "changeYear":true, "dateFormat":"yy-mm-dd" });
    $('##{to}').datepicker({ "changeMonth":true, "changeYear":true, "dateFormat":"yy-mm-dd"  });
    $('##{results}').text("Records from " + dateString + " to " + dateString);
  }

  document.getElementById('#{select}').onclick = function(){
    $('##{select}').addClass('ui-black-button');

    var xhr = new XMLHttpRequest();
    xhr.multipart = true;
    xhr.open("POST", "@{QueryR}", true);
    xhr.setRequestHeader("Content-type","application/x-www-form-urlencoded");
    var from = $('##{from}').val();
    var to = $('##{to}').val();
    xhr.send("from=" + from + "&to=" + to);
    
    xhr.onreadystatechange = function(){
      if(xhr.readyState == 4){ // && xhr.status == 200){
        var table_contents = JSON.parse(xhr.responseText);
//        console.log(xhr.responseText);
        dtable.fnClearTable();
        dtable = $('##{datatable}').dataTable({
          'bDeferRender': true,
          'bPaginate':false,
          'bDestroy': true,
          'aaData':table_contents.body,
          'aoColumns':table_contents.header.map(function(title){ return {"sTitle":title}; })
        });
        dtable.header = table_contents.header;
        $('##{datatable}').css('width', '100%');
        $('##{datatable}_info').css('position','absolute');
        $('##{datatable}_info').css('top','0px');
        $('##{results}').text("Records from " + from + " to " + to);
      }
      $('##{select}').removeClass('ui-black-button');
    }
  };
});

|]


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
    statement <- P.prepare pconn $ 
                 "SELECT " 
                 ++ L.concat (L.intersperse "," field_names)  
                 ++ " FROM radometer_results WHERE date_short >= ? and date_short <= ?"
    
    static' <- static "static"
    -- Yesod.Dispatch.toWaiApp
    warpDebug 12345 $ MyAuthSite { getAuthConn = conn 
                                 , getPConn = pconn
                                 , getDateIntervalStatement = statement
                                 , getStatic = static'
                                 , getFieldNames = field_names }
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
