
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