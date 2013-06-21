
Array.prototype.getUnique = function(selector){
   var u = {}, a = [];
   for(var i = 0, l = this.length; i < l; ++i){
      if(u.hasOwnProperty(selector(this[i]))) {
         continue;
      }
      a.push(this[i]);
      u[selector(this[i])] = 1;
   }
   return a;
}


function toggleSearchPaneVisibility() {
    
    var sp = document.getElementById('libPane'), 
        cp = document.getElementById('codePane');    
    
    if (sp.style.display == "")
        sp.style.display = "none";
        
    if (sp.style.display == "inline-block") {
        
        sp.style.display = "none";
        cp.style.width = "99%"; 
        editor.focus();
        
    } else if (sp.style.display == "none") {        
        
        cp.style.width = "40%";        
        setTimeout(function() {
            sp.style.display = "inline-block";
            sp.style.width = "58%";
            document.getElementById('searchBox').focus();
        }, 200);                                  
    }
}



function post(to, data, callback) {
    var xhr = createCORSRequest('POST', to);
    xhr.setRequestHeader('Content-type', 'text/plain;');
    xhr.onload = function() { callback(xhr); }; 
    xhr.send(data);
}



function searchInput() {
  var prefix = document.getElementById("searchBox").value;

  if (prefix.length == 0) {
      clearSuggestions();
      return;
  }                                     
  
  post('/suggest', prefix.toLowerCase(), function(xhr) {
          if (xhr.responseText.length == 0) {
              clearSuggestions();
          } else {
              var el = document.getElementById("suggestionsPane");
              var newEl = el.cloneNode(false);
              newEl.innerHTML = listToHTML(xhr.responseText);
              el.parentNode.replaceChild(newEl, el);          
          } 
  });         
}

function listToHTML(list) {
    var html = "";
    list.split(",").forEach( function(e) {
            html+="<div class='libPanel libPanelLevel1' data-expanded='false' onclick='panelClick(\""+e+"\")'>"+e+"</div>";
    });
    return html;
}

function panelClick(name) {
   
    var el = event.target;        
   
    if (typeof el.dataset.expanded == "undefined")
        return;
   
    if (el.dataset.expanded != "true")
        expandPanel(name, el);
    else
        collapsePanel(name, el);
}


function expandPanel(name, parent) {    
 
    
    post('/getSymbolsByName', name, function(xhr) {
            
        var html = "", 
            symbols = jsonParse(xhr.responseText),
            div = document.createElement("div");        
        
        symbols.forEach(function(sym) {
                html += symbolToHTML(sym);
        });       
                        
        parent.appendChild(div);
        div.innerHTML = html;  
        parent.dataset.expanded = "true";
        
    });
}

function collapsePanel(name, parent) {
  parent.removeChild(parent.firstChild.nextSibling); 
  parent.dataset.expanded = "false";
}

function symbolToHTML(symbol) {  
  return "<div class='libPanel libPanelLevel2'>" +
    symbol.parent + 
    "<br>" +   
    symbol.pretty +  
    /*"<div class='libPanelSrcButton' onclick='showSource(\""+symbol.uuid+"\")'>source</div>" +*/ 
    "</div>";    
}

function showSource(uuid) {
    var parent = event.target;
    
    post('/getSymbolSourceByUUID', uuid, function(xhr) {
            
        var html = xhr.responseText,             
            div = document.createElement("div");                  
                        
         console.log(xhr.responseText);
            
        parent.appendChild(div);
        div.innerHTML = html;          
    });
}



function clearSuggestions() {
  document.getElementById("suggestionsPane").innerHTML = "";
  document.getElementById("searchBoxUnder").value = "";
}

(function () {
        
  CodeMirror.dHint = function(editor, callback, options) {
      
    function selector(node) {
      var results = [];
      node.symbols
        .getUnique(function(s){ return s.parent; })
        .forEach(function(sym) {
          results.push({text:sym.name, displayText:sym.name + " (" + sym.parent + ")"});
        });
      return results;
    }
      
    var cursor = editor.getCursor();
    var tk = editor.getTokenAt(cursor);    
    post('/suggest', tk.string.toLowerCase(), function(xhr) {
        var list = xhr.responseText.split(",");                        
        callback(
            {
                list:list, 
                from:CodeMirror.Pos(cursor.line, tk.start), 
                to:CodeMirror.Pos(cursor.line, tk.end)
            });        
        });        
  };

}());