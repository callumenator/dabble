
window.onkeydown = function() { return handleWindowEvent(event); };

var editor, history, engine, browser;
var browserAction = null, browserStatus = '';
var historyBuffer = [];
var maxHistory = 200;
var lineIndex = 0; // for moving through the history
var lineWidgetCount = 0;

/**
* Trim string proto
*/
if (typeof(String.prototype.trim) === "undefined") {
    String.prototype.trim = function() {
        return String(this).replace(/^\s+|\s+$/g, '');
    };
}


/**
* Uniq array proto
*/
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


/**
* Autocomplete
*/
CodeMirror.commands.autocomplete = function(cm) {
    CodeMirror.showHint(cm, CodeMirror.dHint, {
        async: true,
        completeSingle: false
    });
}


/**
* On-load setup
*/
$(document).ready(function () {

    require('fs').watch('css/style.css', function (event, name) {
        var queryString = '?reload=' + new Date().getTime();
        $('link[rel="stylesheet"]').each(function () {
            this.href = this.href.replace(/\?.*|$/, queryString);
        });
    });

    editor = CodeMirror.fromTextArea(document.getElementById("code"), {
        mode: "text/x-d",
        viewportMargin: Infinity,
        lineWrapping: true,
        smartIndent: false,
        extraKeys: { "Ctrl-Down": "autocomplete" }
    });

    history = CodeMirror.fromTextArea(document.getElementById("history"), {
        mode: "text/x-d",
        viewportMargin: Infinity,
        readOnly: true,
        lineNumbers: true,
        lineWrapping: true
    });

    editor.setOption("theme", "dabble");
    history.setOption("theme", "dabble");

    editor.options.onKeyEvent = function (cm, e) {

        // If currently auto-completing, do nothing
        if (e.ctrlKey == true) return;
        if (editor.state.completionActive) return;
        if (!e || !(e instanceof KeyboardEvent)) return;

        if (e.type == 'keydown' && e.shiftKey == false && e.keyCode == 13) {            
            var text = editor.getValue();
            preparseInput(text);
            e.preventDefault();
            return true;
        } else if (e.type == 'keydown' &&
                   editor.lineCount() == 1 &&
                   (e.keyCode == 38 || e.keyCode == 40)) {

            var line = "";
            if (e.keyCode == 38) line = retrieveHistory('up');
            else line = retrieveHistory('down');
            if (typeof line != "undefined")
                editor.setValue(line);
        }
    };


    (function () {

        CodeMirror.dHint = function (editor, callback, options) {
            var cursor = editor.getCursor();
            var tk = editor.getTokenAt(cursor);

            browserAction = function (json) {
                if (json.length > 0) {
                    callback({
                        list: json,
                        from: CodeMirror.Pos(cursor.line, tk.start),
                        to: CodeMirror.Pos(cursor.line, tk.end)
                    });
                }
            };

            browser.stdin.write(new Buffer('suggest-names:' + tk.string.toLowerCase() + '\n'));
        };
    } ());


    /**
    * Start repl
    */
    engine = require('child_process').spawn('repl.exe', ['--noConsole'], { cwd: '../../bin' });
    engine.stdout.once('data', function (data) { updateResult(data.toString(), false); });
    

    /**
    * Start browser
    */
    browser = require('child_process').spawn('browser.exe', ['c:/cal/d/dmd2/src/phobos/std'], { cwd: '../../bin' });

    browser.stdout.on('data', function (data) {
        if (browserAction !== null) {
            try {
                var json = JSON.parse(data.toString());
                browserAction(json);
            } catch (error) {
                console.log(error, data.toString(), data);
            }
        } else {
            browserStatus = 'data.toString()';
        }
    });

});


/**
* Shortcuts
*/
function handleWindowEvent(e) {
    if (event.ctrlKey == true) {
        if (event.keyCode == 70) {
            toggleSearchPaneVisibility();
            return false;
        }
    }
    return true;
}


/**
* 
*/
function preparseInput(text) {    
    if (text.trim() == "clear") {
        history.setValue("");
        send("version", function(text) { updateResult(text, false);  });
    } else {
        updateHistory(text);        
        send(text);
    }
    editor.setValue("");    
}


/**
* History lookup
*/
function retrieveHistory(dir) {
    if (dir == 'up') {
        if (lineIndex > 0)
            lineIndex--;
    } else if (dir == 'down') {
        if (lineIndex < historyBuffer.length - 1)
            lineIndex++;
    }
    return historyBuffer[lineIndex];
}


/**
* Got a result from the repl
*/
function updateResult(data, lwidget) {
    if (lwidget) {
        if (data.trim().length == 0)
            return;        
        data = data.replace(/(\r\r\n|\r\n|\n|\r)/g, "<br>");       
        var id = "lineWidget" + (lineWidgetCount++).toString();
        $("body").append("<div class='resultWidget' id='" + id + "'>" + data, + "</div>");
        history.addLineWidget(history.lineCount() - 1, $("#" + id)[0]);
    } else {
        cmAppend(history, data);
    }
    var el = document.getElementById("codePane");
    el.scrollTop = el.scrollHeight;    
}


function cmAppend(cm, text) {
    text = text.replace(/(\r\r\n|\r\n|\r)/g, "\n");
    text = text.split("\n").filter(function (el) { return el.length; }).join("\n")    
    if (cm.getValue() !== "") text = "\n" + text;
    cm.replaceRange(text, CodeMirror.Pos(cm.lastLine()));            
}

/**
* Add last input to history
*/
function updateHistory(text) {
    if (text.length == 0) return;
    cmAppend(history, text);
    if (historyBuffer.length > 1 && historyBuffer[historyBuffer.length-1] == text) return;
    historyBuffer.push(text);
    if (historyBuffer.length > maxHistory)
        for(var i = 0; i < 10; i++)
            historyBuffer.shift();                    
    lineIndex = historyBuffer.length;
}


/**
* Send input to the repl
*/
function send(text, callback) {   
    if (callback === undefined) {
        engine.stdout.once('data', function (data) { updateResult(data.toString(), true); });
    } else {        
        engine.stdout.once('data', function (data) { callback(data.toString()); });
    }    
    engine.stdin.write(new Buffer(text + "\n"));
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


function searchInput() {
    var prefix = document.getElementById("searchBox").value;

    if (prefix.length == 0) {
        clearSuggestions();
        return;
    }                                     
      
    browserAction = function(json) {
        if (json.length == 0) clearSuggestions();
        else $("#suggestionsPane").html(listToHTML(json));                    
    };
  
    browser.stdin.write(new Buffer('search-names:' + prefix.toLowerCase() + "\n"));   
}

function listToHTML(list) {
    var html = "";
    list.forEach( function(e) {
        html+="<div class='libPanel libPanelLevel1' data-expanded='false' onclick='panelClick(\""+e.uuid+"\")'>"+e.name+"</div>";
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


function expandPanel(uuid, parent) {    
    browserAction = function(json) {        
        $(parent).append(symbolToHTML(json));        
        parent.dataset.expanded = "true";
    };     
    browser.stdin.write(new Buffer('get-uuid:' + uuid + "\n"));        
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
    "<br>" + 
    symbol.comment.replace(/\n/g, '<br>') +  
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
    $("#suggestionsPane").html("");
    $("#searchBoxUnder").val("");
}




