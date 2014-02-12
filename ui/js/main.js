
var editor, history, engine, browser;
var browserAction = null, browserStatus = '';
var historyBuffer = [];
var maxHistory = 200; 
var lineIndex = 0; // for moving through the history
var lineWidgetCount = 0;
var resultWindow = null;
var autocompleteCode = "";
var handlers = {};

var autocomplete = {
    callback: null,
    token: null, 
    cursor: 0
};

/** 
* Init global settings. 
*/
var globalSettings = {
	phobosPath: phobosPath(),
	autocompleteOn: true, 
	autocompleteMinLength: 4
};


/**
* Restore settings from localStorage if present. 
*/
if (localStorage.hasOwnProperty("globalSettings"))		
	globalSettings = JSON.parse(localStorage.globalSettings);		


/**
* On close, store settings. 
*/
require('nw.gui').Window.get().on('close', function() {		
	localStorage.globalSettings = JSON.stringify(globalSettings);
	this.close(true);
});


/** 
* Trim string proto. 
*/
if (typeof(String.prototype.trim) === "undefined") {
    String.prototype.trim = function() {
        return String(this).replace(/^\s+|\s+$/g, '');
    };
}


/** 
* Uniq array proto. 
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
* Autocomplete. 
*/
/**
CodeMirror.commands.autocomplete = function(cm) {
    CodeMirror.showHint(cm, dcdHint, {
        async: true,
        completeSingle: false
    });
}
**/

/**
* Devtools shortcut 
*/
shortcut.add("Ctrl+Shift+J",function() { 
    require('nw.gui').Window.get().showDevTools();
});    


/**
* Fullscreen toggle shortcut 
*/
shortcut.add("Ctrl+M",function() { 
    require('nw.gui').Window.get().toggleFullscreen();
});    


/**
* Doc search toggle shortcut 
*/
shortcut.add("Ctrl+H",function() { 
    togglePane('docsearch-pane');
});    


/** 
* Options toggle shortcut 
*/
shortcut.add("Ctrl+O",function() { 
    togglePane('settings-pane');
});    


/**
* On-load setup 
*/
$(document).ready(function () {
		
	$("#repl-status").html("Initializing...");
	
	initPanes();	
	initSettingsEditor();
	monitorStylesheet();
	initCodemirrors();    
    initRepl();
	initBrowser();
        
});

/** 
* Monitor stylesheet for changes .
*/
function monitorStylesheet() {
    require('fs').watch('../dabble/ui/css/style.css', function (event, name) {
        var queryString = '?reload=' + new Date().getTime();
        $('link[rel="stylesheet"]').each(function () {
            this.href = this.href.replace(/\?.*|$/, queryString);
        });		
    });
}

/** 
* Initialize codemirror editors. 
*/
function initCodemirrors() {
	editor = CodeMirror.fromTextArea(document.getElementById("code"), {
        mode: "text/x-d",
        viewportMargin: Infinity,
        lineWrapping: true,
        smartIndent: false        
    });	
	editor.setOption("readOnly", true); // disable until repl is started

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
	
		if (!e || !(e instanceof KeyboardEvent)) return;        		
		var completing = false;
		if ('completionActive' in editor.state && editor.state.completionActive == true && $(".CodeMirror-hints").length)
			completing = true;								
        if (e.ctrlKey == true || completing) return;		
				        				
        if (e.type == 'keydown' && e.shiftKey == false && e.keyCode == 13) { // enter key press
            replInput();	
			e.preventDefault();
            return true;
        } else if (e.type == 'keydown' && editor.lineCount() == 1 && (e.keyCode == 38 || e.keyCode == 40)) { // arrow up/down
            var line = "";
            e.keyCode == 38 ? line = retrieveHistory('up') : line = retrieveHistory('down');
            if (typeof line != "undefined") {
                editor.setValue(line);				
				setTimeout( function() { editor.setCursor(editor.lineCount(), 0); }, 50);								
			}
        } else {
			if (globalSettings.autocompleteOn && editor.getTokenAt(editor.getCursor()).string.length >= globalSettings.autocompleteMinLength) {	
				console.log("New auto-complete");
				CodeMirror.showHint(editor, dcdHint, { async: true,	completeSingle: false });
			}
		}
    };
}

/** 
* Start repl. 
*/
function initRepl() {
	var repl_buffer = {data:""};
	engine = require('child_process').spawn('../dabble/bin/repl', ['--noConsole']);		
	engine.stdout.on('data', function (data) { 				
		var messages = messageProtocol(data, repl_buffer);		
		if (messages.length == 0) return;					
		for(var i = 0; m = messages[i], i < messages.length; i++) 
			handleMessage(m);								
	});	
    send("version");
}

/**
* Start browser. 
*/
function initBrowser() {
	var browser_buffer = {data:""};
	browser = require('child_process').spawn('../dabble/bin/browser');	        		
	browser.stdout.on('data', function (data) {					
		var messages = messageProtocol(data, browser_buffer);		
		if (messages.length == 0) return;			
		for(var i = 0; m = messages[i], i < messages.length; i++) {
			if (m.hasOwnProperty("status")) {
				browserStatus = m.status;
				console.log(browserStatus);
			}
			else if (browserAction !== null)		
				browserAction(m);								
		}
    });	
	if (globalSettings.phobosPath !== undefined && globalSettings.phobosPath != "")
		browser.stdin.write(new Buffer("phobos-path:" + globalSettings.phobosPath + "\u0006"));
}


/**
* Handle child process messages.
*/
function messageProtocol(incomming, buffer) {	
	buffer.data += incomming.toString();		
	if (incomming.slice(-1)[0] != 10 && incomming.slice(-2)[0] != 6) return [];	
	var parts = buffer.data.split(/\u0006/g), 
		jsonArray = [];	
	for(var i = 0; p = parts[i], i < parts.length; i++) {
		p = p.trim();
		if (p.length == 0) 
			continue;				
		try {
			var json = JSON.parse(p);
			jsonArray.push(json);
		} catch(err) {
			console.log("Message proto: json parse error: ", err, p);
		}
	}
	buffer.data = "";
	return jsonArray;
}


/**
* Take input text, handle it. 
*/
function replInput() {
	var text = editor.getValue();           		
	if (text.trim() == "clear") clear();				
	else {
		appendHistory(text);        
		updateResult(text, false);
		send(text);		
	}
	editor.setValue("");                
}


/** 
* Clear history and send request for version/title string. 
*/
function clear() {
	history.setValue("");
	send("version");
}


/** 
* Handle json messages. 
*/ 
function handleMessage(json)
{
	var multiline = false;		
	switch (json.id) {
		case "parse-multiline":
			multiline = true;
			break;
		case "repl-result":			
			updateResult(json.summary, true);			
			send("history");
			break;
		case "repl-message":			
			var jsonMsg = null;
			try {
				jsonMsg = JSON.parse(json.message);				
			} catch(err) {
				console.log("Error parsing repl-message", err);
			}			
			if (jsonMsg !== null)
				handleReplMessage(jsonMsg);
			break;
		case "meta":
			if (json.cmd == "version") {
				updateResult(json.summary, false);
				$("#repl-status").html("");
				editor.setOption("readOnly", false);
				editor.focus();
			} else if (json.cmd == "history") {				
				autocompleteCode = json.summary;
			} else {
				updateResult(json.summary, true);	
			}
			break;			
		default: 
			if (json.hasOwnProperty("summary")) 
				updateResult(json.summary, true);
			else 
				console.log("Unhandled message: ", json);
		break;
	}	
	if (multiline)
		$("#repl-status").html("Multi-line input");
	else
		$("#repl-status").html("");
}


/** 
* Got a result from the repl. 
*/
function updateResult(data, lwidget) {	
	if (data.trim().length == 0)
		return;
    if (lwidget) {       
        var id = "lineWidget" + (lineWidgetCount++).toString();
        $("body").append("<div class='resultWidget' id='" + id + "'><pre>" + data, + "</pre></div>");
        history.addLineWidget(history.lineCount() - 1, $("#" + id)[0]);
    } else {
        cmAppend(history, data);
    }
    scrollToBottom($("#code-pane > div:first-child"));
}


/** 
* Append text to codemirror text. 
*/
function cmAppend(cm, text) {
    text = text.replace(/(\r\r\n|\r\n|\r)/g, "\n");
    text = text.split("\n").filter(function (el) { return el.length; }).join("\n")    
    if (cm.getValue() !== "") text = "\n" + text;
    cm.replaceRange(text, CodeMirror.Pos(cm.lastLine()));            
}


/** 
* Append text to history. 
*/
function appendHistory(text) {    
    if (text.length == 0 || (historyBuffer.length > 1 && historyBuffer[historyBuffer.length-1] == text)) return;
    historyBuffer.push(text);
    if (historyBuffer.length > maxHistory)
        for(var i = 0; i < 10; i++)
            historyBuffer.shift();                    
    lineIndex = historyBuffer.length;
}


/** 
* History lookup. 
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
* Send input to the repl. 
*/
function send(text) {   	    	
    engine.stdin.write(new Buffer(text + "\n"));
}


function searchInput() {
    var prefix = $("#search-box").val();	
    if (prefix.length == 0) {
		$("#ajax-loader").css("visibility", "hidden");	
        clearSuggestions();
        return;
    }                                           
    browserAction = function(json) {
		$("#ajax-loader").css("visibility", "hidden");
        if (json.length == 0) 
			clearSuggestions();
        else 
			$("#suggestionsPane").html(listToHTML(json.result));                    
    };
	$("#ajax-loader").css("visibility", "visible");
    browser.stdin.write(new Buffer('search-names:' + prefix.toLowerCase() + "\u0006"));   
}

function listToHTML(list) {
    var html = "";
    list.forEach( function(e) {
        html+="<div class='doc-panel' data-expanded='false' onclick='panelClick(\""+e.uuid+"\")'>"+e.name+"</div>";
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
        $(parent).append(symbolToHTML(json.result));        
        parent.dataset.expanded = "true";
    };     
    browser.stdin.write(new Buffer('get-uuid:' + uuid + "\u0006"));        
}

function collapsePanel(name, parent) {
    parent.removeChild(parent.firstChild.nextSibling); 
    parent.dataset.expanded = "false";
}

function symbolToHTML(symbol) {  
  return "<div class='doc-panel doc-panel-expanded'>" +
    symbol.parent + "<br>" + symbol.pretty + "<br><pre>" + 
    symbol.comment.replace(/\\n/g, "\n") + "</pre></div>";    
}

function clearSuggestions() {
    $("#suggestionsPane").html("");    
}


function scrollToBottom(jqEl) {
	jqEl.scrollTop(jqEl[0].scrollHeight);
}

/* Try to auto-detect the phobos lib path. */
function phobosPath() {
	var os = require('os');
	if (os.type() == "Windows_NT") {
		var path = process.env.path.split(";").filter( function(e) { return e.indexOf("dmd") != -1; })[0];
		path = path.slice(0, path.indexOf("dmd2")) + "dmd2\\src\\phobos\\std\\";
		if (require('fs').existsSync(path))
			return path;		
	} else if (os.type() == "Linux") {
		var path = "/usr/include/dlang/dmd/std";		
		if (require('fs').existsSync(path))
			return path;		
	}
}


/** 
* Set up sliding panes. 
*/
function initPanes() {
	$(".slider-pane").each(function(i,e) {						
		e.setAttribute("data-shown", "false");	
	});
}


/** 
* Toggle visibility of given pane. 
*/
function togglePane(id) {
	($("#"+id).attr("data-shown") == "false") ? showPane(id) : hidePane(id);
}


/**  
* Slide-in the given pane. 
*/
function showPane(id) {			
	hidePane();
	var cp = document.getElementById('code-pane'),
		sp = document.getElementById(id);
	cp.style.width = "39%";    
	sp.style.width = "59%";
	sp.style.visibility = "visible";
	sp.setAttribute("data-shown", "true");	
	if (sp.getAttribute("data-onshow")) 		
		eval(sp.getAttribute("data-onshow"));						
}


/** 
* Hide active pane. 
*/
function hidePane() {
	var active = $(".slider-pane[data-shown='true']");
	if (active.length == 0) return;		
	var cp = document.getElementById('code-pane'),
		sp = active[0];
	sp.style.width = "0%";	
	sp.style.visibility = "hidden";
	cp.style.width = "99%";     
    editor.focus();   
	active[0].setAttribute("data-shown", "false");
	if (active.attr("data-onhide")) 		
		eval(active.attr("data-onhide"));		
}


/**
* Create the settings edit fields.
*/
function initSettingsEditor() {		
	$("#settings-list").append("<tr><td>Phobos path:</td><td><input data-key='phobosPath' value="+globalSettings.phobosPath+" type='text'></td</tr>");
	$("#settings-list").append("<tr><td>Autocomplete min length:</td><td><input data-key='autocompleteMinLength' value="+globalSettings.autocompleteMinLength+" type='text'></td</tr>");
}


/**
* Called when settings pane gets hidden, used to store modified settings.
*/
function settingsPaneHide() {	
	var prevPhobosPath = globalSettings.phobosPath;

	$("#settings-list tr td input").each(function(i,e) {		
		globalSettings[e.getAttribute("data-key")] = e.value;
	});
	
	// Update phobos path if it has changed
	if (globalSettings.phobosPath !== undefined && 
		globalSettings.phobosPath != "" &&
		globalSettings.phobosPath != prevPhobosPath) {
		console.log("Sending new path");
		browser.stdin.write(new Buffer("phobos-path:" + globalSettings.phobosPath + "\u0006"));
	}
}


/********** Autocomplete stuff ***********/

	function dcdResponse(type, json) {      
                
        var sort_map = {
            'variable': 0,
            'function': 1,            
            'struct': 4,
            'class': 5,
            'keyword': 6
        }
        
        function dcd_sort(a,b) {
            if (a.kind == b.kind) 
				return a.completion.localeCompare(b.completion);
            else 
				return sort_map[a.kind] - sort_map[b.kind]            
        }
           				   
        if (json.length) {                                      
            json.sort(dcd_sort);            
            var completions = [];
                    
            for(var i = 0; i < json.length; i++) {            
                if (type == "completion") {
					if (json[i].completion == autocomplete.token.string)
						continue;
                    completions.push(
                    {
                        text:json[i].completion,            
                        render: (function(index) {
                            return function(element, self, data) {                                                                     
                                var hint = document.createElement("div"),                                                                    
									html = "", 
									image = "";                                                
                                switch (json[index].kind) {
                                    case 'class':    image = 'c.png'; break;                                    
                                    case 'struct':   image = 's.png'; break;
                                    case 'variable': image = 'v.png'; break;
                                    case 'function': image = 'f.png'; break;
                                    case 'keyword':  image = 'k.png'; break;
                                    default: break;
                                }
                                               
                                if (image != "")
                                    html += "<img class='hint-icon' height='10px' src='../images/" + image + "'>";                                
                                                
                                html += "<div class='hint-text hint-type-" + json[index].kind + "'>" + json[index].completion + "</div>";
                                hint.innerHTML = html;
                                element.appendChild(hint);
                            };
                        })(i)                                    
                    });      
                    
                } else if (type == "calltips") {
                    completions.push({
                        text:'', 
                        displayText:json[i] 
                    });                
                }
            }
             							
            if (autocomplete.token.string == ".") {
                var from = autocomplete.token.start + 1, to = autocomplete.token.start + 1;                            
            } else {
                var from = autocomplete.token.start, to = autocomplete.token.end;                            
            }
            
            var completionsObject = {
                list: completions,
                from: CodeMirror.Pos(autocomplete.cursor.line, from), 
                to: CodeMirror.Pos(autocomplete.cursor.line, to)
            };
                                                  
            autocomplete.callback(completionsObject);
        }                
    };

    function dcdHint(cm, callback, options) {           
		
        var cursor = editor.getCursor(), 
			tk = editor.getTokenAt(cursor);     
        		
		var tempCode = autocompleteCode.slice(0, -1) + "\n" + editor.getValue();		       
        offset = tempCode.length - 1;
            
		autocomplete = {
            callback: callback,
            token: tk, 
            cursor: cursor
        };
		
		console.log("Hinting: token - ", tk);
		
		/*
		console.log("Hinting: offset - ", offset);
		console.log("Hinting: code at offset - ", tempCode[offset]);
		console.log("Hinting: token - ", tk);
		console.log("Hinting: code - ", tempCode);
		*/	
		
		browserAction = function (json) {				
            if (json.length == 0) return;			
			dcdResponse(json.type, json.result);
        };				
		browser.stdin.write(new Buffer("autocomplete: -c" + offset.toString() + " " + tempCode + "\u0006"));		        
    };
	
	

	
function handleReplMessage(json) {
	openResultWindow( function() { resultWindow.window.handle(json); });
}
	
function openResultWindow(onload) {
    if (resultWindow == null) {
        resultWindow = require('nw.gui').Window.open('../html/child.html', {
            position: 'center',         
            title: "Results",
            toolbar: true,
            frame: true,                
            width: 500,
            height: 500            
        });
        resultWindow.on('closed', function() { resultWindow = null; });            
        resultWindow.on('loaded', function() { onload(); });		
    }  else {
        onload();
    }                               
}

