
var editor, history, engine, browser;
var browserAction = null, browserStatus = '';
var historyBuffer = [];
var maxHistory = 200; 
var lineIndex = 0; // for moving through the history
var lineWidgetCount = 0;
var autocompleteCode = "";

var autocomplete = {
    callback: null,
    token: null, 
    cursor: 0
};

/** 
* Init global settings. 
*/
var globalSettings = {
	phobosPath: phobosPath()
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
CodeMirror.commands.autocomplete = function(cm) {
    CodeMirror.showHint(cm, dcdHint, {
        async: true,
        completeSingle: false
    });
}


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
    
	// Replace this with DCD version
    (function () {
        CodeMirror.dHint = function (editor, callback, options) {
            var cursor = editor.getCursor();
            var tk = editor.getTokenAt(cursor);
            browserAction = function (json) {
                if (json.length == 0) return;
                callback({
					list: json,
                    from: CodeMirror.Pos(cursor.line, tk.start),
                    to: CodeMirror.Pos(cursor.line, tk.end)
                });                
            };
            browser.stdin.write(new Buffer('suggest-names:' + tk.string.toLowerCase() + '\u0006'));
        };
    }());

    initRepl();
	initBrowser();
        
});

/** 
* Monitor stylesheet for changes .
*/
function monitorStylesheet() {
    require('fs').watch('../../dabble/ui/css/style.css', function (event, name) {
        var queryString = '?reload=' + new Date().getTime();
        $('link[rel="stylesheet"]').each(function () {
            this.href = this.href.replace(/\?.*|$/, queryString);
        });
		setTimeout(windowResize, 500);
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
        smartIndent: false,
        extraKeys: { "Ctrl-Space": "autocomplete" }
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
        // If currently auto-completing, do nothing		
        if (e.ctrlKey == true || editor.state.completionActive) return;
        
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
        }
    };
}

/** 
* Start repl. 
*/
function initRepl() {
	var repl_buffer = {data:""};
	engine = require('child_process').spawn('../repl', ['--noConsole']);		
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
	browser = require('child_process').spawn('../browser', [globalSettings.phobosPath]);	        	
	browser.stdout.on('data', function (data) {	
		console.log('From browser: ', data.toString());
		var messages = messageProtocol(data, browser_buffer);		
		if (messages.length == 0) return;			
		for(var i = 0; m = messages[i], i < messages.length; i++) {
			if (m.hasOwnProperty("status"))
				browserStatus = m.status;
			else if (browserAction !== null)		
				browserAction(m.result);					
		}
    });
}


/**
* Handle child process messages.
*/
function messageProtocol(incomming, buffer) {	
	buffer.data += incomming.toString();		
	if (incomming.slice(-1)[0] != 10 && incomming.slice(-2)[0] != 6) return [];
	var str = buffer.data.replace(/(\r\r\n|\r\n|\n|\r)/g, "\\n");       	
	str = str.replace(/(")/g, "\"");       	
	var parts = str.split(/\u0006/g);	
	var jsonArray = [];
	for(var i = 0; p = parts[i], i < parts.length; i++) {
		if (p.replace(/<br>/g,'').trim().length == 0) continue;
		try {
			var json = JSON.parse(p);
			jsonArray.push(json);
		} catch(err) {
			console.log("Message proto: json parse error: ", err);
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
			// need to handle inner messages
			updateResult(json.summary, true);
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
    if (lwidget) {       
        var id = "lineWidget" + (lineWidgetCount++).toString();
        $("body").append("<div class='resultWidget' id='" + id + "'>" + data, + "</div>");
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
			$("#suggestionsPane").html(listToHTML(json));                    
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
    if (typeof el.dataset.expanded == "undefined") return;   
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
  return "<div class='doc-panel doc-panel-expanded'>" +
    symbol.parent + "<br>" + symbol.pretty + "<br>" + 
    symbol.comment.replace(/\n/g, '<br>') + "</div>";    
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
}


/**
* Called when settings pane gets hidden, used to store modified settings.
*/
function settingsPaneHide() {
	console.log($("#settings-pane").data("tempSettings"));
	$("#settings-list tr td input").each(function(i,e) {
		console.log("set");
		globalSettings[e.getAttribute("data-key")] = e.value;
	});
}


/********** Autocomplete stuff ***********/

	function dcdResponse(str) {      
        
        var type = "", jsonPart = "";
        if (str.indexOf("completionInfo") == 0) {
            type = "complete";
            jsonPart = (str.split("completionInfo"))[1];
        } else if (str.indexOf("calltipInfo") == 0) {
            type = "tip";
            jsonPart = (str.split("calltipInfo"))[1];
        }
        
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
           
        if (jsonPart != "") {                
            var json = JSON.parse(jsonPart);           
            json.sort(dcd_sort);            
            var completions = [];
                    
            for(var i = 0; i < json.length; i++) {            
                if (type == "complete") {
                    completions.push(
                    {
                        text:json[i].completion,            
                        render: (function(index) {
                            return function(element, self, data) {                                                                     
                                var hint = document.createElement("div"),                                                                    
									html = "", 
									image = '../images/blank.png';                                                
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
                    
                } else if (type == "tip") {
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
		cm = editor;	
        var cursor = cm.getCursor(), 
			tk = cm.getTokenAt(cursor);     
        
		console.log("Hinting: token - ", tk);
		
        var offset = 0;
        cm.doc.eachLine(0, cursor.line, function(handle) { offset += handle.text.length + 1; } );    
        offset += cursor.ch;    
            
		autocomplete = {
            callback: callback,
            token: tk, 
            cursor: cursor
        };
        
		//browser.stdin.write(new Buffer("-c" + offset.toString() + " " + (homeDir + "_temp").replace(/\\/g, "/") + "\n")); 
		
        //require("fs").writeFile(homeDir + "_temp", cm.getValue(), function(err) {                             
        //        browser.stdin.write(new Buffer("-c" + offset.toString() + " " + (homeDir + "_temp").replace(/\\/g, "/") + "\n")); 
        //    });                                
    };