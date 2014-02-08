
var editor, history, engine, browser;
var browserAction = null, browserStatus = '';
var historyBuffer = [];
var maxHistory = 200; 
var lineIndex = 0; // for moving through the history
var lineWidgetCount = 0;
var activeSliderPane = "";

/**
* Init global settings
*/
var globalSettings = {
	phobosPath: phobosPath()
};


/**
* Restore settings from localStorage if present
*/
if (localStorage.hasOwnProperty("globalSettings"))		
	globalSettings = JSON.parse(localStorage.globalSettings);		


/**
* On close, store settings
*/
require('nw.gui').Window.get().on('close', function() {	
	localStorage.globalSettings = JSON.stringify(globalSettings);
	this.close(true);
});


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


function windowResize() {
	$('body > table > tbody > tr > td > div').height(
		window.innerHeight - 
		$("body > table > thead > tr > th").height() - 
		$("body > table > tfoot > tr > td").height());
	scrollToBottom($("#code-pane > div:first-child"));
}

/* Devtools shortcut */
shortcut.add("Ctrl+Shift+J",function() { 
    require('nw.gui').Window.get().showDevTools();
});    

/* Fullscreen toggle shortcut */
shortcut.add("Ctrl+M",function() { 
    require('nw.gui').Window.get().toggleFullscreen();
});    

/* Doc search toggle shortcut */
shortcut.add("Ctrl+H",function() { 
    togglePane('docsearch-pane');
});    

/* Options toggle shortcut */
shortcut.add("Ctrl+O",function() { 
    togglePane('settings-pane');
});    




/**
* On-load setup
*/
$(document).ready(function () {
	
	//windowResize();
    //$(window).resize(windowResize);              
	
	$("#repl-status").html("Initializing...");
	
	initPanes();	
	initSettingsEditor();
	
    require('fs').watch('../../ui/css/style.css', function (event, name) {
        var queryString = '?reload=' + new Date().getTime();
        $('link[rel="stylesheet"]').each(function () {
            this.href = this.href.replace(/\?.*|$/, queryString);
        });
		setTimeout(windowResize, 500);
    });

    editor = CodeMirror.fromTextArea(document.getElementById("code"), {
        mode: "text/x-d",
        viewportMargin: Infinity,
        lineWrapping: true,
        smartIndent: false,
        extraKeys: { "Ctrl-Down": "autocomplete" }
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
        // If currently auto-completing, do nothing
        if (e.ctrlKey == true) return;
        if (editor.state.completionActive) return;
        if (!e || !(e instanceof KeyboardEvent)) return;

        if (e.type == 'keydown' && e.shiftKey == false && e.keyCode == 13) {            
            replInput();	
			e.preventDefault();
            return true;
        } else if (e.type == 'keydown' && editor.lineCount() == 1 && (e.keyCode == 38 || e.keyCode == 40)) {
            var line = "";
            if (e.keyCode == 38) line = retrieveHistory('up');
            else line = retrieveHistory('down');
            if (typeof line != "undefined") {
                editor.setValue(line);				
				setTimeout( function() { editor.setCursor(editor.lineCount(), 0); }, 50);								
			}
        }
    };


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
            browser.stdin.write(new Buffer('suggest-names:' + tk.string.toLowerCase() + '\n'));
        };
    } ());


    /**
    * Start repl
    */
	var repl_buffer = {data:""};
	engine = require('child_process').spawn('../repl', ['--noConsole']);		
	engine.stdout.on('data', function (data) { 		
		var messages = messageProtocol(data, repl_buffer);		
		if (messages.length == 0) return;					
		for(var i = 0; m = messages[i], i < messages.length; i++) 
			handleMessage(m);							
	});	
    send("version");
    

    /**
    * Start browser
    */
	var browser_buffer = {data:""};
	browser = require('child_process').spawn('../browser', [globalSettings.phobosPath]);	        	
	browser.stdout.on('data', function (data) {	
		var messages = messageProtocol(data, browser_buffer);		
		if (messages.length == 0) return;			
		for(var i = 0; m = messages[i], i < messages.length; i++) {
			if (m.hasOwnProperty("status"))
				browserStatus = m.status;
			else if (browserAction !== null)		
				browserAction(m.result);					
		}
    });
});


function messageProtocol(incomming, buffer) {	
	buffer.data += incomming.toString();		
	if (incomming.slice(-1)[0] != 10 && incomming.slice(-2)[0] != 6) return [];
	var str = buffer.data.replace(/(\r\r\n|\r\n|\n|\r)/g, "<br>");       
	str = str.replace(/\u0009/g, "   ");
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
* Take input text, handle it
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
* Handle json messages
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
* Got a result from the repl
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
* Append text to history
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
* Send input to the repl
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
    browser.stdin.write(new Buffer('search-names:' + prefix.toLowerCase() + "\n"));   
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


// Set up sliding panes
function initPanes() {
	$(".slider-pane").each(function(i,e) {						
		e.setAttribute("data-shown", "false");	
	});
}

// Toggle visibility of given pane
function togglePane(id) {
	($("#"+id).attr("data-shown") == "false") ? showPane(id) : hidePane(id);
}

// Slide-in the given pane
function showPane(id) {			
	hidePane();
	var cp = document.getElementById('code-pane'),
		sp = document.getElementById(id);
	cp.style.width = "40%";    
	sp.style.width = "58%";
	sp.style.visibility = "visible";
	sp.setAttribute("data-shown", "true");	
	if (sp.getAttribute("data-onshow")) eval(sp.getAttribute("data-onshow"));					
}

// Hide active pane
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
	if (active.attr("data-onhide")) eval(active.attr("data-onhide"));	
}

function initSettingsEditor() {		
	$("#settings-list").append("<tr><td>Phobos path:</td><td><input data-key='phobosPath' value="+globalSettings.phobosPath+" type='text'></td</tr>");
}

function settingsPaneHide() {
	console.log($("#settings-pane").data("tempSettings"));
	$("#settings-list tr td input").each(function(i,e) {
		console.log("set");
		globalSettings[e.getAttribute("data-key")] = e.value;
	});
}

