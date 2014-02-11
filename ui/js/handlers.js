
handlers = {};

function handle(json) {
    if (json.hasOwnProperty('data') && 
        json.hasOwnProperty('handler') && 
        handlers.hasOwnProperty(json.handler)) {
        handlers[json.handler](json.data);
    }
}

handlers.html = function(data) {
	result(data);    
};

handlers.plot = function(data) {	
	
    result('<div><svg id="chart" style="height:600px" /></div>');                                               
    
	nv.addGraph(function() {
		var chart = nv.models.lineChart()
					.margin({left: 100})  //Adjust chart margins to give the x-axis some breathing room.
					.useInteractiveGuideline(true)  //We want nice looking tooltips and a guideline!
					.transitionDuration(350)  //how fast do you want the lines to transition?
					.showLegend(true)       //Show the legend, allowing users to turn on/off line series.
					.showYAxis(true)        //Show the y-axis
					.showXAxis(true)        //Show the x-axis
				;
 
	  chart.xAxis 
		  .axisLabel('X-Axis')
		  .tickFormat(d3.format(',r'));
	 
	  chart.yAxis
		  .axisLabel('y-Axis')
		  .tickFormat(d3.format('.02f'));
	 	  	
	  d3.select('#chart')
		  .datum(data)     
		  .call(chart);
	 
	  //Update the chart when window resizes.
	  nv.utils.windowResize(function() { chart.update() });
	  return chart;
	});
};    