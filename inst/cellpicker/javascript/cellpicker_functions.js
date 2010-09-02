// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// cellpicker_functions.js
// Author : Remy CLEMENT
// Last modification : August 2008
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Draws the contour of a cell. 
// white for the selected cells, and purple if the cell is only suggested.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function paint(cellId) {
	// Remove the content of the JsGraphics for this object
	myJsGraphicsArray[cellId].clear();
	// if the cell is selected 
	if(currentSelectedCells[cellId]==true) {
		myJsGraphicsArray[cellId].setColor("#ffffff"); // white
		var x = new Array();
		var y = new Array();
		for (var j=0; j<contours[cellId].length; j++) {
			if(j%2==0) {x[j/2]=parseInt(contours[cellId][j]);}
			else{y[(j-1)/2]=parseInt(contours[cellId][j]);}
		}
		myJsGraphicsArray[cellId].drawPolygon(x, y);
	}
	// if the cell is suggested only
	// (priority is given to the selected items, e.g. if a cell is both selected and suggested then the contour will be white)	
	if((suggestedCells[cellId]== true) & currentSelectedCells[cellId]==false){
		myJsGraphicsArray[cellId].setColor("#ff00ff"); // purple			
		var x = new Array();
		var y = new Array();
		for (var j=0; j<contours[cellId].length; j++) {
			if(j%2==0) {x[j/2]=parseInt(contours[cellId][j]);}
			else{y[(j-1)/2]=parseInt(contours[cellId][j]);}				
		}	
		myJsGraphicsArray[cellId].drawPolygon(x, y); 				
	}	
	// Paint		
	myJsGraphicsArray[cellId].paint();
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Apply 'paint' to every cell. 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function paintAll() {
	for (cellId=1;cellId<cellNumber+1;cellId++) {
		paint(cellId);
	}
}  

function toggleLabels() {
	labels=!labels;
	paintLabels();
}

function paintLabels() {
	if (labels) {
		for (cellId=1;cellId<cellNumber+1;cellId++) {
                        myJsGraphicsLabels[cellId].clear();
	        	var x = new Array();
	                var y = new Array();
        	        for (var j=0; j<contours[cellId].length; j++) {
                	        if(j%2==0) {x[j/2]=parseInt(contours[cellId][j]);}
                        	else{y[(j-1)/2]=parseInt(contours[cellId][j]);}
	                }
               
        	        if (labels==true) {
                	   var cx1=x[1];
	                   var cx2=x[1];
        	           var cy1=y[1];
                	   var cy2=y[1];
	                   for (var j=0; j<x.length; j++) {
        	              if (x[j]<cx1) cx1=x[j];
                	      if (x[j]>cx2) cx2=x[j];
	                      if (y[j]<cy1) cy1=y[j];
        	              if (y[j]>cy2) cy2=y[j];
                	   }
	                   cx = (cx1+cx2)/2;
        	           cy = (cy1+cy2)/2;
                	   myJsGraphicsLabels[cellId].drawString(cellLabels[cellId], cx, cy);
	                }
   			myJsGraphicsLabels[cellId].paint();
		}
        } else {
		for (cellId=1;cellId<cellNumber+1;cellId++) {
			myJsGraphicsLabels[cellId].clear();
			myJsGraphicsLabels[cellId].paint();
		}
	}
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Remove every drawing. 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function depaintAll() {
	for (cellId=1;cellId<cellNumber+1;cellId++) {
		myJsGraphicsArray[cellId].clear();
		myJsGraphicsLabels[cellId].clear();
	}
}   

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// this function enables the selection of a object. 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function selectObject(cellId){ 
	myJsGraphicsArray[cellId].clear();
	// update of the list of the object which should be drawn
	if(currentSelectedCells[cellId] == true) {currentSelectedCells[cellId] = false;}
	else {currentSelectedCells[cellId] = true;}
	// update the drawing of the contour	 
	paint(cellId); 
	display();	    
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// select all the cells of the image in the state 'filter'.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function selectFilterCells() {
	for (var cellId=1;cellId<cellNumber+1;cellId++) {	
		if(suggestedCells[cellId]==true) {currentSelectedCells[cellId]=true;} 
		else {currentSelectedCells[cellId]=false;}
	}
	paintAll();
	display();
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Deselect the selected cells and update the graphics.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function deselect() {
	for (var i=1;i<cellNumber+1;i++) {currentSelectedCells[i]=false;}
	paintAll();
	display();
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// This function will update a table which contains the selected cells
// Col 1 : name of the image
// Col 2 : cellId
// Col 3 : label (only if labels==true)
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function display() {
    myTableAsString = "<table class=\"style\">"+"<tr>"+
	"<th width=\"100px\">uname</th>"+
	"<th>spot</th>"+
	"<th>id</th>"+
	"<th>label</th>"+"</tr>";	
    for (var i=1;i<currentSelectedCells.length;i++) {
	if(currentSelectedCells[i]==true){
	    k = currentImage.length-2;
	    myTableAsString=myTableAsString+"<tr>";
	    myTableAsString=myTableAsString+"<td>"+currentImage.substring(0, k)+"</td>";
	    myTableAsString=myTableAsString+"<td>"+currentImage.substring(k+1)+"</td>";
	    myTableAsString=myTableAsString+"<td>"+i+"</td>";		
	    myTableAsString=myTableAsString+"<td>"+plabel+"</td>";
	    myTableAsString=myTableAsString+"</tr>";
	}
    }
    myTableAsString=myTableAsString+"</table>";	
    document.getElementById("displayed").innerHTML = myTableAsString;    	
}
