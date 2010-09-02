// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// var_ini.js
// Author : Remy CLEMENT
// Last modification : August 2008
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

// default settings
labels = false;
plabel = "X";
sepChar ="@";

// current image 
imageId = 0;

// getting parameters from the url
cellList = new Array();	
info = location.search.substring(1).split('&');
parameters = new Array();
paraValues = new Array();
for (var i=0; i<info.length; i++) {
    var x = info[i].split('=');
    parameters[i]=x[0];
    paraValues[i]=x[1];
}
if(parameters.length>0){ 
    for(var i=0;i<parameters.length;i++) {
	if(parameters[i]=="plabel") {	
	    plabel = paraValues[i];
	}
	if(parameters[i]=="display") {
	    images = paraValues[i];
	    cellList = images.split('+');  
	}
    }
} 

// check if display argument was present
if (cellList.length==0) {
    document.write("var_init.js: cellpicker.html is not meant to open by users<br/>");
    document.write("var_init.js: cellpicker.html must be called with a display argument, such as: ");
    exurl = document.URL+"?display=001-01-A03-1";
    document.write("\"<a href=\""+exurl+"\">"+exurl+"</a>\"<br/>");
}

// Extracting images names
imageList = new Array(cellList.length);
for (var i=0; i<cellList.length; i++) {
    imageList[i]= cellList[i].split(sepChar)[0];
}

// finalSelectedCells remembers all the selected cells for each image
// finalSelectedCellsLabels remembers the corresponding labels
finalSelectedCells = new Array(imageList.length);
finalSelectedCellsLabels = new Array(imageList.length);
for (var i=0; i<finalSelectedCells.length; i++) {
    finalSelectedCells[i] = new Array();
    finalSelectedCellsLabels[i] = new Array();
}		
