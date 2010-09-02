// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// next_image.js
// Author : Remy CLEMENT
// Last modification : August 2008
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Call 'initializeImage' if there are other images to be processed.
// otherwise, call 'endOfSession'.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function nextImage() {	
	for (var i=1;i<cellNumber+1;i++) {
		if(currentSelectedCells[i]==true){
			finalSelectedCells[imageId].push(i);
			finalSelectedCellsLabels[imageId].push(cellLabels[i]);
		}
	}		
	depaintAll();
	imageId++;	
	if(imageId<imageList.length) {
		initializeImage(imageId);
		paintLabels();
	}
	else endOfSession();
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// This function must be called before working on a new image.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function initializeImage(imageId) {
	// Update currentImage, cellNumber, and contours	
	currentImage= imageList[imageId];		
	updateCoords(currentImage);
	// Initialization of myJsGraphics which contains the graphics for the drawing of polygons
	// Must be initialized now that we know what 'myPicture' is
	myJsGraphicsArray=new Array(cellNumber);
        myJsGraphicsLabels=new Array(cellNumber);
	for(var i=1;i<cellNumber+1;i++) {
		myJsGraphicsArray[i]=new jsGraphics("myPicture");
		myJsGraphicsArray[i].setColor("#ffffff"); // white
                myJsGraphicsLabels[i]=new jsGraphics("myPicture");
                myJsGraphicsLabels[i].setColor("#ffffff"); // white
	}
	// Nothing should be selected
	currentSelectedCells=new Array(cellNumber);
	for (var i=1;i<cellNumber+1;i++) {currentSelectedCells[i] = false;}
	// update cellState
	updateCellLabels(currentImage);
	// for mode ==1 update suggestions
	suggestedCells = new Array(cellNumber);
	updateSugg();   
	// new title (== name of the current well)
	updateTitle("<h1>"+currentImage+"<h1>");
	// new Extra
	updateExtra("");
	// JPEG	& map
	updateImage();
	// Form
	writeForm();
	// Display 	
	paintAll();
	display();
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Update the title (== name of the current well).
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function updateTitle(title) {	
	document.getElementById("intro").innerHTML = title;	
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Update the extra information div.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function updateExtra(extra) {
	document.getElementById("extra").innerHTML = extra;
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Update the image and its map.
// first the map of the image is modified according to the new coordinates 
// of the polygons then the raw image is updated. 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function updateImage() { 	
	// updating map
	myMap="";
	for(var i=1;i<cellNumber+1;i++){
		areaShape = '<area shape=\"polygon\" coords='+contours[i]+' onClick=\"selectObject('+i+')\" ';
		if(labels==true) {areaShape=areaShape+"onmouseover=\"Tip('"+cellLabels[i]+"',OFFSETX,10)\" onmouseout=\"UnTip()\" ";}
		areaShape=areaShape+"/>\n";
		myMap=myMap+areaShape;
	}
	document.getElementById("map").innerHTML = myMap;
	// updating image
	well = currentImage.split('-')[0]
	plate = well.substr(0, 2);	
	imageurl = "view/"+currentImage.substring(0,6)+"/"+currentImage+"_um.jpeg";

        // check imageurl
        var xmlhttp = getXMLHTTP();
        xmlhttp.open("HEAD", imageurl, false);
        xmlhttp.send(null);
        if (xmlhttp.status==404) {	
           document.write("Cannot open image url="+imageurl+"</br>");
        }
        document.getElementById("ima").src = imageurl;
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// write the form for the current cell in the document.
// the form is written as a string, and then this string
// is placed inside "document.getElementById("myForm").innerHTML"
// to paintAll it.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function writeForm() {  
	myFormString = "<form name=\"formu\">"+"<table class=\"style2\">";
	// Reset and nextImage buttons
	myFormString=myFormString+"<tr>";
	//myFormString=myFormString+"<td><input type=\"button\" value=\"Select suggested cells only\" onclick=\"selectFilterCells()\"></td>";
	myFormString=myFormString+"<td><input type=\"button\" value=\"Deselect all\" onclick=\"deselect("+labels+")\"></td>";
	myFormString=myFormString+"<td><input type=\"button\" value=\"Confirm selection and go to the next image\" onclick=\"nextImage()\"></td>";
        myFormString=myFormString+"<td><input type=\"button\" value=\"Toggle cell labelling\" onclick=\"toggleLabels()\"></td>";
	//myFormString=myFormString+"<td width='50px'>&nbsp;</td>";
	myFormString=myFormString+"<td><input type=\"button\" value=\"End session\" onclick=\"endOfSession()\"></td>";
	myFormString=myFormString+"</tr>";
	// end of the form
	myFormString=myFormString+"</table>";
	myFormString=myFormString+"</form>";
	// putting the form in "document.getElementById("myForm").innerHTML"
	if (document.getElementById) {
    		document.getElementById("myForm").innerHTML = myFormString;
    	}
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Return a XMLHttpRequest object for the main browsers,
// i.e. at least Firefox, Safari, and I.E.
// And if the browser do not enable javascript, a pop up will appear.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function getXMLHTTP() {
	var xhr=null;
	if (window.ActiveXObject) {
		// Internet Explorer <7
		try {xhr = new ActiveXObject("Msxml2.XMLHTTP");} 
		catch (e) {
        		try {xhr = new ActiveXObject("Microsoft.XMLHTTP");} 
			catch (e1) { }                                  
        	} 
	} else if (window.XMLHttpRequest) {
	 	// Firefox & Safari& IE7
		xhr = new XMLHttpRequest();                                                          
     	}
	else {alert("Your browser does not enable javascript");}                                                                      
	return xhr;                                                            
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Get new data from a 'wellImage.js' file.
// use of a xmlHttpRequest object in *synchronous* mode :
// the data must be reinitialized before continuing the process.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function updateCoords(currentImage) {	
    url = "cellpicker/"+currentImage.substring(0,6)+"/"+currentImage+"_con.js";
    try {
	var xmlhttp = getXMLHTTP();
	xmlhttp.open("GET", url, false);
	xmlhttp.send(null);
	if (xmlhttp.status==404) {
	    document.write("next_image.js/updateCoords: cannot open contour url="+url+"</br>");
	} else window.eval(xmlhttp.responseText);
    } catch (exception) {
	document.write("next_image.js/updateCoords: cannot open contour url="+url+"</br>");
    }
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// update the vector 'cellLabels' for this image.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function updateCellLabels(currentImage) {	   
    url = "cellpicker/"+currentImage.substring(0,6)+"/"+currentImage+"_clabels.js";
    try {
	var xmlhttp = getXMLHTTP();
	xmlhttp.open("GET", url, false);
	xmlhttp.send(null);
	if (xmlhttp.status==404) cellLabels = new Array();
	else window.eval(xmlhttp.responseText);
    } catch (exception) {
	cellLabels = new Array();
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// update the vector 'suggestedCells' for this image.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function updateSugg() {	
	for (var i=1;i<cellNumber+1;i++) {suggestedCells[i]=false;}
	for (var i=1;i<cellList[imageId].length;i++) {
		suggestedCells[cellList[imageId].split(sepChar)[i]]=true;
	}
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// After all the images have been analyzed.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function endOfSession() {
    document.getElementById("myForm").innerHTML = "";
    document.getElementById("myPicture").innerHTML="";			
    document.getElementById("displayed").innerHTML = "";
    
    newIntro = "<h1>End of session</h1>";			
    updateTitle(newIntro);		
    newExtra = "<h2>List of the selected cells :</h2>";	
    
    myTableAsString ="<br/><table class=\"style\">"+"<tr>"+
	"<th width=\"100px\">uname</th>"+
	"<th>spot</th>"+
	"<th>id</th>"+
	"<th>label</th>";
    myTableAsString=myTableAsString+"</tr>";
    for (var i=0;i<finalSelectedCells.length;i++) {
	for(var j=0;j<finalSelectedCells[i].length;j++){
	    k = imageList[i].length-2;
	    myTableAsString=myTableAsString+"<tr>";
	    myTableAsString=myTableAsString+"<td>"+imageList[i].substring(0, k)+"</td>";
	    myTableAsString=myTableAsString+"<td>"+imageList[i].substring(k+1)+"</td>";
	    myTableAsString=myTableAsString+"<td>"+finalSelectedCells[i][j]+"</td>";
	    myTableAsString=myTableAsString+"<td>"+plabel+"</td>";
	    myTableAsString=myTableAsString+"</tr>";
	}		
    }
    myTableAsString=myTableAsString+"</table>";			
    updateExtra(newExtra+myTableAsString);	
}
