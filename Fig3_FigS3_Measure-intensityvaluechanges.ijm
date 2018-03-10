//Description
//Author: Luis Bezares Calderon, written in May 2017 
//Purpose:Recording intensity profiles of selected ROIs across a list of files. This macro is the same as that called Measure-intensityvaluechanges3.ijm
//Input Data: Registered time-lapse recordings with a green (GCaMP) and red (tdTomato) channel. In this case 
//Important: the macro assumes the videos are already corrected for X-Y shifts. This is done using the Descriptor-based Series registration plugin. Before registering the videos, crop only the frames to be analyzed and that do not include big XY shifts. Use the DIC channel for registration and then apply the calculations to the GCaMP and TdTomato channels.
//Publication:Bezares-Calderon et al, 2018 

macro "MeasureIntensityprofiles"

//Obtaining directory addresses.
{
	if((getBoolean("Choose input and output directories if not select a list of input and output directories")))
	{
         inputTomDir = getDirectory("Choose the Tomato file directory (where the files are)");
         inputGCDir= getDirectory("Choose the GC file  directory (where the files are)");
	    	G_Ddir = getDirectory("Choose destination directory");
	}

 
	// Get all the files in the input directory.
	list=getFileList(inputTomDir);
	Array.sort(list);
	print(list.length);

//This 'for' will apply  the LUT described in Guehmann et al,2015
	 for (k=0; k<list.length; k++)
	{
			print(list[k]); 
			fullpathTom_image=inputTomDir + list[k];
			open(fullpathTom_image);
			img_title = getTitle();
			img_title = replace(img_title, " ", "_"); // Replace spaces by underscores to avoid problems with file writing
	        img_titleGC= replace(img_title, "Tom", "GC");
	        print(img_titleGC);
	         rename("video");
	         fullpathGC_image= inputGCDir+img_titleGC;
	         open(fullpathGC_image);
			 r=newArray(256);g=newArray(256);b=newArray(256); 
			for (i=0;i<256;i++) { 
				i4=4*i/256; 
				r[i]=255*minOf(maxOf(minOf(i4-1.5,-i4+4.5),0),1); 
				g[i]=255*minOf(maxOf(minOf(i4-0.5,-i4+3.5),0),1); 
				b[i]=255*minOf(maxOf(minOf(i4+0.5,-i4+2.5),0),1); 
			} 
			setLut(r,g,b); 
			 saveAs("Tiff",fullpathGC_image +"JETlut");
             rename("videoGC");
             for(i=0;i<nSlices;i++) {
               run("Next Slice [>]");
               wait(100);
             }
			
//This part of the code has the purpose of creating the ROIs of the cells to be measured. Use the reference tdTomato signal for drawing the ROIs.	 
	 do{
	   selectWindow("video");
		setTool("polygon");
		waitForUser("Set ellipse in region for measuring intensity");
		Cell="untitled";
		Dialog.create("CellOfInterest");
		Dialog.addString("Title:",Cell );
		Dialog.show();
		Cell=Dialog.getString();
		
		ROIname=img_title+"_"+Cell;
	    print(ROIname);
	      Roi.setName(ROIname); 
	      roiManager("Add");
		 selectWindow("video");
		 run("Clear Results");
		 run("Plot Z-axis Profile");  //This will create the intensity values in the ROI across the whole video.
		img_title=replace(img_title,".tif","");
		print(img_title);
		saveAs("Results",G_Ddir+img_title+"_"+Cell+".txt");
		run("Clear Results");
		 selectWindow("videoGC");
	index=findRoiWithName(ROIname);
	 roiManager("select",index);
	 run("Clear Results");
	 run("Plot Z-axis Profile");
	 img_titleGC=replace(img_titleGC,".tif","");
	 print(img_titleGC);
	 saveAs("Results",G_Ddir+img_titleGC+"_"+Cell+".txt");
	 run("Clear Results");
	selectWindow("videoGC");
	 for(i=0;i<nSlices;i++) {
               run("Next Slice [>]");
               wait(100);
             }
	 Dialog.create("More ROIs");
		Dialog.addCheckbox("Check box if more rois in the present video", false);
		Dialog.show();
     	Satis = Dialog.getCheckbox();
     	print(Satis);
	 }while(Satis);
	 run("Close All");
	}
}

/* 
 * Returns index of first ROI that matches  
 * the given regular expression  written by oburri http://forum.imagej.net/t/selecting-roi-based-on-name/3809/2
 */ 
function findRoiWithName(roiName) { 
	nR = roiManager("Count"); 
 
	for (i=0; i<nR; i++) { 
		roiManager("Select", i); 
		rName = Roi.getName(); 
		if (matches(rName, roiName)) { 
			return i; 
		} 
	} 
	return -1; 
} 


