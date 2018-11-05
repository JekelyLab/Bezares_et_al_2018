//Description
//Author: Luis Bezares Calderon and Gaspar Jekely, written in June--July 2017.
//Purpose: Extract speed and other parameters from freely swimming larvae (recorded from the top) startled with a motor attached to the container dish.
//Input data: Time-lapse recordings of freely swimming larvae startled at a defined point in time. The larva density should not be too high to be able to isolate instances of the startle response in individual larvae. The number of pixels per larvae has to be enough to detect differences in area due to parapodia elevation. 
//Publication:Bezares-Calderon et al,2018.

macro "MeasureLarvaTracks"
{
//getting files and specifiying output folders. This fragment was obtained from macro used in Guehmann et al, 2015
	if((getBoolean("Choose input and output directories if not select a list of input and output directories")))
	{
         inputDir = getDirectory("Choose the  file directory (where the files are)");  //Collect all the videos to be analysed under one single folder or analyse them in batches.  
         G_DdirMtrack = getDirectory("Choose destination directory Mtrack");  //The tracks of individual larvae will be stored in this folder.
         G_DdirSummary = getDirectory("Choose destination directory Summary");
         G_DdirTiff =getDirectory("Choose destination directory Paths");
	}
    list=getFileList(inputDir);
	Array.sort(list);
	print(list.length);
    for (k=0; k<list.length; k++)
	{
		print(list[k]); 
		fullpath_image=inputDir + list[k];
		open(fullpath_image);
		img_title = getTitle();
		img_title = replace(img_title, " ", "_"); // Replace spaces by underscores to avoid problems with file writing     
		img_titleEd = replace(img_title, ".czi", ""); // Removee extension file     
		print(img_title);
		
		//{Removing non-moving elements from stack
		run("Invert", "stack");
		run("Z Project...","projection=[Average Intensity]");
		selectWindow(img_title);
		selectWindow("AVG_"+img_title);
		imageCalculator("Subtract stack", img_title,"AVG_"+img_title);
		run("Smooth", "stack");
		selectWindow("AVG_"+img_title);
		close();
		
		
		
		do{
			//Finding the ROI where a single larva displays the startle response, as well as prior and post stimulus behavior. 
			selectWindow(img_title);
			run("Duplicate...", "title=1 duplicate");
			setTool("rectangle");
			waitForUser("Set rectangle in region with startled larva");   //The user has to select the ROI to measure. Only choose rectangle ROIs that during the whole behavior only include one larva.
		
			Dialog.create("ROIOfInterest");
			ROI="untitled";
			Dialog.addString("Title:",ROI );
			Dialog.show();
		   
		    ROI = Dialog.getString();
		    Roi.setName(ROI); 
			roiManager("Add");
		    run("Crop");
		    setAutoThreshold("Yen dark stack");   //This step will threshold the image series.
			run("Convert to Mask", "method=Yen background=Dark");
			//Determine where to cut the stack so that a single track is left and that it includes also the startle part.
			waitForUser("Scroll through video to detect beginning and end of behavior. Only one larva visible");
			Dialog.create("Beginning and End of Startle behavior");
			Dialog.addNumber("Beginning:", 512);
		    Dialog.addNumber("End:", 512);
		    Dialog.show();
			begin = Dialog.getNumber();
		    end = Dialog.getNumber();
			run("Slice Keeper", "first="+begin+" last="+end+" increment=1");
			rename("video");
			//tracking
			mintracklength=(end-begin-((end-begin)/2));
			print(mintracklength);
			fullpathresults=G_DdirMtrack+img_titleEd+"_MTrackResults"+ROI+".txt";
			run("Set Measurements...","redirect=None decimal=3");
			run("Set Scale...", "distance=0 known=0 pixel=1 unit=pixel");
			run("MTrack2 ", "minimum=200 maximum=999999 maximum_=20 minimum_="+mintracklength+" display show save save=" +fullpathresults);  //The Mtrack parameters have to be adjusted depending on the source video. 
		    selectWindow("Paths");
		    saveAs("Tiff",G_DdirTiff+img_titleEd+"_Path"+ROI+".tiff");  //The paths of the individual larva will be stored.
		    selectWindow(img_title);
			run("Set Scale...", "global");
 			//particle analysis. This part is required to obtain the area of the larva across the video.
			run("Set Measurements...", "bounding fit shape feret's redirect=None decimal=3");
		    selectWindow("video");
		    run("Analyze Particles...", "size=30000-100000 show=[Bare Outlines] exclude clear include summarize stack");
			selectWindow("Summary of video");
             saveAs("Text", G_DdirSummary+img_titleEd+"_Summary"+ROI+".txt"); 
             selectWindow(img_title);
             close("\\Others");
			Dialog.create("More ROIs");
			Dialog.addCheckbox("Check box if more rois in the present video", false);
			Dialog.show();
		    MoreROIs = Dialog.getCheckbox();
		    print(MoreROIs);
		}while(MoreROIs);
}	



