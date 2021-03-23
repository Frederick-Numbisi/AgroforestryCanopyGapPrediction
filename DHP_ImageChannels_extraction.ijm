
//Split the colour channels of DHP images in a directory using a batch processing 
/*
At the moment, this script saves the results to a new directory called "ImageChannels". 
If you want to save the result files in the same directory 
as your original files delete the line "dir2 = ....." and change 
in the first lane "dir1" into "dir2" 
*/

dir1 = getDirectory("Choose Source Directory ");
dir2 = dir1+"ImageChannels_Min"+File.separator;
File.makeDirectory(dir2);
list = getFileList(dir1);
for (i=0; i<list.length; i++)
        {
       
        if (File.isDirectory(dir1+list[i])){}
        else{
               
                path = dir1+list[i];
                if (endsWith(path, ".db")){}
                else{
                       
                        open(path);
                        if (bitDepth!=24){}  
                        else {
                                setBatchMode(true);
                                title = File.nameWithoutExtension ;
		        open("D:\\PhD Manuscript\\DHP IMAGES\\DHP_BakoaPlot3\\MaskROI2_Plot3.roi"); 
		        run("Clear Outside"); 
                                saveAs("Jpeg", dir2+title+"rgn.jpg");
		        run("Set Scale...", "distance=2082 known=2082 unit=unit");
                                run("Split Channels");
                                close();
                               
                                saveAs("Jpeg", dir2+title+"d0.jpg");
                                run("Auto Threshold...", "method=Minimum setthreshold show");
                                run("Auto Threshold...", "method=Minimum white");                                                        
                                saveAs("Tiff", dir2+title+"bin.tif");
                                run("Close");
                                run("Close");  
		        close();                              
                                setBatchMode(false);
                              
                                }
                        }
                }
        } 


