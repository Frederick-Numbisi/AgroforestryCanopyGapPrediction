Estimation of Canopy Gap Fraction –a percentage of sky pixels in each DHP. 
This script was adapted from the Hemispherical_2.0 script of Beckschäfer (2015), 
and the author was credited accordingly in the published paper that is accessibly via the link https://doi.org/10.3390/rs12244163
/**
 The macro batch processes large quantities of both digital
 hemispherical and non-hemispherical canopy photographs.
 The following steps are conducted:
  1.) A photograph from the input folder is opened.
  2.) A mask file to remove the black frame sourrounding the
  circular image area is interactively created.
  3.) Vegetation and sky pixels are separeted by applying the
  "Minimum" thresholding algorithm to the blue color plane.
  4.) Canopy metrics (number of gaps, total gap area [pixel]
  and gap fraction [%]) are calculated from thresholded
  photographs and stored in a table of results.
  5.) Thresholded photographs are exported and saved as 
  TIFF files.
*/
inputFolder = getDirectory("input");
outputFolder = getDirectory("output");
images = getFileList(inputFolder);
inputPath = inputFolder + images[0];
setBatchMode(false);
open(inputPath);
myImageID = getImageID();
ww=getWidth()/2;
hh=getHeight()/2;
run("Specify...", "width="+ww+" height="+ww+" x="+ww+"
y="+hh+" oval centered");
beep();
waitForUser;
run("Clear Outside");
setBatchMode(true);
run("Create Mask");
run("Analyze Particles...", "size=0-Infinity
circularity=0.00-1.00 show=Nothing clear");
for (a=0; a<nResults(); a++) {
    circle=circle+getResult("Area",a);
    }
run("Divide...", "value=255");
MASK = getImageID();
  title = "[Progress]";
  run("Text Window...", "name="+ title +" width=30 
  height=2 monospaced");
  function getBar(p1, p2) {
        n = 20;
        bar1 = "--------------------";
        bar2 = "********************";
        index = round(n*(p1/p2));
        if (index<1) index = 1;
        if (index>n-1) index = n-1;
        return substring(bar2, 0, index) + 
        substring(bar1, index+1, n);
        }
photo = newArray(images.length);
no_of_gaps = newArray(images.length);
gap_area = newArray(images.length); 
gap_fraction = newArray(images.length);
for (i=0; i<images.length; i++) {
inputPath2 = inputFolder + images[i];
open(inputPath2);
PHOTO = getImageID();
run("Split Channels");
PHOTO = getImageID();
open(""Directory of circular roi mask Ex.MaskROI2_Plot3.roi"");
run("Clear Outside");
imageCalculator("Multiply create", PHOTO, MASK);
setAutoThreshold("Minimum");
run("Convert to Mask");
ind = indexOf(images[i], "\.");
im = substring(images[i],0,ind);
outputPath = outputFolder + im + "_T";
saveAs("Tiff", outputPath);
ng = 0;
ga = 0;
run("Invert");
run("Set Measurements...", "area perimeter redirect=None
decimal=2");
run("Analyze Particles...", "size=0-Infinity
circularity=0.00-1.00 show=Nothing clear");
for (a=0; a<nResults(); a++) {
    ga=ga+getResult("Area",a);
    }
photo[i] = images[i];
no_of_gaps[i] = nResults();
gap_area[i] = ga;
gap_fraction[i] = ga/circle*100;
Array.show("Hemipix Results (indexes)", photo,
no_of_gaps, gap_area, gap_fraction);
     prog = i+1;
     print(title, "\\Update:"+prog+"/"+images.length+"
     ("+(prog*100)/images.length+"%)\n"+getBar(prog,
     images.length));
close();
}
close();
close();
