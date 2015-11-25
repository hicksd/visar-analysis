Updated July 1, 2010
******************
STARTING XVIS


Double click the file xvis.sav. As long as you have IDL virtual  
machine (freeware) or IDL itself (expensiveware) it will run as a  
stand alone program. It will also run if you type "xvis" at the IDL  
command line. The only other file it needs is the streak camera  
settings file (also attached) to be able to load time sweeps.

If you have the IDL source files rather than the "xvis.sav" executable
then just type "xvis" at the commandline.


*********************
IMAGE FORMATS


These are the formats that can be read:  
HDF4,HDF5,IPL,NU,TIF,IMG,BMP,RAW,SAV,SPE,PNG,JPG


Several of them are "special" (like HDF4) which means that the  
program directly targets a known version of that format but does  
not necessarily read all versions. Things like RAW (pure 16-bit  
binary) and SAV (IDL save file with 2-D image saved as "image"  
variable) are very xvis specific while the TIF may only read 16-bit  
versions.


*********************
GENERAL FRINGE ANALYSIS


To analyze a visar image. First load it into the viewer from File - > Open.
Time must go from left to right (and thus space should be up-down). Use
Tools -> Rotate or Flip to turn the image in the right direction. Then:


1) Select a 'Phase ROI' - usually most of the image containing the  
fringes, but sometimes a subset. Click on the Phase ROI button, and  
use the cursor to create a box on the screen.


2) Select a 'Plot ROI' - this has to be entirely within the phase  
ROI and is usually a set of several adjacent rows all the way  
across the Phase ROI.


3) Under the list 'Plot Type' drop-down menu select 'Freq  
spectrum'. This will produce a power spectrum of the fringes within  
your phase ROI at the x-positions of your plot ROI. If you have  
distinct fringes, you will see a distinct peak in your spectrum.  
Input 'f_min' and 'f_max' in the boxes above the image,  
corresponding to the fringe frequency just below the peak, and well  
above the peak (usually many times the peak frequency).


4) Click on 'Calculate Phase'. This will calculate a phase image  
which you can see if you go to Window -> Phase.


5) Select 'Phase unwrap' from the plot type drop-down menu.


6) Click 'Plot Now'. You should see a phase unwrapped plot on your  
right.


7) You can either cut and paste the plot into ppt or download the  
data into a text file and use your favorite plotting program.
*************************
UPDATES TO NEWER VERSIONS


Make sure to put old versions of xvis into a folder that IDL does  
not look in. Otherwise you run the risk of accidentally calling an  
old version since the filenames are all the same.


1) Time calibration is no longer on the main interface. 
Go to Tools- > Camera Settings to do all your timing stuff.
MinPix, MaxPix correspond to the boundaries of the sweep  
calibration, beyond which the sweep rate is held constant. Time  
calibration headings within Camera Settings are in terms of year +month, 
e.g. 0802 is 2008 Feb. The proliferation of sweep rates  
makes this interface still a little clumsy. At present I use an  
offline method for picking out fidus to calculate the time sweep  
then load the coefficients manually. Note that the coefficient "a1"  
corresponds to the linear sweep velocity rate (in ps/pixel). "a2"  
is the 2nd order coefficient, etc. You can plot the currently  
loaded sweep rate by selecting "sweep rate" in the plot type drop  
down menu at the main interface.


2) You can rotate the image by 90 degrees and flip it as necessary  
using Tools->Rotate or Flip (depending on which was your last  
version, you may already have this).


3) You can now load an image without the background subtracted  
(useful to see fidus on SOP)
And you can also load just the background. Just click on the 
check- boxes below "Reference"


4) To use the ghost fringe subtraction, set the ghostROI only  
around the region you need subtraction from. The more pre-break-out  
region you include the worse the subtraction gets. Play around with  
different regions to see what works best. You'll need to re-load  
the image each time if you want to reset the image.
*******************************************
