# TowedCameraTools
A variety of tools for working with benthic imagery from towed camera surveys

## Installation

You can install the development version of TowedCameraTools from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cyesson/TowedCameraTools")
```

## Examples

Load the library
``` r
library("TowedCameraTools")
```

Calculate area seen in an image from a downwardly angled camera (need to know height and angle of camera and its field of view (in water). Example is from [Long et al (2020)](https://doi.org/10.3389/fmars.2020.00460)
``` r
IA<-ImmageArea(Height=0.55, Angle=28.8, VFOV=40.3, HFOV=66.4)
```

Create stills from a video at regular intervals, choosing most in-focus still at each interval time period.
``` r
log<-FixedIntervalStills("myvideo.mp4", window=0.5, interval=30)
```

Estimate towed camera position from ships position using the layback method
``` r
Layback.Position<-Layback(StartLong=0, StartLat=0, EndLong=1, EndLat=1, Depth=100, WireLength=120)
```

Estimate height of annotations for an image (need to know height and angle of camera, as well as its field of view). 
``` r
# Camera setup based on GoPro Hero 5 using medium FOV (setup as in Long et al 2020)
myCamSetup <- CamSetup(FullHFOV=122.6, FullVFOV=94.4, UsedHFOV=94.4, UsedVFOV=55, 
	PixDimX=2704, PixDimY=1520, FullSensorWidth=6.17, FullSensorHeight=4.65, 
	CamAngle=28.8, CamHeight=550)
# Calculate height of annotation in image
Annotation.Height<-AnnotationHeight(X1=1562, Y1=535, X2=1543, Y2=62, CamSetup=myCamSetup)
```

Above example is based on the image below - an annotation of a deep-sea seapen from Greenland. The height is calculated at ~315 mm.
![Example linear annotation from Biigle platform](LinearAnnotationExample.png)
