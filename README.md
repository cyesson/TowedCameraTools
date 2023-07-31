# TowedCameraTools
A variety of tools for working with benthic imagery from towed camera surveys

## Installation

You can install the development version of TowedCameraTools from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cyesson/TowedCameraTools")
```

## Examples

Calculate area seen in an image from a downwardly angled camera (need to know height and angle of camera and its field of view (in water).
``` r
IA<-ImmageArea(Height=0.55, Angle=28.8, VFOV=40.3, HFOV=66.4)
```

Create stills from a video at regular intervals, choosing most in-focus still at each interval time period.
``` r
log<-FixedIntervalStills("myvideo.mp4", window=0.5, interval=30)
```

Estimate height of annotations for an image (need to know height and angle of camera, as well as its field of view. 
