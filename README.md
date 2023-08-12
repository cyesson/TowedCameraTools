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

Estimate in-water field of view from in-air field of view 
``` r
FOV.water <- InWaterFOV(100, RefractiveIndex=1.34)
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
