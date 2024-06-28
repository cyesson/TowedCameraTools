################################################################
## Start with simple function to estimate image width from laser validation points
################################################################

#' Estimate width of image based on laser annotations
#' @param LaserXY1 - pixel coordinates (top left is 0,0) of first laser dot
#' @param LaserXY2 - pixel coordinates (top left is 0,0) of second laser dot
#' @param LaserDistanceM - Distance in meters between laser dots
#' @param PixelDimensions - dimensions of image in pixels
#' @details Estimate width of image based on laser dots. Input requires pixel coordinates of two laser dots seen in an image. It assumes you have a horizontal, flat annotated image. 
#' @return List containing the following elements
#' LaserXY1 - Input parameter
#' LaserXY2 - Input parameter
#' LaserDistanceM - Input parameter
#' PixelDimensions - Input parameter
#' LaserHeight - Height of laser dots in image (percentage of image from bottom)
#' LaserDistanceXPix - Distance between lasers along the x-axis in pixels
#' LaserDistanceYPct - Variability in laser dot height (Y axis) expressed as a percentage of image pixels
#' ImageWidth - Inferred image with at the height of the laser dots.
#' @examples
#' x <- ImageWidthFromLasers(c(10, 10), c(50, 10), 0.2, c(2000, 1000))
#' @export

ImageWidthFromLasers <- function( LaserXY1, LaserXY2, LaserDistanceM, PixelDimensions ){

    # work out proportional height of dots in image (0 is bottom, 1 is top)
    LaserHeight <- 1 - (mean(LaserXY1[2], LaserXY2[2]) / PixelDimensions[2])
    
    # Laser width in pixels
    LaserDistanceXPix <- abs(LaserXY1[1] - LaserXY2[1])

    # image width is laser distance scaled up by the proportion of image covered by the laser
    ImageWidth <- LaserDistanceM * PixelDimensions[1] / LaserDistanceXPix

    # check laser points are horizontal in the image
    LaserDistanceYPix <- abs(LaserXY1[2] - LaserXY2[2])

    # check difference between Y coordinates on lasers in terms of % of image height
    LaserDistanceYPct <- LaserDistanceYPix / PixelDimensions[2]
    if(LaserDistanceYPct >= 0.05){
        print("Warning: lasers are not horizontal in the image - image width will not be a good estimate")
    }

    return(list(LaserXY1=LaserXY1,
                LaserXY2=LaserXY2,
                LaserDistanceM=LaserDistanceM,
                PixelDimensions=PixelDimensions,
                LaserHeight=LaserHeight,
                LaserDistanceXPix=LaserDistanceXPix,
                LaserDistanceYPct=LaserDistanceYPct,
                ImageWidth=ImageWidth))
}

################################################################
## Main function to do validation
################################################################

#' Estimate width of image based on laser annotations
#' @param LaserXY1 - pixel coordinates (top left is 0,0) of first laser dot
#' @param LaserXY2 - pixel coordinates (top left is 0,0) of second laser dot
#' @param LaserDistanceM - Distance in meters between laser dots
#' @param PixelDimensions - dimensions of image in pixels
#' @param Height - Height of the camera above the seabed (in m)
#' @param Angle - Angle (in dgrees) of the camera pointing down from the horizontal
#' @param VFOV - Vertical field of view angle (in degrees)
#' @param HFOV - Horizontal field of view angle (in degrees)
#' @details Compare image width calculated with trigonometry and in-image parallel lasers. Input requires pixel coordinates of two laser points seen in an image and image details required for trigonometric area calculation. This assumes a horizontal camera at a fixed oblique angle.
#' @return List containing the following elements
#' LaserXY1 - Input parameter
#' LaserXY2 - Input parameter
#' LaserDistanceM - Input parameter
#' PixelDimensions - Input parameter
#' Height - Input parameter
#' Angle - Input parameter
#' VFOV - Input parameter
#' HFOV - Input parameter
#' LaserHeight - Height of laser dots in image (proportion of image height 0=bottom, 1=top)
#' LaserWidth - Laser-based width of image at height of lasers 
#' TrigWidth - Trigometry-based width of image at height of lasers
#' WidthDifference - Absolute difference between two image width measurements
#' BestAngle - Camera angle that produces trigonometric image-width calculation that matches laser width (assuming fixed camera height)
#' BestHeight - Camera height that produces trigonometric image-width calculation that matches laser width (assuming fixed camera angle)
#' @examples
#' x <- ValidateWithLasers(c(400, 900), c(700, 900), 0.2, c(2000, 1000), Height=0.55, Angle=28.8, VFOV=40.3, HFOV=66.4)
#' @export

ValidateWithLasers <- function(LaserXY1, LaserXY2, LaserDistanceM, PixelDimensions,
                               Height, Angle, VFOV, HFOV){

    # calculate width of image from lasers
    LaserWidth <- ImageWidthFromLasers(LaserXY1, LaserXY2, LaserDistanceM, PixelDimensions)

    # calculate inferred image width from trigonometry of oblique angle camera
    TrigWidth <- ImageWidth(Height, Angle, VFOV, HFOV, Position=LaserWidth$LaserHeight)

    # difference in widths
    WidthDifference <- abs(LaserWidth$ImageWidth - TrigWidth)
 
    # assuming the height is correct, what angle would you need to make the trig width match?
    # try a bunch of angles and find the closest matching one
    BestDiff <- WidthDifference
    LastDiff <- 9999999
    BestAngle <- Angle

    # incremental steps down then up of camera angle
    for(i in c(-0.1, 0.1)){
        TestAngle <- Angle
        CurrentDiff <- BestDiff
        while(CurrentDiff <= BestDiff){
            TestAngle <- TestAngle + i
            TestTrigWidth <- ImageWidth(Height, TestAngle, VFOV, HFOV, Position=LaserWidth$LaserHeight)
            CurrentDiff <- abs(LaserWidth$ImageWidth - TestTrigWidth)
            if(CurrentDiff < BestDiff){
                BestDiff <- CurrentDiff
                BestAngle <- TestAngle
            }
        }        
    }

    BestAngleDiff<-BestDiff

    BestDiff <- WidthDifference
    LastDiff <- 9999999
    BestHeight <- Height

    # incremental steps down then up of camera height
    for(i in c(-0.01, 0.01)){
        TestHeight<-Height
        CurrentDiff <- BestDiff
        while(CurrentDiff <= BestDiff){
            TestHeight <- TestHeight + i
            TestTrigWidth <- ImageWidth(TestHeight, Angle, VFOV, HFOV, Position=LaserWidth$LaserHeight)
            CurrentDiff <- abs(LaserWidth$ImageWidth - TestTrigWidth)
            if(CurrentDiff < BestDiff){
                BestDiff <- CurrentDiff
                BestHeight <- TestHeight
            }
        }        
    }
    BestHeightDiff <- BestDiff
    
    # find image areas for original setup and best angle / best height setup
    OrigSetup<- ImageArea( Height, Angle, VFOV, HFOV, Proportion=1)
    BestAngleSetup<- ImageArea( Height, BestAngle, VFOV, HFOV, Proportion=1)
    BestHeightSetup<- ImageArea( BestHeight, Angle, VFOV, HFOV, Proportion=1)

    return(list(LaserXY1=LaserXY1,
                LaserXY2=LaserXY2,
                LaserDistanceM=LaserDistanceM,
                PixelDimensions=PixelDimensions,
                Height=Height,
                Angle=Angle,
                VFOV=VFOV,
                HFOV=HFOV,
                LaserHeight=LaserWidth$LaserHeight,
                LaserWidth=LaserWidth$ImageWidth,
                TrigWidth=TrigWidth,
                WidthDifference=abs(LaserWidth$ImageWidth-TrigWidth),
                BestAngle=BestAngle,
                BestHeight=BestHeight,
                TrigAreaOrig=OrigSetup$ImageArea,
                TrigAreaBestHeight=BestHeightSetup$ImageArea,
                TrigAreaBestAngle=BestAngleSetup$ImageArea
                ))
}
