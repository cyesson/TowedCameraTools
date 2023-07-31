###############################################################
# find height of object from annotation of towed camera image #

################################################################
# helper (undocumented) functions
################################################################

# convert degrees to radians
deg2rad<-function(deg){
    return(deg * pi / 180)}

################################################################
# Main (public) functions 
################################################################


#' Convert in-air FOV angle to in-water FOV angle
#' @param FOV value in degrees of the field of view
#' @param RefractiveIndex value of refractive index of water (default 1.34 - varies by temperature, salinity & pressure) 
#' @details getInWaterFOV() convert in-air field of view angle into in-water field of view angle based on refractive index of water
#' Formula used is 2 x asin( sin( FOV / 2 ) / RI ) where FOV= Field of View and RI = Refractive Index
#' The default value of refractive index is 1.34, which is a commonly used value representing shallow marine environments.
#' RI varies based on temperature, salinity and atmopheric pressure (~depth). A value of 1.33 is often used for shallow fresh water, but can go above 1.35 for very deep, saline, cold water.
#' You can use a value specific to your water properties if you know Temperature, salinity and pressure using rho::n_water()
#' (https://github.com/AlexCast/rho/)
#' @return A value representing the in-water field of view in degrees
#' @examples
#' FOV.water <- getInWaterFOV(100, RefractiveIndex=1.34)
#' @export

getInWaterFOV<-function(FOV, RefractiveIndex=1.34){
    return(asin(sin(FOV * pi / 180 / 2)/RefractiveIndex)/(pi/180)*2)
}


#' Calucating the pixel coordinate of the camera nadir (point directly below the camera)
#' @param VFOV value in degrees of the vertical field of view
#' @param CamAngle Camera angle in degrees relative to the horizon (0=horizontal, 90=downward facing)
#' @param PixDimX pixel dimension of image on x-axis (image width)
#' @param PixDimY pixel dimension of image on y-axis (image height)
#' @details Calucate the coordinate of the camera nadir (point directly below the camera) in pixel coordinates of the image
#' We are assuming that the camera roll is zero and only the pitch is non-zero
#' Therefor the nadir in the x dimension is half way across the image (in the x dimension)
#' We assume the top left pixel is 0,0 and positive numbers proceed down and to the right
#' @return A list containing the following elements
#'    VFOV:     vertical field of view of camera
#'    CamAngle: camera angle in degrees
#'    PixDimX:  pixel dimension of image on x-axis (image width)
#'    PixDimY:  pixel dimension of image on y-axis (image height)
#'    AngCAD:   angle from bottom of image (C) to camera (A) to mid point of image (D)
#'    AngDCA:   angle from mid point of image (D) to bottom of image (C) to camera (A) 
#'    AngBCA:   angle from nadir (B) to bottom of image (C) to camera (A) 
#'    LenAC:    length (in pixels) from camera (A) to bottom of image (C)
#'    LenBC:    length (in pixels) from bottom of image (B) to nadir (C)
#'    NadirX:   pixel coordinate of nadir in X dimension - fixed to 1/2 width of image (relative to the top left corner of the image which is position 0,0)
#'    NadirY:   pixel coordinate of nadir in Y dimension (relative to the top left corner of the image which is position 0,0)
#' @examples
#' myNadir <- getNadirXY(100, 45, 2700, 1900)
#' @export

getNadirXY<-function(VFOV, CamAngle, PixDimX, PixDimY){

    # find angles relevant for calculation
    AngCAD<-VFOV/2
    AngDCA<-180-AngCAD-CamAngle
    AngBCA<-180-AngDCA
    AngBAC<-90-AngBCA
    AngBAD<-AngBAC+AngCAD

    # find lengths in pixels
    LenAC<-sin(deg2rad(CamAngle))/(sin(deg2rad(AngCAD))/(PixDimY/2))
    LenBC<-sin(deg2rad(AngBAC))/(1/LenAC)

    NadirX<-PixDimX/2
    NadirY<-round(LenBC+PixDimY)

    return(list(VFOV=VFOV, CamAngle=CamAngle, 
                PixDimX=PixDimX, PixDimY=PixDimY,
                AngCAD=AngCAD, AngDCA=AngDCA, AngBCA=AngBCA,
                AngBAC=AngBAC, AngBAD=AngBAD,
                LenAC=LenAC, LenBC=LenBC,
                NadirX=NadirX, NadirY=NadirY))
}

#' Calculate the camera constant C
#' @param FullHFOV Value in degrees of the horizontal field of view of a camera when using the full sensor (e.g. 122.6 for gopro 5 in 4:3 W mode)
#' @param FullVFOV Value in degrees of the vertical field of view of a camera when using the full sensor (e.g. 94.4 for gopro 5 in 4:3 W mode)
#' @param UsedHFOV Value in degrees of the horizontal field of view of the camera as used in the image (e.g. 94.4 for gopro 5 in 16:9 M mode)
#' @param UsedVFOV Value in degrees of the vertical field of view of the camera as used in the image (e.g. 55.0 for gopro 5 in 16:9 W mode)
#' @param PixDimX Pixel dimension of image in X dimension (image width)
#' @param PixDimY Pixel dimension of image in Y dimension (image height)
#' @param FullSensorWidth width of sensor in mm as reported by manufacturer (e.g. gopro 5 has width 6.17)
#' @param FullSensorHeight height of sensor in mm as reported by manufacturer (e.g. gopro 5 has width 4.65)
#' @details Camera constant C is calculated based on the sensor size, pixel dimensions of the image
#' We are using formula c = f / p where f = focal length and p = pixel size
#' where p = X / W and X = pixel dimension of image and W = width of sensor
#' and where f is derived trigonimcally based on a triangle of adjacent length = half of sensor width and adjacent angle = 180 - (FOV/2)
#' @return A list containing the following elements
#'    FullHFOV: Value in degrees of the horizontal field of view of a camera when using the full sensor
#'    FullVFOV: Value in degrees of the vertical field of view of a camera when using the full sensor
#'    UsedHFOV: Value in degrees of the horizontal field of view of the camera as used in the image
#'    UsedVFOV: Value in degrees of the vertical field of view of the camera as used in the image
#'    FullSensorWidth: width of sensor in mm as reported by manufacturer
#'    FullSensorHeight: height of sensor in mm as reported by manufacturer
#'    PixDimX:  pixel dimension of image on x-axis (image width)
#'    PixDimY:  pixel dimension of image on y-axis (image height)
#'    LenAF:    length (mm) between the lens and the sensor
#'    AngACF:   Angle equal to half full FOV 
#'    AngCAF:   Angle = 90-AngACF
#'    FocalLength: Calculated focal length of the camera
#'    AngECF:   Angle equal to half the Used FOV
#'    AngCEF:   Angle = 90-AngCEF
#'    LenEF:    Length (mm) half the used sensor width
#'    UsedSensorWidth: width of used part of the sensor in mm 
#'    PixSize: UsedSensorWidth / PixDimX
#'    CamConstantC: Calculated camera constant C
#' @examples
#' cc<-getCamConstantC(122.6, 94.4, 94.4, 55.0, 2704, 1520, 6.17, 4.65)
#' @export

# calculate camera constant c based on sensor dimensions
getCamConstantC<-function(FullHFOV, FullVFOV,
                          UsedHFOV, UsedVFOV,
                          PixDimX, PixDimY,
                          FullSensorWidth, FullSensorHeight){

    # calculate functional sensor width based on % of sensor used 
    LenAF<-FullSensorWidth/2
    AngACF<-FullHFOV/2
    AngCAF<-90-AngACF

    # Focal length = length CF
    FocalLength<-sin(deg2rad(AngCAF))*(LenAF/(sin(deg2rad(AngACF))))
    AngECF<-UsedHFOV/2
    AngCEF<-90-AngECF
    LenEF<-sin(deg2rad(AngECF))*(FocalLength/(sin(deg2rad(AngCEF))))
    UsedSensorWidth<-2*LenEF

    # find pixel size
    PixSize<-UsedSensorWidth/PixDimX

    # find camera constant C
    CamConstantC<-FocalLength/PixSize

    return(list(FullHFOV=FullHFOV, FullVFOV=FullVFOV,
                UsedHFOV=UsedHFOV, UsedVFOV=UsedVFOV,
                PixDimX=PixDimX, PixDimY=PixDimY,
                FullSensorWidth=FullSensorWidth,
                FullSensorHeight=FullSensorHeight,
                LenAF=LenAF, AngACF=AngACF, AngCAF=AngCAF,
                FocalLength=FocalLength,
                AngECF=AngECF, AngCEF=AngCEF, LenEF=LenEF,
                UsedSensorWidth=UsedSensorWidth,
                PixSize=PixSize,
                CamConstantC=CamConstantC))
}


#' Calculate camera nadir and constant C
#' @param FullHFOV Value in degrees of the horizontal field of view of a camera when using the full sensor (e.g. 122.6 for gopro 5 in 4:3 W mode)
#' @param FullVFOV Value in degrees of the vertical field of view of a camera when using the full sensor (e.g. 94.4 for gopro 5 in 4:3 W mode)
#' @param UsedHFOV Value in degrees of the horizontal field of view of the camera as used in the image (e.g. 94.4 for gopro 5 in 16:9 M mode)
#' @param UsedVFOV Value in degrees of the vertical field of view of the camera as used in the image (e.g. 55.0 for gopro 5 in 16:9 W mode)
#' @param PixDimX Pixel dimension of image in X dimension (image width)
#' @param PixDimY Pixel dimension of image in Y dimension (image height)
#' @param FullSensorWidth width of sensor in mm as reported by manufacturer (e.g. gopro 5 has width 6.17)
#' @param FullSensorHeight height of sensor in mm as reported by manufacturer (e.g. gopro 5 has width 4.65)
#' @param CamAngle Camera angle in degrees relative to the horizon (0=horizontal, 90=downward facing)
#' @param CamHeight Camera height in mm above the ground
#' @details call functions getNadirXY and getCamConstantC and package output into a single list object
#' @return A list with the following elements
#'    FullHFOV: Value in degrees of the horizontal field of view of a camera when using the full sensor
#'    FullVFOV: Value in degrees of the vertical field of view of a camera when using the full sensor
#'    UsedHFOV: Value in degrees of the horizontal field of view of the camera as used in the image
#'    UsedVFOV: Value in degrees of the vertical field of view of the camera as used in the image
#'    FullSensorWidth: width of sensor in mm as reported by manufacturer
#'    FullSensorHeight: height of sensor in mm as reported by manufacturer
#'    UsedSensorWidth: width of used part of the sensor in mm 
#'    CamAngle: Angle of camera 
#'    CamHeight: Height of camera above the ground (mm)
#'    FocalLength: Calculated focal length of the camera
#'    CamConstantC: Calculated camera constant C
#'    PixDimX:  pixel dimension of image on x-axis (image width)
#'    PixDimY:  pixel dimension of image on y-axis (image height)
#'    PPX:      principle point of the image on the X axis (middle of image)
#'    PPX:      principle point of the image on the Y axis (middle of image)
#'    NadirX:   X pixel coordinate of point directly beneath camera
#'    NadirY:   Y pixel coordinate of point directly beneath camera
#' @examples
#' CamSetup <- getCamSetup(122.6, 94.4, 94.4, 55, 2704, 1520, 6.17, 4.65, 28.8, 550)
#' @export

getCamSetup<-function(FullHFOV, FullVFOV,
                      UsedHFOV, UsedVFOV,
                      PixDimX, PixDimY,
                      FullSensorWidth, FullSensorHeight,
                      CamAngle, CamHeight){

    # call nadir and constant functions
    NadirInfo<-getNadirXY(UsedVFOV, CamAngle, PixDimX, PixDimY)
    CCInfo<-getCamConstantC(FullHFOV, FullVFOV,
                            UsedHFOV, UsedVFOV,
                            PixDimX, PixDimY,
                            FullSensorWidth, FullSensorHeight)

    # return all info for main function
    return(list(FullHFOV=FullHFOV, FullVFOV=FullVFOV,
                UsedHFOV=UsedHFOV, UsedVFOV=UsedVFOV,
                FullSensorWidth=FullSensorWidth,
                FullSensorHeight=FullSensorHeight,
                UsedSensorWidth=CCInfo$UsedSensorWidth,
                CamAngle=CamAngle, CamHeight=CamHeight,
                FocalLength=CCInfo$FocalLength,
                CamConstantC=CCInfo$CamConstantC,
                PixDimX=PixDimX, PixDimY=PixDimY,
                PPX=PixDimX/2, PPY=PixDimY/2,
                NadirX=NadirInfo$NadirX,NadirY=NadirInfo$NadirY))
}

#' Calculate height of annotation in image
#' @param X1 x pixel coordinate of base of annotation object
#' @param Y1 y pixel coordinate of base of annotation object
#' @param X2 x pixel coordinate of top of annotation object
#' @param Y1 y pixel coordinate of top of annotation object
#' @param NadirX x pixel coordinate of point directly beneath camera
#' @param NadirY y pixel coordinate of point directly beneath camera
#' @param PPX x pixel denoting principle point of image (middle)
#' @param PPY y pixel denoting principle point of image (middle)
#' @param CamConstantC camera constant c
#' @param CamHeight height of camera above the ground (mm)
#' @details Calculate height of an annotation in an low oblique image where camera specifics such as FOV, height & angle are known. Calculation is based on the formula described in S. Verykokou and C. Ioannidis, "Metric exploitation of a single low oblique aerial image," in FIG Working Week 2015: From the Wisdom of the Ages to the Challenges of the Modern World, May 17-21, 2015, Sofia, Bulgaria.
#' @return A list with the following elements
#' X1: x pixel coordinate of base of annotation object
#' Y1: y pixel coordinate of base of annotation object
#' X2: x pixel coordinate of top of annotation object
#' Y2: y pixel coordinate of top of annotation object
#' NadirX: x pixel coordinate of point directly beneath camera
#' NadirY: y pixel coordinate of point directly beneath camera
#' PPX: x pixel denoting principle point of image (middle)
#' PPY: y pixel denoting principle point of image (middle)
#' CamConstantC: camera constant C
#' CamHeight: height of camera above the ground (mm)
#' ObjPixX: width of annotation in pixels
#' ObjPixY: height of annotation in pixels
#' ObjPixXY: length of annotation in pixels
#' ObjAngR: angle of annotation line in the image
#' A: intermediate calculation A
#' B: intermediate calculation B
#' C: intermediate calculation C
#' D: intermediate calculation D
#' E: intermediate calculation E
#' Eq1: equation 1 (Verykokou & Ioannidis 2015)
#' Eq2: equation 2 (Verykokou & Ioannidis 2015)
#' Eq3: equation 3 (Verykokou & Ioannidis 2015)
#' Eq4: equation 4 (Verykokou & Ioannidis 2015)
#' ObjHeight: Height of object above the ground (mm)
#' ObjWidth: width of object as seen in image (mm)
#' ObjLength: Length of object as seen in image (mm - assumes object is inclined on a plane perpendicular to the direction the camera is pointing)
#' @examples
#' getAnnotationHeightFull(100, 100, 100, 50, 200, 200, 100, 100, 1, 1)
#' @export

# find height of annotation object in image
getAnnotationHeightFull<-function(X1,Y1, X2, Y2,
                                  NadirX, NadirY,
                                  PPX, PPY,
                                  CamConstantC, CamHeight){

    # pixel dimensions of object
    ObjPixX<-abs(X1-X2)
    ObjPixY<-abs(Y1-Y2)
    ObjPixXY<-(ObjPixX^2+ObjPixY^2)^0.5

    # object angle in radians (0=horizontal, pi/2=vertical)
    ObjAngR<-atan(ObjPixY/ObjPixX)
    if(is.na(ObjAngR)){
        ObjAngR<-0
    }
    
    # use formula from Verykokou, S. and Ioannidis, C., 2015. Metric exploitation of a single low oblique aerial image. 
    # There are a lot of intermediate steps that require calculation
    A<-((X1-PPX)^2)+((Y1-PPY)^2)
    B<-((NadirX-PPX)^2)+((NadirY-PPY)^2)
    C<-((NadirX-X1)^2)+((NadirY-Y1)^2)
    D<-((X2-PPX)^2)+((Y2-PPY)^2)
    E<-((NadirX-X2)^2)+((NadirY-Y2)^2)

    Eq1<-((4*(CamConstantC^2+A)*(CamConstantC^2+B))-(((2*(CamConstantC^2))+A+B-C)^2))^0.5
    Eq2<-(2*(CamConstantC^2))+A+B-C
    Eq3<-((4*(CamConstantC^2+D)*(CamConstantC^2+B))-(((2*(CamConstantC^2))+D+B-E)^2))^0.5
    Eq4<-(2*(CamConstantC^2))+D+B-E

    ObjHeight<-CamHeight*(1-((Eq1/Eq2)/(Eq3/Eq4)))
    if(ObjAngR==0){  ## fix if we have a vertical line
        ObjLength<-ObjHeight
        ObjWidth<-0
    } else {
        ObjLength<-ObjHeight/sin(ObjAngR)
        ObjWidth<-(ObjLength^2-ObjHeight^2)^0.5
    }

    return(list(X1=X1,Y1=Y1, X2=X2, Y2=Y2,
                NadirX=NadirX, NadirY=NadirY,
                PPX=PPX, PPY=PPY,
                CamConstantC=CamConstantC, CamHeight=CamHeight,
                ObjPixX=ObjPixX,
                ObjPixY=ObjPixY,
                ObjPixXY=ObjPixXY,
                ObjAngR=ObjAngR,
                A=A, B=B, C=C, D=D, E=E,
                Eq1=Eq1, Eq2=Eq2, Eq3=Eq3, Eq4=Eq4,
                ObjHeight=ObjHeight,
                ObjWidth=ObjWidth,
                ObjLength=ObjLength))
}
                
getAnnotationHeight<-function(X1,Y1, X2, Y2,
                              CamSetup){

    # call main function parsing Camera setup
    return(getAnnotationHeightFull(X1,Y1, X2, Y2,
                                   CamSetup$NadirX, CamSetup$NadirY,
                                   CamSetup$PPX, CamSetup$PPY,
                                   CamSetup$CamConstantC,
                                   CamSetup$CamHeight))
}

#' Calculate height of annotation in image
#' @param X1 x pixel coordinate of base of annotation object
#' @param Y1 y pixel coordinate of base of annotation object
#' @param X2 x pixel coordinate of top of annotation object
#' @param Y1 y pixel coordinate of top of annotation object
#' @param CamSetup details of camera as output from function getCamSetup
#' @details Calculate height of an annotation in an image where camera specifics such as FOV, height & angle are known
#' @return A list with the following elements
#' X1: x pixel coordinate of base of annotation object
#' Y1: y pixel coordinate of base of annotation object
#' X2: x pixel coordinate of top of annotation object
#' Y2: y pixel coordinate of top of annotation object
#' NadirX: x pixel coordinate of point directly beneath camera
#' NadirY: y pixel coordinate of point directly beneath camera
#' PPX: x pixel denoting principle point of image (middle)
#' PPY: y pixel denoting principle point of image (middle)
#' CamConstantC: camera constant C
#' CamHeight: height of camera above the ground (mm)
#' ObjPixX: width of annotation in pixels
#' ObjPixY: height of annotation in pixels
#' ObjPixXY: length of annotation in pixels
#' ObjAngR: angle of annotation line in the image
#' A: intermediate calculation A
#' B: intermediate calculation B
#' C: intermediate calculation C
#' D: intermediate calculation D
#' E: intermediate calculation E
#' Eq1: equation 1 (Verykokou & Ioannidis 2015)
#' Eq2: equation 2 (Verykokou & Ioannidis 2015)
#' Eq3: equation 3 (Verykokou & Ioannidis 2015)
#' Eq4: equation 4 (Verykokou & Ioannidis 2015)
#' ObjHeight: Height of object above the ground (mm)
#' ObjWidth: width of object as seen in image (mm)
#' ObjLength: Length of object as seen in image (mm - assumes object is inclined on a plane perpendicular to the direction the camera is pointing)
#' @examples
#' CamSetup <- getCamSetup(122.6, 94.4, 94.4, 55, 2704, 1520, 6.17, 4.65, 28.8, 550)
#' getAnnotationHeight(100, 100, 100, 50, CamSetup)
#' @export
