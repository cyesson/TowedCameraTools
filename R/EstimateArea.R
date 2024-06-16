# Estimate area in towed sled image

################################################################
# Helper functions
################################################################
# convert degrees to radians
deg2rad<-function(deg){
    return(deg * pi / 180)}

################################################################
# Main function
################################################################

#' Estimate area seen in tilted camera pointing at the seabed
#' @param Height - Height of the camera above the seabed (in m)
#' @param Angle - Angle (in dgrees) of the camera pointing down from the horizontal
#' @param VFOV - Vertical field of view angle (in degrees)
#' @param HFOV - Horizontal field of view angle (in degrees)
#' @param Proportion - Proportion of image to use to estimate area (0-1 values default = 1, using 0.99 cuts off the top 1% of the image)
#' @details Estimate area seen in tilted camera pointing at the seabed based on Nakajima, R. et al. (2014) A new method for estimating the area of the seafloor from oblique images taken by deep-sea submersible survey platforms. JAMSTEC Report of Research and Development, 19, pp.59-66, as used in Long et al (2020) https://www.frontiersin.org/articles/10.3389/fmars.2020.00460/full
#' @return List containing the following calculations used in estimating the area (see Long et al 2020):
#' Height - input parameter
#' Angle - input parameter
#' VFOV - input parameter
#' HFOV - input paramter
#' Alpha - Angle (radians)
#' Beta - Angle (radians)
#' Delta - Angle (radians)
#' Theta - Angle (radians)
#' LengthAE - Length (m) - width of top of image (based on the proportion of image selected)
#' LengthBD - Length (m) - width of bottom of image (always the bottom of image)
#' LengthGF - Length (m) 
#' AreaS - Area (m2)
#' ImageArea - Area (m2)
#' @examples
#' x <- ImageArea(0.55, 28.8, 40.3, 66.4)
#' @export

ImageArea <- function(Height, Angle, VFOV, HFOV, Proportion=1){
    # convert angles to radians

    # convert degree angles to radians
    Theta<-deg2rad(Angle)
    Alpha<-deg2rad(VFOV)
    Beta<-deg2rad(HFOV)

    # Use terminology from Nakajima et al to describe angles
    OH<-Height

    # PI-(PI/2+θ+α/2)
    Delta <-pi-((pi/2) + Theta + Alpha/2)

    # 2tan(β/2)*OH(cos(D)^-1)
    AE <- 2*tan(Beta/2) * OH * (cos(Delta + (Alpha*Proportion))^-1)

    # 2tan(β/2)*OH(cos(δ)^-1)
    BD <- 2*tan(Beta/2) * OH * cos(Delta)^-1

    # OH(tan(θ)-tanδ)
    GF <- OH * (tan(Delta + (Alpha*Proportion)) - tan(Delta))

    # (AE + BD) x CF/2
    S <- (AE+BD)*GF/2

    return(list(Height=Height,
                Angle=Angle,
                VFOV=VFOV,
                HFOV=HFOV,
                Alpha=Alpha,
                Beta=Beta,
                Delta=Delta,
                Theta=Theta,
                LengthAE=AE,
                LengthBD=BD,
                LengthGF=GF,
                AreaS=S,
                ImageArea=S))
}

################################################################
# Related function that depends on the same code
################################################################

#' Estimate width of image at a given point seen in tilted camera pointing at the seabed
#' @param Height - Height of the camera above the seabed (in m)
#' @param Angle - Angle (in dgrees) of the camera pointing down from the horizontal
#' @param VFOV - Vertical field of view angle (in degrees)
#' @param HFOV - Horizontal field of view angle (in degrees)
#' @param Position - Position in the image to calculate width (Proption 0-1, where 0 is along the bottom of the image, 1 is the top and 0.5 (default) is the midline.
#' @details Estimate image width seen in tilted camera pointing at the seabed based on Nakajima, R. et al. (2014) A new method for estimating the area of the seafloor from oblique images taken by deep-sea submersible survey platforms. JAMSTEC Report of Research and Development, 19, pp.59-66, as used in Long et al (2020) https://www.frontiersin.org/articles/10.3389/fmars.2020.00460/full
#' @return width of the image in m at the position selected
#' @examples
#' x <- ImageWidth(Height=0.55, Angle=28.8, VFOV=40.3, HFOV=66.4, Position=0.5)
#' @export

ImageWidth <- function(Height, Angle, VFOV, HFOV, Position=0.5){

    # call image area function parsing position as the proportion
    ImageArea <- ImageArea(Height, Angle, VFOV, HFOV, Position)

    return(ImageArea$LengthAE)
}
