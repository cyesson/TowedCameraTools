################################################################
# Calculate position of towed camera by the layback method

################################################################
# Main function
################################################################

#' Estimate position of towed camera using the layback method
#' @param StartLong - Longitude (decimal degrees) of the start of the tow
#' @param StartLat - Latitude (decimal degrees) of the start of the tow
#' @param EndLong - Longitude (decimal degrees) of the end of the tow
#' @param EndLat - Latitude (decimal degrees) of the end of the tow
#' @param Depth - Depth (m) of the camera on the seabed
#' @param WireLength - Length of wire (m) payed out for the towed camera
#' @param Offset - Distance (m) from the GPS reciever to the back of the vessel (default 0)
#' @details Estimate position of towed camera from the vessel position using the layback method. Trigonometrically calculate the distance of camera from vessel using the water depth and amount of wire payed out. This assumes the camera is directly behind the ship (based on inferred direction of travel). 
#' @return List containing the following calculations used in estimating layback position
#' StartLong - Input parameter
#' StartLat - Input parameter
#' EndLong - Input parameter
#' EndLat - Input parameter
#' Depth - Input parameter
#' WireLength - Input parameter
#' Offset - Input parameter
#' Layback.Distance. - Distance of camera behind vessel
#' Layback.StartLong - Longitude of camera start position
#' Layback.StartLat - Latitude of camera start position
#' Layback.EndLong - Longitude of camera end position
#' Layback.EndLat - Latitude of camera end position
#' Depth - Input parameter
#' @examples
#' x <- Layback(StartLong=0, StartLat=0, EndLong=1, EndLat=1, Depth=100, WireLength=120)
#' @export
Layback<-function(StartLong, StartLat, EndLong, EndLat, Depth, WireLength, Offset=0){

    # calculate direction of travel - camera is in the opposite direction
    Bearing <- geosphere::bearing(c(EndLong, EndLat), c(StartLong,StartLat))

    # find layback points using start / end pos, bearing and layback distance in metres
    # use trigonometry based on wire out & depth to estimate distance behind the vessel
    Layback.Distance <- (( WireLength^2 - Depth^2 ) ^0.5) + Offset

    # find new positions using bearing and layback distance
    Layback.Start<-geosphere::destPoint(c(StartLong,StartLat), Bearing, Layback.Distance)
    Layback.End<-geosphere::destPoint(c(EndLong,EndLat), Bearing, Layback.Distance)

    # extract lat & long
    Layback.StartLat<-Layback.Start[,2]
    Layback.StartLong<-Layback.Start[,1]
    Layback.EndLat<-Layback.End[,2]
    Layback.EndLong<-Layback.End[,1]

    # return a list of values
    return(list(StartLong=StartLong,
                StartLat=StartLat,
                EndLong=EndLong,
                EndLat=EndLat,
                Depth=Depth,
                WireLength=WireLength,
                Offset=Offset,
                Layback.Distance,
                Layback.StartLat=Layback.StartLat[[1]],
                Layback.StartLong=Layback.StartLong[[1]],
                Layback.EndLat=Layback.EndLat[[1]],
                Layback.EndLong=Layback.EndLong[[1]]))
}
