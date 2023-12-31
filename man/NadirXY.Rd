% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HeightsInImage.R
\name{NadirXY}
\alias{NadirXY}
\title{Calucating the pixel coordinate of the camera nadir (point directly below the camera)}
\usage{
NadirXY(VFOV, CamAngle, PixDimX, PixDimY)
}
\arguments{
\item{VFOV}{value in degrees of the vertical field of view}

\item{CamAngle}{Camera angle in degrees relative to the horizon (0=horizontal, 90=downward facing)}

\item{PixDimX}{pixel dimension of image on x-axis (image width)}

\item{PixDimY}{pixel dimension of image on y-axis (image height)}
}
\value{
A list containing the following elements
VFOV:     vertical field of view of camera
CamAngle: camera angle in degrees
PixDimX:  pixel dimension of image on x-axis (image width)
PixDimY:  pixel dimension of image on y-axis (image height)
AngCAD:   angle from bottom of image (C) to camera (A) to mid point of image (D)
AngDCA:   angle from mid point of image (D) to bottom of image (C) to camera (A)
AngBCA:   angle from nadir (B) to bottom of image (C) to camera (A)
LenAC:    length (in pixels) from camera (A) to bottom of image (C)
LenBC:    length (in pixels) from bottom of image (B) to nadir (C)
NadirX:   pixel coordinate of nadir in X dimension - fixed to 1/2 width of image (relative to the top left corner of the image which is position 0,0)
NadirY:   pixel coordinate of nadir in Y dimension (relative to the top left corner of the image which is position 0,0)
}
\description{
Calucating the pixel coordinate of the camera nadir (point directly below the camera)
}
\details{
Calucate the coordinate of the camera nadir (point directly below the camera) in pixel coordinates of the image
We are assuming that the camera roll is zero and only the pitch is non-zero
Therefor the nadir in the x dimension is half way across the image (in the x dimension)
We assume the top left pixel is 0,0 and positive numbers proceed down and to the right
}
\examples{
myNadir <- NadirXY(100, 45, 2700, 1900)
}
