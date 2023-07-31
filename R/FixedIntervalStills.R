################################################################
# GetStillImages - Fetch still images from video at fixed intervals

####required packages
library("exifr") # read video metadata
library('imager') # image readign
library('tools') # file extension remover code
library('utils') # file extension remover code
#' @import tools
#' @import imager
#' @import exifr
#' @importFrom utils txtProgressBar setTxtProgressBar

################################################################
# Helper functions
################################################################
# convert degrees to radians
deg2rad<-function(deg){
    return(deg * pi / 180)}

################################################################
# Main function
################################################################

#' Fetch stills from video at fixed intervals
#' @param video - file name of your video survey
#' @param outdir - directory for output images (default = where video is located
#' @param interval - interval in seconds for stills (default 30 seconds)
#' @param window - time window around interval to look for best image (default 1 second)
#' @param dark.max - maximum proportion of dark pixels acceptable for image (default 0.1)
#' @param outdir - directory for output images (default = where video is located)
#' @param verbose - print details of progress
#' @details Fetch best-focus still images from video at fixed intervals 
#' @return Dataframe of timecode, image files, focus score & darkness pixels 
#' @export

FixedIntervalStills<-function(video, outdir=NA, window=1, interval=30,
                              dark.max=0.1, time.offset=0, verbose=F){

    # get video metadata
    meta<-read_exif(video)

    # kernel for focus calculation
    ck<-as.cimg(c(0,1,0,1,-4,1,0,1,0))

    # check if output directory is given
    if(is.na(outdir)){
        # find directory of vidoe file 
        outdir <- dirname(video)
    }

    # find basename for video
    station<-file_path_sans_ext(basename(video))

    # store current time code and duration of video
    timecode<-time.offset # for first test video
    duration<-meta$Duration
    pix<-meta$SourceImageWidth * meta$SourceImageHeight

    # find number of frames to examine (window x frame rate)
    frames<-round(meta$VideoFrameRate * window)
    frame.int<-1/meta$VideoFrameRate

    if(verbose){
        print(paste("video: ", video, ", window: ", window, ", interval: ", interval))
    }

    pb = txtProgressBar(min = 0, max = duration, initial = 0, style=3)
    setTxtProgressBar(pb, 0)

    # set up returnable dataframe
    outdf<-data.frame(Time=seq(timecode, duration, interval), File=NA, Focus=NA, DarkPCT=NA)

    # keep going while we have video left to run
    while(timecode< duration){

        # initialise best focus and darkness values
        focus.best<-0
        dark.best<-0
        best.j<-0

        # now loop through stills
        for(j in 0:frames){

            RGB<-load.video(video, skip.to=timecode+(j*frame.int), frames=1)
                # convert to greyscale
            grey<-grayscale(RGB)
                #grey<-rgb2grey(RGB)
            grey.dark<-sum(grey<0.0001)/pix

            # acceptable darkness levels (proxy for camera angle)
            if(grey.dark<dark.max){
                gck<-convolve(grey*10^5, ck)
                focus<-sd(gck)^2

                # if better focus then keep
                if(focus>focus.best){
                    focus.best<-focus
                    best.j<-j
                    dark.best<-grey.dark
                }
            }
        }

        # fetch best still
        RGB<-load.video(video, skip.to=timecode+(best.j*frame.int), frames=1)

        # construct file name for image
        outname<-paste(station,"-", timecode, ".png",sep="")
        save.image(RGB, outname)

        outdf[which(outdf$Time==timecode),]<-c(timecode, outname, round(focus.best,0), dark.best)
        ## print(paste("Station:", station, ", File:", flist[i],
        ##             ", Timecode:", timecode, ", Focus:", focus.best,
        ##             "Darkness:", dark.best))

        # advance time code
        timecode<-timecode+interval

        # increment progress bar
        setTxtProgressBar(pb, timecode)
    }

    # end progress bar
    close(pb)

    return(outdf)

}
