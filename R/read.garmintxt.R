#' Read text data files produced by Garmin GPS units
#'
#' This function reads data pulled from Garmin GPS units using the 
#' \code{gpsbabel} program.
#'
#'
#' @param file file name
# @param na.rm remove columns with all NA entries?
#' @param encoding file encoding (usually "latin1" but if you're having probelms, try changing this.)
# @param
#'
#' @author David L. Miller
#' @import lubridate
#' @examples library(readGPS)
#'
#' # args
#' fn <- "akutco.txt"
#'
#' dat <- read.garmintxt(fn)

read.garmintxt <- function(file, encoding="latin1"){

  # maybe do something with this later to leave in NA columns?
  na.rm <- TRUE

  # storage
  res <- list()
  res.i <- 1

  # file is Latin-1 encoded (degrees etc)
  f <- readLines(file, encoding=encoding)

  # gives the grid used
  grid <- sub("^Grid\\t(.*)","\\1",f[1])
  # grab teh datum
  datum <- sub("^Datum\\t(.*)","\\1",f[2])

  # line starting with "Header" are the starts of tracks
  # line 2 before gives the track label
  # line 2 after is the start of the track
  start.tracks <- grep("^Header",f)

  for(i in 1:length(start.tracks)){

    # what is the line number for this track
    track <- start.tracks[i]
    # and the next track
    n.track <- start.tracks[i+1]

    if((i+1) > length(start.tracks)){
      n.track <- length(f)
    }

    # build the index for this set of lines
    line.index <- c(track,(track+2):(n.track-4))


    # if this line index doesn't make sense (e.g. is not in order)
    # then it's not valid, do nothing
    if(all(sort(line.index) == line.index)){

      glob <- f[line.index]

      if(grepl("^Waypoint",glob[2])){
        warning(paste0("Ignoring waypoints!"))
        #next
      }else{

        this.file <-read.table(file,skip=line.index[1]-1,
                           nrows=line.index[length(line.index)]-line.index[1]-1,
                            fill=TRUE,sep="\t",header=TRUE,
                            fileEncoding=encoding, encoding=encoding)

        # now the file is loaded, start removing the crap
        this.file$Header <- NULL

        # Pull out lat and long
        ll.str <- as.character(this.file$Position)
        long <- sub(" [EW]\\d+ \\d+\\.\\d+$","",ll.str)
        lat <- sub("^[NS]\\d+ \\d+\\.\\d+ ","",ll.str)

        long <- process.ll(long)
        lat <- process.ll(lat)

        res[[res.i]] <- data.frame(Latitude  = lat,
                                   Longitude = long)

        # remove all the columns which are all NA
        if(na.rm){
          rm.cols<-c()
          for(j in 1:ncol(this.file)){
            if(all(is.na(this.file[,j]))){
              rm.cols<-c(rm.cols,j)
            }
          }
          for(j in rm.cols){
            if(all(is.na(this.file[,j]))){
              this.file[,j]<-NULL
            }
          }
        }

        ## Altitude
        alt <- as.character(this.file$Altitude)
        alt <- sub(" m$","",alt)
        alt <- as.numeric(alt)
        res[[res.i]] <- cbind(res[[res.i]],
                          Altitude=alt)

        ### Depth
        #depth <- as.character(this.file$Depth)
        #depth <- sub(" m$","",depth)
        #depth <- as.numeric(depth)
        #res[[res.i]] <- cbind(res[[res.i]],
        #                  Depth=depth)

        ## Leg.Length
        legl <- as.character(this.file$Leg.Length)
        legl <- sub(" m$","",legl)
        ks <- grepl(" km$",legl) # find where it was km and multiply
        legl <- sub(" km$","",legl)
        legl <- as.numeric(legl)
        legl[ks] <- legl[ks]*1000
        res[[res.i]] <- cbind(res[[res.i]],
                          Leg.Length=legl)

        ## Leg.Time
        legt <- suppressWarnings(hms(as.character(this.file$Leg.Time)))
        res[[res.i]] <- cbind(res[[res.i]],
                          Leg.Time=legt)

        ## Leg.Speed
        legs <- as.character(this.file$Leg.Speed)
        legs <- sub(" kph$","",legs)
        legs <- as.numeric(legs)
        res[[res.i]] <- cbind(res[[res.i]],
                          Leg.Speed=legs)

        ## Leg.Course
        legc <- as.character(this.file$Leg.Course)
        legc <- sub("(\\d+).* true$","\\1",legc)
        legc <- as.numeric(legc)
        legc[ks] <- legc[ks]*1000
        res[[res.i]] <- cbind(res[[res.i]],
                          Leg.Course=legc)


        res.i <- res.i + 1
      }

    }else{
      warning(paste0("Entry ",i," failed to be loaded, check file!"))
    }
  }

  attr(res,"grid") <- grid
  attr(res,"datum") <- datum

  return(res)
}



#
#par(mfrow=c(2,4))
#
#for(i in 1:length(a)){
#  plot(a[[i]][,c("Latitude","Longitude")],type="l")
#}


#library(ggplot2)
#library(ggmap)
#
##ak <- get_map(c(mean(tracks$long),mean(tracks$lat)),zoom=6,maptype="hybrid")
#ak <- get_map("Anchorage",zoom=8,maptype="hybrid")
#p <- ggmap(a[[)
#
#p <- p + geom_path(aes(x=long,y=lat),data=tracks)
#print(p)




