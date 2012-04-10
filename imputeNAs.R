#' Fill in NAs with nearest non-NAs
#' Fills in NAs in a vector with the nearest non-NA value. Similar to zoo::na.locf(), but uses the nearest, rather than the previous value.
#' Answer to the question proposed by geoffjentry at http://stackoverflow.com/questions/10077415/replacing-nas-in-r-with-nearest-value/10081444#10081444
#' @param dat the vector with (potentially) NAs to be replaced
#' @return the vector with the NAs having been replaced by the nearest non-NA value
#' @author Jeffrey D. Allen \email{jeff.allen@@trestletechnology.net}
imputeNAs <- function(dat){
  #get the index of all NA values
  nas <- which(is.na(dat))

  #get the Boolean map of which are NAs, used later to determine which values can be used as a replacement, and which are just filled-in NA values
  namask <- is.na(dat)

  #calculate the maximum size of a run of NAs
  length <- getLengthNAs(dat);

  #the furthest away an NA value could be is half of the length of the maximum NA run
  windowSize <- ceiling(length/2)

  #loop through all NAs
  for (thisIndex in nas){
    #extract the neighborhood of this NA
    neighborhood <- dat[(thisIndex-windowSize):(thisIndex+windowSize)]
    #any already-filled-in values which were NA can be replaced with NAs
    neighborhood[namask[(thisIndex-windowSize):(thisIndex+windowSize)]] <- NA

    #the center of this neighborhood
    center <- windowSize + 1

    #compute the difference within this neighborhood to find the nearest non-NA value
    delta <- center - which(!is.na(neighborhood))

    #find the closest replacement
    replacement <- delta[abs(delta) == min(abs(delta))]
    #in case length > 1, just pick the first
    replacement <- replacement[1]

    #replace with the nearest non-NA value.
    dat[thisIndex] <- dat[(thisIndex - (replacement))]
  }
  dat
}

