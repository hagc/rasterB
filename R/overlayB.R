#' Overlay Raster objects in blocks
#'
#' Create a new RasterLayer object, based on one multi-layer Raster* object.
#' Similar to \code{\link[raster]{overlay}}, but processing in blocks
#' with \code{\link[raster]{blockSize}}, \code{\link[raster]{writeStart}}, and
#' \code{\link[raster]{writeStop}}. Alternative to \code{\link[raster]{overlay}} when
#' RAM memory is low.
#'
#' \code{overlayB} is limited comparing to \code{\link[raster]{overlay}} as
#' the former only accepts one multi-layer Raster* object and outputs
#' a RasterLayer.
#'
#' @param cores Integer. Number of cores to process blocks in parallel
#' @param ... Additional arguments for writing files as in \code{\link{writeRasterB}}
#' @param fun Function to be applied. The function should match the number of layers of the RasterStack/Brick object.
#' @param verbose logical. Enable verbose execution?
#' @inheritParams raster::overlay
#'
#' @return RasterLayer object
#' @export
#'
#' @examples
#' r <- raster(ncol=10, nrow=10)
#' r1 <- init(r, fun=runif)
#' r2 <- init(r, fun=runif)
#' r3 <- overlay(r1, r2, fun=function(x,y){return(x+y)})
#' r3B <- overlayB(stack(r1, r2), fun=function(x,y){return(x+y)}, verbose=FALSE)
#' compareRaster(r3,r3B,values=TRUE, stopiffalse=FALSE)
#' cellStats(abs(r3-r3B),max) # minor difference
#'
#' # long version for multiplication
#' r4 <- overlay(r1, r2, fun=function(x,y){(x*y)} )
#' r4B <- overlayB(stack(r1, r2), fun=function(x,y){(x*y)}, verbose=FALSE)
#' compareRaster(r4,r4B,values=TRUE, stopiffalse=FALSE)
#' cellStats(abs(r4-r4B),max) # minor difference
#'
#' # use the individual layers of a RasterStack to get a RasterLayer
#' s <- stack(r1, r2)
#' r5 <- overlay(s, fun=function(x,y) x*y)
#' r5B <- overlayB(s, fun=function(x,y) x*y, verbose=FALSE)
#' compareRaster(r5,r5B,values=TRUE, stopiffalse=FALSE)
#' cellStats(r5-r5B,max) # minor difference
#'
#' overlayB(s, fun=function(x,y) x*y)
#' overlayB(s, fun=function(x,y) x*y, cores=2)
overlayB<-function(x, ..., fun, cores, filename="", verbose=TRUE){

  out <- raster(x)
  out <- writeStart(out, filename, ...)  # open wrinting session for the output raster
  bs  <- blockSize(out, n=nlayers(x))    # define blocks for writing
  n<-parallel::detectCores()

  if(missing(cores) | cores=1){
    if(verbose) message(n," cores available; using 1; use argument 'cores' for parallel processing.")
    for (i in 1:bs$n) {

      extb<-extent(out)
      extb[4]<-yFromRow(out,bs$row[i])+yres(out)/2                 # ymax
      extb[3]<-yFromRow(out,(bs$row[i]+bs$nrows[i])-1)-yres(out)/2 # ymin

      tilex<-crop(x,extb)
      v <- overlay(tilex, fun=fun)
      writeValues(out, values(v), bs$row[i])
      if(verbose) message(sprintf('progress: %f; block: %d', i/bs$n, i))
    }
  }else{

    ## Register CoreCluster
    if(cores>n){stop(sprintf("Argument 'cores' should be <=%i as only %i cores are available in the local machine.", n, n))}
    if(verbose) message(n," cores available; using ", cores)
    cl<- parallel::makeCluster(cores)
    doSNOW::registerDoSNOW(cl)

    ## print out progress of foreach
    if(verbose){
      progress <- function(nfin, tag) message(sprintf('progress: %f; block: %d', nfin/bs$n, tag))
      opts <- list(progress=progress)
    }else{
      opts<-NULL
    }

    ## overlay (parallel)
    agg<-foreach::foreach(i=1:bs$n,.packages="raster",.options.snow=opts) %dopar% {
      extb<-extent(out)
      extb[4]<-yFromRow(out,bs$row[i])+yres(out)/2                 # ymax
      extb[3]<-yFromRow(out,(bs$row[i]+bs$nrows[i])-1)-yres(out)/2 # ymin

      tilex<-crop(x,extb)
      overlay(tilex, fun=fun)
    }
    parallel::stopCluster(cl)
    for (i in 1:bs$n) writeValues(out, values(agg[[i]]), bs$row[i])
  }
  out <- writeStop(out)
}
