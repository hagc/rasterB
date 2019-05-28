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
#' r3B <- overlayB(stack(r1, r2), fun=function(x,y){return(x+y)})
#' compareRaster(r3,r3B,values=TRUE, stopiffalse=FALSE)           # not exactly equal for some reason
#' cellStats(r3-r3B,max)                                          # minor difference
#'
#' # long version for multiplication
#' r4 <- overlay(r1, r2, fun=function(x,y){(x*y)} )
#' r4B <- overlayB(stack(r1, r2), cores=2, fun=function(x,y){(x*y)})
#' compareRaster(r4,r4B,values=TRUE, stopiffalse=FALSE)           # not exactly equal for some reason
#' cellStats(r4-r4B,max)                                          # minor difference
#'
#' #use the individual layers of a RasterStack to get a RasterLayer
#' s <- stack(r1, r2)
#' r5 <- overlay(s, fun=function(x,y) x*y)
#' r5B <- overlayB(s, cores=2, fun=function(x,y) x*y)
#' compareRaster(r5,r5B,values=TRUE, stopiffalse=FALSE)           # not exactly equal for some reason
#' cellStats(r5-r5B,max)                                          # minor difference
overlayB<-function(x, ..., fun, cores, filename=""){
  tictoc::tic()

  out <- raster(x)
  out <- writeStart(out, filename, ...)  # open wrinting session for the output raster
  bs  <- blockSize(out, n=nlayers(x))    # define blocks for writing

  if(missing(cores)){
    warning("Processing blocks sequentially; define argument 'cores' for parallel processing.", call. = FALSE)
    for (i in 1:bs$n) {
      extb<-extent(out)
      extb[4]<-yFromRow(out,bs$row[i])+yres(out)/2                 # ymax
      extb[3]<-yFromRow(out,(bs$row[i]+bs$nrows[i])-1)-yres(out)/2 # ymin

      tilex<-crop(x,extb)
      v <- overlay(tilex, fun=fun)
      writeValues(out, values(v), bs$row[i])
    }
  }else{
    # Register CoreCluster
    cl<- parallel::makeCluster(cores)
    doSNOW::registerDoSNOW(cl)

    # print out progress of foreach
    progress <- function(nfin, tag) {
      cat(sprintf('progress: %f; block: %d\n', nfin/bs$n, tag))
    }
    opts <- list(progress=progress)

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
  tictoc::toc()
  return(out)
}
