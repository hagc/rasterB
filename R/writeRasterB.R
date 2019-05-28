#' Write raster data to a file in blocks
#'
#' Write an entire Raster* object to a file in blocks by using \code{\link[raster]{writeRaster}},
#' \code{\link[raster]{blockSize}}, \code{\link[raster]{writeStart}}, and
#' \code{\link[raster]{writeStop}}. Alternative to \code{\link[raster]{writeRaster}} when
#' RAM memory is low.
#'
#' See \code{\link[raster]{writeRaster}}
#'
#' @inheritParams raster::writeRaster
#'
#' @return This function is used for the side-effect of writing values to a file.
#' @export
#'
#' @examples
#' # Using the examples of raster::writeRaster
#' r <- raster(system.file("external/test.grd", package="raster"))
#'
#' # take a small part
#' r <- crop(r, extent(179880, 180800, 329880, 330840) )
#'
#' # write to an integer binary file
#' rf <- writeRaster(r, filename="allint.grd", datatype='INT4S', overwrite=TRUE)
#' rfB <- writeRasterB(r, filename="allint.grd", datatype='INT4S', overwrite=TRUE)
#' compareRaster(rf, rfB, values=TRUE)
#'
#' # make a brick and save multi-layer file
#' b <- brick(r, sqrt(r))
#' bf <- writeRaster(b, filename="multi.grd", bandorder='BIL', overwrite=TRUE)
#' bfB <- writeRasterB(b, filename="multi.grd", bandorder='BIL', overwrite=TRUE)
#' compareRaster(bf[[1]], bfB[[1]], values=TRUE)
#' compareRaster(bf[[2]], bfB[[2]], values=TRUE)
#'
#' # write to a new geotiff file (depends on rgdal)
#' if (require(rgdal)) {
#'   rf <- writeRaster(r, filename="test.tif", format="GTiff", overwrite=TRUE)
#'   bf <- writeRaster(b, filename="multi.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
#'   rfB <- writeRasterB(r, filename="test.tif", format="GTiff", overwrite=TRUE)
#'   bfB <- writeRasterB(b, filename="multi.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
#' }
#' compareRaster(rf, rfB, values=TRUE)
#' compareRaster(bf[[1]], bfB[[1]], values=TRUE)
#' compareRaster(bf[[2]], bfB[[2]], values=TRUE)
#'
#' # write to netcdf
#' if (require(ncdf4)) {
#'   rnc <- writeRaster(r, filename='netCDF.nc', format="CDF", overwrite=TRUE)
#'   rncB <- writeRasterB(r, filename='netCDF.nc', format="CDF", overwrite=TRUE)
#' }
#' rnc
#' rncB
writeRasterB<-function(x, filename, ...){
  tictoc::tic()

  if(class(x)=="RasterLayer"){
    out <- raster(x)
  }else{
    out <- brick(x, values=FALSE)
  }

  out <- writeStart(out, filename, ...)  # open wrinting session for the output raster
  bs  <- blockSize(out, n=nlayers(x))    # define blocks for writing

  for (i in 1:bs$n) {
    v <- getValuesBlock(x, row=bs$row[i], nrows=bs$nrows[i])
    out <- writeValues(out, v, bs$row[i])
  }
  out <- writeStop(out)
  tictoc::toc()
  return(out)
}
