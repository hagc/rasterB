#' Write raster data to a file in blocks
#'
#' Write an entire Raster* object to a file using \code{\link[raster]{writeRaster}},
#' \code{\link[raster]{blockSize}}, \code{\link[raster]{writeStart}}, and
#' \code{\link[raster]{writeStop}}.
#'
#' \code{writeRasterB} is an alternative to \code{\link[raster]{writeRaster}} when
#' RAM memory is low.
#'
#' @inheritParams raster::writeRaster
#'
#' @return This function is used for the side-effect of writing values to a file.
#' @export
#'
#' @examples
writeRasterB<-function(x, filename, ...){
  if(class(x)=="RasterLayer"){
    out <- raster(x, values=FALSE)
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
}
