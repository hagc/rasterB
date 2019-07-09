#' Aggregate raster cells in blocks
#'
#' Aggregate a Raster* object to create a new RasterLayer or RasterBrick with a lower resolution
#' (larger cells). Similar to \code{\link[raster]{aggregate}}, but processing in blocks
#' with \code{\link[raster]{blockSize}}, \code{\link[raster]{writeStart}}, and
#' \code{\link[raster]{writeStop}}. Alternative to \code{\link[raster]{aggregate}} when
#' RAM memory is low.
#'
#'
#' \code{aggregateB} runs function \code{\link[raster]{aggregate}} of raster package but in
#' blocks. This is alternative to original \code{\link[raster]{aggregate}}, which was
#' returning and error while aggregating rasters from 20 m to 100 m.
#' The error was:
#'
#' Error in setValues(out, .Call("_raster_aggregate_fun", x, dims, as.integer(na.rm),  :
#'   std::bad_alloc
#'
#' @param x why inheritParams doesn't work??
#' @inheritParams raster::aggregate
#'
#' @return RasterLayer or RasterBrick
#' @export
#'
#' @examples
#' # same examples as in raster::aggregate
#' r <- raster()
#' r <- setValues(r, runif(ncell(r)))
#'
#' # a new aggregated raster, max of the values
#' ra <- aggregate(r, fact=10, fun=max)
#' ra2<- aggregateB(r, fact=10, fun=max)
#' cellStats(ra-ra2, function(x,...){max(abs(x))}) # maximum difference between the rasters
#'
aggregateB<-function(x, fact=2, fun=mean, filename='', ...){


  # split ESM tile in blocks
  bs      <-blockSize(x)                   # find automatic blocks
  bs$row  <-seq(1, nrow(x), fact)          # starting row of each block
  bs$nrows<-diff(c(bs$row,(nrow(x)+1)))    # number of rows of each block
  bs$row2 <-bs$row + bs$nrows - 1          # ending row of each block
  bs$n    <-length(bs$row)                 # number of blocks


  # start writing the output raster
  rout     <- x                              # get raster basic structure
  res(rout)<- res(rout)*fact                 # output resolution
  rout<- writeStart(rout, filename=filename
                    # options="COMPRESS=DEFLATE", datatype="INT2U", overwrite=overwrite
                    )

  # split output raster in blocks
  bs2  <- blockSize(rout)                          # find automatic blocks
  bs2$row<-seq(1, nrow(rout), by=nrow(rout)/bs$n)  # starting row of each block


  # loop the blocks defined above for aggregating
  for (j in 1:bs$n) {
    # print(paste(j, "out of", bs$n))

    # crop the ESM and IMD tiles using the block
    extb<-extent(x, r1=bs$row[j], r2=bs$row2[j], c1=1, c2=ncol(x))
    subx<-crop(x, extb)

    # aggregate pixels (proportion of 'ones')
    a<-aggregate(subx, fact, fun=fun)


    # write the block to the output raster (permillage)
    writeValues(rout, values(a), bs2$row[j])

  }
  rout <- writeStop(rout)           # stop writing
}
