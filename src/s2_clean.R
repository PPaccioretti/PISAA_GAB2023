# Filtro para las nubes 
# * Function to mask clouds using the Sentinel-2 QA band
# * @param {ee.Image} image Sentinel-2 image
# * @return {ee.Image} cloud masked Sentinel-2 image
getQABits <- function(image, qa) {
  # Convert decimal (character) to decimal (little endian)
  qa <- sum(2^(which(rev(unlist(strsplit(as.character(qa), "")) == 1)) - 1))
  # Return a single band image of the extracted QA bits, giving the qa value.
  image$bitwiseAnd(qa)$lt(1)
}

s2_clean <- function(img) {
  # Select only band of interest, for instance, B1 to B8
  img_band_selected <- img$select(list("B[1-8]", "B11", "B12"))
  # quality band
  ndvi_qa <- img$select("QA60")
  # Select pixels to mask
  quality_mask <- getQABits(ndvi_qa, "110000000000")
  # Mask pixels with value zero.
  ee$Image(img_band_selected$updateMask(quality_mask)$
             divide(10000)$
             copyProperties(ndvi_qa))$
    set('system:time_start', ndvi_qa$get('system:time_start'))
  
}

