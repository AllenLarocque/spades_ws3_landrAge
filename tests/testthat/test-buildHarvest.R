suppressPackageStartupMessages(library(terra))  # the module attaches terra (reqdPkgs)
## buildHarvest() is a plain function in the module file; pull it out without running defineModule()
local({
  exprs <- parse(test_path("..", "..", "spades_ws3_landrAge.R"), keep.source = FALSE)
  for (e in exprs) if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("buildHarvest")))
    eval(e, envir = globalenv())
})

writeMask <- function(path, v) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  r <- terra::rast(nrows = 1, ncols = 4, vals = v)
  terra::writeRaster(r, path, overwrite = TRUE)
}

test_that("buildHarvest reads projected_harvest when no applied_harvest exists (default)", {
  d <- withr::local_tempdir()
  writeMask(file.path(d, "tif", "tsaX", "projected_harvest_2025.tif"), c(1, 1, NA, NA))
  r <- buildHarvest(2025, "tsaX", "tif", d)
  expect_equal(terra::values(r)[, 1], c(1, 1, NA, NA))
})

test_that("buildHarvest prefers applied_harvest (realised harvest) when spades_ws3 wrote it", {
  d <- withr::local_tempdir()
  writeMask(file.path(d, "tif", "tsaX", "projected_harvest_2025.tif"), c(1, 1, NA, NA))
  writeMask(file.path(d, "tif", "tsaX", "applied_harvest_2025.tif"), c(1, NA, NA, NA))  # pixel 2 burned
  r <- buildHarvest(2025, "tsaX", "tif", d)
  expect_equal(terra::values(r)[, 1], c(1, NA, NA, NA))
})
