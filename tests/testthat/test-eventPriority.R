## Event order: adjustBurnedPixels must run after the year's fire, every year, so that the
## burned pixels reset in sim$landscape$age are this year's (scfmSpread "burn" is at 7.5).

eventCalls <- function(type) {
  src <- paste(readLines(test_path("..", "..", "spades_ws3_landrAge.R")), collapse = "\n")
  exprs <- parse(text = src)
  found <- list()
  walk <- function(e) {
    if (is.call(e)) {
      if (identical(e[[1]], as.name("scheduleEvent")) && any(vapply(as.list(e), identical, logical(1), type))) {
        found[[length(found) + 1]] <<- e
      }
      for (a in as.list(e)[-1]) if (!missing(a)) walk(a)
    }
  }
  for (e in exprs) walk(e)
  found
}

test_that("adjustBurnedPixels runs after the fire and its readers, at the same priority every year", {
  # Order each year: scfmSpread burn (7.5) -> modules that read pre-fire ages, e.g.
  # ws3Verify_FRESH collectStats (8) -> adjustBurnedPixels resets the burned ages.
  calls <- eventCalls("adjustBurnedPixels")
  expect_gte(length(calls), 2)  # the first one (init) and the yearly re-schedule
  prios <- vapply(calls, function(cl) {
    p <- as.list(cl)$eventPriority
    if (is.null(p)) 5 else eval(p)     # no eventPriority = SpaDES default 5, before the burn
  }, numeric(1))
  expect_length(unique(prios), 1)
  expect_true(all(prios > 8), info = paste(prios, collapse = ", "))
})
