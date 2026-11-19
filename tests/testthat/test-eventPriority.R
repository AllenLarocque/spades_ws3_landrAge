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

test_that("every adjustBurnedPixels event is scheduled after scfmSpread's burn (7.5)", {
  calls <- eventCalls("adjustBurnedPixels")
  expect_gte(length(calls), 2)  # the first one (init) and the yearly re-schedule
  for (cl in calls) {
    prio <- as.list(cl)$eventPriority
    expect_false(is.null(prio), info = deparse(cl))  # default priority 5 runs before the burn
    expect_gt(eval(prio), 7.5)
  }
})
