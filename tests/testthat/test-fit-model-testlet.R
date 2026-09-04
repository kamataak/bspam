# Tests specifically for fit.model.testlet() validation and interface.

make_small_sentence_data <- function() {
  tibble::tibble(
    id.student = c(1, 1, 2, 2),
    id.passage = c(1, 1, 1, 1),
    id.sentence = c(1, 2, 1, 2),
    numwords.sent = c(10, 12, 10, 12),
    wrc = c(9, 11, 8, 12),
    sec = c(5, 6, 5.5, 6.5)
  )
}


test_that("fit.model.testlet returns NA when required column names are missing", {
  out <- fit.model.testlet(
    data = make_small_sentence_data(),
    person.id = "id.student",
    sub.task.id = "",
    obs.counts = "wrc",
    time = "sec",
    task.id = "id.passage",
    max.counts = "numwords.sent"
  )

  expect_true(is.na(out))
})


test_that("fit.model.testlet returns NA when a passage is read by fewer than two students", {
  dat <- make_small_sentence_data() |>
    dplyr::filter(id.student == 1)

  out <- fit.model.testlet(
    data = dat,
    person.id = "id.student",
    sub.task.id = "id.sentence",
    obs.counts = "wrc",
    time = "sec",
    task.id = "id.passage",
    max.counts = "numwords.sent"
  )

  expect_true(is.na(out))
})
