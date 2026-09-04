# Lightweight tests for plot.person() with scoring objects.

test_that("plot.person accepts a scoring object and returns a plotly object", {
  obj <- structure(
    list(
      person.id = c(1, 2),
      occasion = c(1, 1),
      group = c(3, 3),
      task.n = c(2, 2),
      max.counts.total = c(200, 200),
      obs.counts.obs = c(150, 160),
      secs.obs = c(100, 105),
      theta.map = c(0.1, 0.2),
      se.theta.map = c(0.01, 0.02),
      tau.map = c(0.3, 0.4),
      se.tau.map = c(0.03, 0.04)
    ),
    class = "scoring"
  )

  plt <- plot.person(obj, parameter = "theta", show.se = TRUE)
  expect_s3_class(plt, "plotly")
})


test_that("plot.person rejects invalid parameter names", {
  obj <- structure(list(person.id = 1, theta.map = 0.1), class = "scoring")

  expect_error(
    plot.person(obj, parameter = "invalid"),
    "parameter name"
  )
})
