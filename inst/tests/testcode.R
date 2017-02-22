context("Check Summary Output")
test_that("The sum of fatalities is as expected", {
    expect_equal(sum(fars_summarize_years(2015)), 32244)
  })
