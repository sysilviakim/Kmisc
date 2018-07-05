context("Gender Inference")

test_that("Known names are successfully classified as male/female", {
  df <- data.frame(NameFirst = c("Madison", "Nimrodel", "Michael"),
                   stringsAsFactors = FALSE)
  output <- gender_mutate_df(df, "NameFirst", years = c(1920, 2010))
  expect_equal(output$gender[1], "female")
  expect_equal(is.na(output$gender[2]), TRUE)
  expect_equal(output$gender[3], "male")
})
