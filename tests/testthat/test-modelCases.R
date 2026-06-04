test_that("multiplication works", {
  expect_s3_class(modelCases(), "gt_tbl")
  expect_s3_class(modelCases(interaction = F), "gt_tbl")
  expect_type(modelCases(), "list")
  expect_type(modelCases(interaction = F), "list")
  expect_error(modelCases(interaction = butt))
  expect_error(modelCases(interaction = "butt"))
})
