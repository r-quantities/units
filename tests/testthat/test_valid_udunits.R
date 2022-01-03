test_that("udunits database can be read", {
  skip_if_not_installed("xml2")
  suppressMessages(expect_s3_class(valid_udunits(), "data.frame"))
  suppressMessages(expect_s3_class(valid_udunits_prefixes(), "data.frame"))
})
