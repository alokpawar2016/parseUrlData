library(testthat)
library(parseUrlData)

test_that("Content Type check", {
  url<-"http://echo.jsontest.com/fieldkey/fieldvalue/purpose/test"
  responce.data <-parseUrl(url)
  expect_match("application/json",getContentType(headers(responce.data)))
})


test_that("json url responce type check", {
  url<-"http://echo.jsontest.com/fieldkey/fieldvalue/purpose/test"
  responce.data <-getData(url)
  print(class(responce.data))
  expect_match("list", class(responce.data))
})

test_that("cacheing check", {
  test.object<- list(c(1:5))
  setCacheData("test.object",test.object)
  cacheobject<- getCacheData("test.object")
  print(cacheobject)
  expect_equal(test.object, cacheobject)
})
