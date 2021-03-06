library(readr)
library(testthat)

#data.frame2013 <- readr::read_csv("C:/Users/james/Desktop/PeerReviewProjectWeek4/accident_2013.csv.bz2")
#data.frame2014 <- readr::read_csv("C:/Users/james/Desktop/PeerReviewProjectWeek4/accident_2014.csv.bz2")
#data.frame2015 <- readr::read_csv("C:/Users/james/Desktop/PeerReviewProjectWeek4/accident_2015.csv.bz2")

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

testthat::expect_that(make_filename(2013), is_identical_to("accident_2013.csv.bz2"))
# testthat::expect_that(fars_read("C:/Users/james/Desktop/PeerReviewProjectWeek4/accident_2014.csv.bz2"), is_a("data.frame"))
