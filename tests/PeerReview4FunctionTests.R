data.frame2013 <- read_csv("C:/Users/james/Desktop/PeerReviewProjectWeek4/accident_2013.csv.bz2")
data.frame2014 <- read_csv("C:/Users/james/Desktop/PeerReviewProjectWeek4/accident_2014.csv.bz2")
data.frame2015 <- read_csv("C:/Users/james/Desktop/PeerReviewProjectWeek4/accident_2015.csv.bz2")


expect_that(make_filename(2013), is_identical_to("accident_2013.csv.bz2"))
expect_that(fars_read("C:/Users/james/Desktop/PeerReviewProjectWeek4/accident_2014.csv.bz2"), is_a("data.frame"))
