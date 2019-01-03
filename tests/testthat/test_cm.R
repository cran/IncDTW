context("Equal dtw package")
library(dtw)


test_that("Bub: dtw normalized mv", {
   Q <- matrix(rnorm(100), ncol=2)
   C <- matrix(rnorm(800), ncol=2)
   
   d_norm1 <- function(x,y){
      sum(abs(x-y))
   }
   cm0 <- IncDTW::cm(Q, C, dist_method = "norm1")
   cm1 <- IncDTW::cm(Q, C, dist_method = d_norm1)
   expect_equal(cm1, cm0)
   
   
   d_norm2 <- function(x,y){
      sqrt(sum((x-y)^2))
   }
   cm0 <- IncDTW::cm(Q, C, dist_method = "norm2")
   cm1 <- IncDTW::cm(Q, C, dist_method = d_norm2)
   expect_equal(cm1, cm0)
   
   
   d_norm22 <- function(x,y){
      sum((x-y)^2)
   }
   cm0 <- IncDTW::cm(Q, C, dist_method = "norm2_square")
   cm1 <- IncDTW::cm(Q, C, dist_method = d_norm22)
   expect_equal(cm1, cm0)
   
})




