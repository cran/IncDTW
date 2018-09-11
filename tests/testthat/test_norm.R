context("normalizing time series")

test_that("test znorm", {
   #skip("blabla")
   eps <- 1/10^7
   x <- rnorm(100, sample(100,1), sample(10,1))
   
   f0 <- function(x){  (x-mean(x))/sd(x)  }
   f1 <- function(x, mu0, sd0){  (x-mu0)/sd0  }
   
   mm <- rnorm(1)
   ss <- rnorm(1)
   expect_equal(max(abs(f0(x) - cpp_znorm(x))) < eps, TRUE)
   expect_equal(max(abs(f1(x, mu0 = mm, sd0 = ss) - 
                           cpp_znorm(x, mu_in = mm, sd_in = ss))) < eps, TRUE)
   
   
})



test_that("test znorm speed", {
   skip("speed comparison")
   
   x <- rnorm(10^6, sample(100,1), sample(10,1))
   
   f0 <- function(x){  (x-mean(x))/sd(x)  }
   f1 <- function(x, mu0, sd0){  (x-mu0)/sd0  }
   
   mm <- rnorm(1)
   ss <- rnorm(1)
   microbenchmark::microbenchmark(f0(x),
                                  cpp_znorm(x),
                                  cpp_znorm(x, mm, ss))
   
})



test_that("test norm01", {
   #skip("blabla")
   eps <- 1/10^7
   x <- rnorm(100, sample(100,1), sample(10,1))
   f0 <- function(x){  (x-min(x))/(max(x)-min(x))  }
   f1 <- function(x, mi, ma){  (x-mi)/(ma - mi)  }
   
   mmi <- rnorm(1)
   mma <- rnorm(1)
   
   expect_equal(max(abs(f0(x) - cpp_norm01(x))) < eps, TRUE)
   expect_equal(max(abs(f1(x, mi = mmi, ma = mma) - 
                           cpp_norm01(x, min_in = mmi, max_in = mma))) < eps, TRUE)
   
   
})



test_that("test norm01 speed", {
   skip("speed comparison")
   
   x <- rnorm(10^4, sample(100,1), sample(10,1))
   
   f0 <- function(x){  (x-min(x))/(max(x)-min(x))  }
   f1 <- function(x, mi, ma){  (x-mi)/(ma - mi)  }
   
   mmi <- rnorm(1)
   mma <- rnorm(1)
   
   microbenchmark::microbenchmark(f0(x),
                                  cpp_norm01(x),
                                  cpp_norm01(x, min_in = mmi, max_in = mma))
   
})
