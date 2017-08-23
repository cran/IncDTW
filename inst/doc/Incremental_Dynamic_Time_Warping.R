## ---- echo = FALSE, message=FALSE----------------------------------------
library(IncDTW)
library(dtw)
if(require(microbenchmark)){}

## ------------------------------------------------------------------------
set.seed(1090)
x <- c(1,2,3,3,3,4,4,5)
h <- c(2.2:5.2) 
tmp <- IncDTW::dtw(Q = h, C = x, return_diffM = T, return_QC = TRUE)
names(tmp)

## ---- fig.show=TRUE------------------------------------------------------
plot(tmp, type = "QC")
plot(tmp, type = "warp")

## ------------------------------------------------------------------------
tmp$diffM

## ---- eval=FALSE---------------------------------------------------------
#  cm <- abs(diffM) # cost matrix
#  gcm <- cm # initialize the global cost matrix
#  for(j in 1:m)
#  for(i in 1:n)
#  gcm[ , 1] <- cm[ ,1]
#  gcm[1,  ] <- cm[1, ]
#  gcm[i, j] <- cm[i, j] + min(c(gcm[i-1, j-1],
#                                gcm[i  , j-1],
#                                gcm[i-1, j  ]))

## ---- eval=FALSE---------------------------------------------------------
#  dm <- matrix(NA, ncol=length(x), nrow = length(h))
#  dm[1,  ] <- 3
#  dm[ , 1] <- 2
#  dm[1, 1] <- NA
#  
#  for(j in 1:m)
#  for(i in 1:n)
#  min_index <- which.min(c(gcm[i-1, j-1],
#                           gcm[i  , j-1],
#                           gcm[i-1, j  ]))
#  if( min_index == 1){
#     dm[i, j] <- 1
#  } else if( min_index == 2){
#     dm[i, j] <- 2
#  } else if( min_index == 3){
#     dm[i, j] <- 3
#  }

## ---- eval=FALSE---------------------------------------------------------
#  IncDTW::dtw(Q = tmp$diffM, C = "diffM")
#  IncDTW::dtw(Q = abs(tmp$diffM), C = "cm")

## ---- message = FALSE----------------------------------------------------
set.seed(1030)
x0 <- cumsum(rnorm(1000))
h <- cumsum(rnorm(800))
tmp0 <- IncDTW::dtw(Q = h, C = x0)
gcm0 <- tmp0$gcm
dm0 <- tmp0$dm

## ------------------------------------------------------------------------
x_new <- cumsum(rnorm(10))
x_update <- c(x0, x_new) 

# result from incremental calculation
res_inc <- IncDTW::idtw(Q = h, C = x_update, newO = x_new, 
                    gcm = tmp0$gcm, dm = tmp0$dm)

## ------------------------------------------------------------------------
# result from scratch
res_scratch <- IncDTW::dtw(Q = h, C = x_update) 
sapply(names(res_inc), function(x){identical(res_inc[[x]], res_scratch[[x]])})

## ---- message=FALSE------------------------------------------------------
my_check <- function(values) {
  all(sapply(values[-1], function(x) identical(values[[1]], x)))
}

#--- define 'benchmark' functions from package: dtw
dtw_0 <- function(x,h){
   dtw::dtw(x , h, step.pattern = symmetric1)$distance }

dtw_sc <- function(x,h){
   dtw::dtw(x , h, step.pattern = symmetric1, 
            window.type = "sakoechiba", window.size = 40)$distance }

dtw_cm<- function(cm){
   dtw::dtw(x=cm, step.pattern = symmetric1)$distance }

#--- define functions to be tested from package: IncDTW
idtw_0 <- function(x,h){
   IncDTW::dtw(Q = h, C = x)$gcm[length(h), length(x)] }

idtw_sc <- function(x,h){
   IncDTW::dtw(Q = h, C = x, ws = 40)$gcm[length(h), length(x)] }

idtw_diff <- function(diffM){
   IncDTW::dtw(Q = diffM, C = "diffM")$gcm[nrow(diffM), ncol(diffM)] }

idtw_cm <- function(cm){
   IncDTW::dtw(Q = cm, C = "cm")$gcm[nrow(cm), ncol(cm)] }

idtw_inc <- function(x,h,gcm00, dm00){
   IncDTW::idtw(Q = h, C = x, newO = x[(length(x)-9) : length(x)],
                gcm = gcm00, dm = dm00)$gcm[length(h), length(x)] }


## ------------------------------------------------------------------------
tmp <- lapply(1:100, function(pseudoseed){
   set.seed(pseudoseed)
   x <- cumsum(rnorm(500))
   h <- cumsum(rnorm(480))
   tmp00 <- IncDTW::dtw(Q = h, C = x[1:(length(x)-10)], return_diffM = TRUE)
   gcm00 <- tmp00$gcm
   dm00 <- tmp00$dm
   tmp <- IncDTW::dtw(Q = h, C = x, return_diffM = TRUE)
   diffM <- tmp$diffM
   cm <- abs(diffM)
   
   mic <- microbenchmark( dtw_0(x,h),
                          dtw_cm(cm),
                          #----
                          idtw_0(x,h),
                          idtw_diff(diffM),
                          idtw_cm(cm),
                          idtw_inc(x,h,gcm00, dm00),
                          check = my_check,
                          times = 1)
                          
   return(as.data.frame(mic))
})
mics <- do.call(rbind, tmp)
class(mics) <- c("microbenchmark", "data.frame")

## ------------------------------------------------------------------------
print(mics, digits = 2, unit = "ms")

## ------------------------------------------------------------------------
tmp <- lapply(1:100, function(pseudoseed){
   set.seed(pseudoseed)
   x <- cumsum(rnorm(500))
   h <- cumsum(rnorm(480))
   tmp00 <- IncDTW::dtw(Q = h, C = x[1:(length(x)-10)], return_diffM = TRUE)
   gcm00 <- tmp00$gcm
   dm00 <- tmp00$dm
   tmp <- IncDTW::dtw(Q = h, C = x, return_diffM = TRUE)
   diffM <- tmp$diffM
   cm <- abs(diffM)
   
   mic <- microbenchmark( dtw_sc(x,h),
                          #----
                          idtw_sc(x,h),
                          check = my_check,
                          times = 1)

   return(as.data.frame(mic))
})
mics_sc <- do.call(rbind, tmp)
class(mics_sc) <- c("microbenchmark", "data.frame")

## ------------------------------------------------------------------------
print(mics_sc, digits = 2, unit = "ms")

## ---- message = FALSE----------------------------------------------------
set.seed(1150)
Q <- cos(1:100)
C <- cumsum(rnorm(80))
Ndec <- 4
# the ordinary calculation
result_base <- IncDTW::dtw(Q=Q, C=C) 
gcm0 <- result_base$gcm

## ------------------------------------------------------------------------
# the ordinary calculation without the last 4 observations
result_decr1 <- IncDTW::dtw(Q=Q, C=C[1:(length(C) - Ndec)])
gcm1 <- result_decr1$gcm

# the decremental step: reduce C for 4 observation
result_decr2 <- IncDTW::dec_dm(result_base$dm, Ndec = Ndec) 

# compare ii, jj and wp of result_decr and those of 
identical(result_decr1$ii, result_decr2$ii)
identical(result_decr1$jj, result_decr2$jj)
identical(result_decr1$wp, result_decr2$wp)
gcm1[nrow(gcm1), ncol(gcm1)] == gcm0[nrow(gcm0), ncol(gcm0) - Ndec]

