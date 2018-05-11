## ---- echo = FALSE, message=FALSE----------------------------------------
library(IncDTW)
library(dtw)
benchmark_replications <- c(100,100)
# if(require(microbenchmark)){}

## ------------------------------------------------------------------------
set.seed(1090)
Q <- sin(1:10)#+rnorm(20)
C <- sin(-2:10)#+rnorm(15)
tmp <- IncDTW::dtw(Q = Q, C = C, return_diffM = TRUE, return_wp = TRUE, return_QC = TRUE, return_cm = TRUE, return_diffp = TRUE)
names(tmp)

## ---- fig.show=TRUE, results='hide', fig.align='center', fig.height=3.5, fig.width=7, echo=FALSE----
plot(x=tmp, type = "QC")

## ---- fig.show=TRUE, results='hide', fig.align='center', fig.height=7, fig.width=7, echo=FALSE----
plot(x=tmp, type = "warp")

## ------------------------------------------------------------------------
set.seed(1090)
Q <- c(1, 1, 2, 3, 2, 0)
C <- c(0, 1, 1, 2, 3, 2, 1)

tmp <- IncDTW::dtw(Q = Q, C = C, return_diffM = TRUE, return_wp = TRUE, return_QC = TRUE, return_cm = TRUE, return_diffp = TRUE)
names(tmp)

## ---- fig.show=TRUE, results='hide', fig.align='center', fig.height=3.5, fig.width=7, echo=FALSE----
plot(x=tmp, type = "QC")

## ---- fig.show=TRUE, results='hide', fig.align='center', fig.height=7, fig.width=7, echo=FALSE----
plot(x=tmp, type = "warp")

## ------------------------------------------------------------------------
tmp$diffM

## ---- eval=FALSE---------------------------------------------------------
#  cm <- abs(diffM) # cost matrix
#  gcm <- cm # initialize the global cost matrix
#  for(j in 1:m){
#     for(i in 1:n){
#        gcm[ , 1] <- cm[ ,1]
#        gcm[1,  ] <- cm[1, ]
#        gcm[i, j] <- cm[i, j] + min(c(gcm[i-1, j-1],
#                                      gcm[i  , j-1],
#                                      gcm[i-1, j  ]))}}

## ---- eval=FALSE---------------------------------------------------------
#  dm <- matrix(NA, ncol=length(C), nrow = length(Q))
#  dm[1,  ] <- 3
#  dm[ , 1] <- 2
#  dm[1, 1] <- NA
#  
#  for(j in 1:m){
#     for(i in 1:n){
#        min_index <- which.min(c(gcm[i-1, j-1],
#                                 gcm[i  , j-1],
#                                 gcm[i-1, j  ]))
#        if( min_index == 1){
#           dm[i, j] <- 1
#        } else if( min_index == 2){
#           dm[i, j] <- 2
#        } else if( min_index == 3){
#           dm[i, j] <- 3
#        } } }

## ---- eval=FALSE---------------------------------------------------------
#  IncDTW::dtw(Q = tmp$diffM, C = "diffM")
#  IncDTW::dtw(Q = abs(tmp$diffM), C = "cm")

## ---- message = FALSE----------------------------------------------------
set.seed(1030)
C0 <- cumsum(rnorm(1000))
Q <- cumsum(rnorm(800))
tmp0 <- IncDTW::dtw(Q = Q, C = C0)
gcm0 <- tmp0$gcm
dm0 <- tmp0$dm

## ------------------------------------------------------------------------
C_new <- cumsum(rnorm(10))
C_update <- c(C0, C_new) 

# result from incremental calculation
res_inc <- IncDTW::idtw(Q = Q, C = C_update, newO = C_new, 
                    gcm = tmp0$gcm, dm = tmp0$dm)

## ------------------------------------------------------------------------
# result from scratch
res_scratch <- IncDTW::dtw(Q = Q, C = C_update) 
sapply(names(res_inc), function(x){identical(res_inc[[x]], res_scratch[[x]])})

## ---- message=FALSE------------------------------------------------------
WS <- 40
my_check <- function(values) {
  all(sapply(values[-1], function(x) identical(values[[1]], x)))
}

#--- define 'benchmark' functions from package: dtw
dtw_0 <- function(C, Q){
   dtw::dtw(C, Q, step.pattern = symmetric1, distance.only = TRUE)$distance }

dtw_sc <- function(C, Q){
   dtw::dtw(C, Q, step.pattern = symmetric1, 
            window.type = "sakoechiba", distance.only = TRUE, window.size = WS)$distance }

dtw_cm<- function(cm){
   dtw::dtw(x=cm, step.pattern = symmetric1, distance.only = TRUE)$distance }

#--- define functions to be tested from package: IncDTW
idtw_dtw2vec <- function(C, Q){
   IncDTW::dtw2vec(Q = Q, C = C) }

idtw_dtw2vec_sc <- function(C, Q){
   IncDTW::dtw2vec(Q = Q, C = C, ws = WS) }

idtw_0 <- function(C, Q){
   IncDTW::dtw(Q = Q, C = C)$distance }

idtw_sc <- function(C, Q){
   IncDTW::dtw(Q = Q, C = C, ws = WS)$distance }

idtw_diff <- function(diffM){
   IncDTW::dtw(Q = diffM, C = "diffM")$distane }

idtw_cm <- function(cm){
   IncDTW::dtw(Q = cm, C = "cm")$distance }

idtw_inc <- function(C, Q, gcm00, dm00){
   IncDTW::idtw(Q = Q, C = C, newO = C[(length(C)-9) : length(C)],
                gcm = gcm00, dm = dm00)$distance }



## ------------------------------------------------------------------------
tmp <- lapply(1:20, function(pseudoseed){
   set.seed(pseudoseed)
   C <- cumsum(rnorm(500))
   Q <- cumsum(rnorm(480))
   tmp00 <- IncDTW::dtw(Q = Q, C = C[1:(length(C)-10)], return_diffM = TRUE)
   gcm00 <- tmp00$gcm
   dm00 <- tmp00$dm
   tmp <- IncDTW::dtw(Q = Q, C = C, return_diffM = TRUE)
   diffM <- tmp$diffM
   cm <- abs(diffM)
   
   
   mic <- rbenchmark::benchmark( dtw_0(C, Q),
                          dtw_cm(cm),
                          #----
                          idtw_dtw2vec(C, Q),
                          idtw_0(C, Q),
                          idtw_diff(diffM),
                          idtw_cm(cm),
                          idtw_inc(C, Q, gcm00, dm00),
                          replications = benchmark_replications[1]
                        )
                          
   return(as.data.frame(mic))
})
mics <- do.call(rbind, tmp)
# class(mics) <- c("microbenchmark", "data.frame")

## ------------------------------------------------------------------------
head(mics)
tmp <- aggregate(mics$elapsed, by = list(mics$test), mean)
tmp$xrel <- tmp$x/min(tmp$x)
tmp

## ------------------------------------------------------------------------
tmp <- lapply(1:20, function(pseudoseed){
   set.seed(pseudoseed)
   C <- cumsum(rnorm(500))
   Q <- cumsum(rnorm(480))
   tmp00 <- IncDTW::dtw(Q = Q, C = C[1:(length(C)-10)], return_diffM = TRUE)
   gcm00 <- tmp00$gcm
   dm00 <- tmp00$dm
   tmp <- IncDTW::dtw(Q = Q, C = C, return_diffM = TRUE)
   diffM <- tmp$diffM
   cm <- abs(diffM)
   
   mic <- rbenchmark::benchmark( dtw_sc(C, Q),
                          #----
                          idtw_dtw2vec_sc(C,Q),
                          idtw_sc(C, Q),
                          replications = benchmark_replications[1])

   return(as.data.frame(mic))
})
mics_sc <- do.call(rbind, tmp)

## ------------------------------------------------------------------------
head(mics_sc)
tmp <- aggregate(mics_sc$elapsed, by = list(mics_sc$test), mean)
tmp$xrel <- tmp$x/min(tmp$x)
tmp


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

# compare the results: ii, jj, wp and the gcm of 
# result_decr1 (conventional) and result_decr2 (recycling previous results)
comparison_0 <- c(
identical(result_decr1$ii, result_decr2$ii),
identical(result_decr1$jj, result_decr2$jj),
identical(result_decr1$wp, result_decr2$wp))
comparison_0

