dtw <- function(Q, C, ws = NULL, return_diffM = FALSE){
   # require(GCM)
   # wrapper function for the C++ implementation of
   # an calculation of the global cost matrix + direction matrix
   # for dynamic time warping
   #
   # Q ... one dimensional vector, query time series,
   #           or matrix of differences/ costs (diffM, cm)
   # C ... one dimensional vector, time series to be fitted to C, 
   #           and for which values are constantly observed.
   #           if Q is not a vector C needs to be one 
   #           of the following strings ('diffM', 'cm')
  
   if(is.character(C)){
      if(C == "diffM"){
         cm <- abs(Q)
         n <- nrow(cm)
         m <- ncol(cm)
         diffM <- Q
         rm(list=c("Q"))
         
      } else if(C == "cm"){
         cm <- Q
         n <- nrow(cm)
         m <- ncol(cm)
         diffM <- NA
      } else{
         stop("C needs to be a vector or one of the two strings: 'diffM' for difference Matrix or 'cm' for cost Matrix")
      }
   } else {
      n <- length(Q)
      m <- length(C)
   }
  
   #--- initial checking
   if(!is.null(ws)){
      if(abs(n-m)>ws){
         stop("window size is too small, no warping path can be found")
      }
   }

   #--- preparation
   if(!is.character(C)){
      mQ <- matrix(Q, ncol = m, nrow = n, byrow = F)
      mC <- matrix(C, ncol = m, nrow = n, byrow = T)
      diffM <- mQ - mC
      cm <- abs(diffM)
      rm(list = c("mQ", "mC"))
   }
   
   #--- calculation in C++
   if(is.null(ws)){
      ret <- GCM_cpp(cm)
   } else {
      ret <- GCM_Sakoe_cpp(cm, ws)
   }
   tmp <- BACKTRACK_cpp(ret$dm)
   ret <- c(ret, list(ii = rev(tmp$ii),
                      jj = rev(tmp$jj),
                      wp = rev(tmp$wp)))

   if(return_diffM) ret <- c(ret, list(diffM = diffM))
   return(ret)
}


idtw <- function(Q, C, newO, gcm, dm, ws = NULL, return_diffM = FALSE){
   # require(GCM)
   # wrapper function for the C++ implementation of
   # an incremental calculation of the global cost matrix + direction matrix
   # for dynamic time warping
   #
   # Q ... one dimensional vector, query time series
   # C ... one dimensional vector, time series to be fitted to C, and for which values are constantly observed
   # newO ... one dimensional vector, new observation, to be appended to C
   # gcm ... global cost matrix, output from dtw(Q,C)
   # dm ... direction matrix, output from dtw(Q,C)

   #--- initial checking
   n <- length(Q)
   m <- length(C)
   o <- length(newO)
   m2 <- o+m
   if(!is.null(ws)){
      if(abs(n-m2)>ws){
         stop("window size is too small, no warping path can be found")
      }
   }

   #--- preparation
   C <- c(C, newO)
   gcm <- cbind(gcm, matrix(NA, nrow = n, ncol = o))
   dm  <- cbind(dm, matrix(NA, nrow = n, ncol = o))
   mQ <- matrix(Q, ncol = o, nrow = n, byrow = F)
   mC <- matrix(newO, ncol = o, nrow = n, byrow = T)
   diffM <- matrix((mQ - mC), ncol = length(newO))
   cm <- abs(diffM)

   #--- calculation in C++
   if(is.null(ws)){
      ret <- IGCM_cpp(gcmN = gcm, dmN = dm, cmN = cm)
   } else {
      ret <- IGCM_Sakoe_cpp(gcmN = gcm, dmN = dm, cmN = cm, ws = ws)
   }
   tmp <- BACKTRACK_cpp(ret$dm)
   ret <- c(ret, list(ii = rev(tmp$ii),
                      jj = rev(tmp$jj),
                      wp = rev(tmp$wp)))

   if(return_diffM) ret <- c(ret, list(diffM = diffM))
   return(ret)
}


dec_dm <- function(dm, Ndec){
  # wrapper function for the C++ implementation of
  # an decremental calculation of the warping path subject to a given
  # direction matrix for dynamic time warping
  # 
  # dm ... direction matrix, output from dtw(Q,C)
  # Ndec ... integer, number of observations (columns) to be reduced
  
  Nnew <- ncol(dm) - Ndec
  if(Nnew == 1){
     warning("Nnew = 1, calculation is maybe meaningless")
     ret <- list(ii = nrow(dm):1,
                 jj = rep(1, nrow(dm)),
                 wp = rep(3, nrow(dm)-1) )
  } else {
     tmp <- BACKTRACK_cpp(dm[, 1:Nnew])
     ret <- list(ii = rev(tmp$ii),
                 jj = rev(tmp$jj),
                 wp = rev(tmp$wp))
  }
  return(ret)
}
