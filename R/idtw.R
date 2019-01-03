idtw <- function(Q, C, newObs, gcm, dm, dist_method = c("norm1", "norm2", "norm2_square"),
                 step_pattern = c("symmetric2", "symmetric1"),
                 diffM = NULL, ws = NULL, 
                 return_cm = FALSE,
                 return_diffM = FALSE,
                 return_wp = FALSE,
                 return_diffp = FALSE,
                 return_QC = FALSE){

   dist_method <- match.arg(dist_method)
   step_pattern <- match.arg(step_pattern)
   if(return_diffp) return_wp <- TRUE
   
   
   if(is.character(C)){
      return_QC <- FALSE
      if(C == "diffM_add"){
         cm_add <- abs(Q)
         n <- nrow(cm_add)#should be equal nrow(gcm)
         m <- ncol(gcm)
         o <- ncol(cm_add)
         
         if(return_diffM) diffM <- Q
         rm(list=c("Q"))
         
      } else if(C == "cm_add"){
         cm_add <- Q
         n <- nrow(cm_add)#should be equal nrow(gcm)
         m <- ncol(gcm)
         o <- ncol(cm_add)
         
         rm(list=c("Q"))
         return_diffp <- FALSE
         diffM <- NA
      } else{
         stop("C needs to be a vector, matrix or one of the two strings: 
              'diffM_add' for difference Matrix or
              'cm_add' for cost Matrix")
      }
      } else {
         if(is.vector(Q)){
            if(dist_method != "norm1"){
               dist_method <- "norm1"
               warning("dist_method is set to 'norm1' for the univariate case")
            }
            n <- length(Q)
            m <- ncol(gcm)
            o <- length(newObs)
         }else{
            if(ncol(Q) != ncol(C) | ncol(C) != ncol(newObs)){
               stop("newObs, C and Q are matrices and must have the same number of columns.")
            }
            return_diffp <- FALSE
            diffM <- NA
            n <- nrow(Q)
            m <- ncol(gcm)
            o <- nrow(newObs)
         }
      }
   
   
   #--- initial checking
   m2 <- o + m
   if(!is.null(ws)){
      if(abs(n-m2)>ws){
         stop("window size is too small, no warping path can be found")
      }
   }
   
   #--- preparation
   gcm <- cbind(gcm, matrix(NA, nrow = n, ncol = o))
   dm  <- cbind(dm, matrix(NA, nrow = n, ncol = o))
   if(!is.character(C)){
      ws_cpp <- ifelse(is.null(ws), yes = -1, no = ws)
      if(is.vector(Q)){
         diffM_add <- cpp_diffm(Q, newObs, ws = ws_cpp, nPrevObs = m)
         cm_add <- abs(diffM_add)
      }else{
         cm_add <- cpp_cm(Q, newObs, dist_method = dist_method, ws = ws_cpp, nPrevObs = m)
      }
   }
   
   #--- calculation in C++
   if(is.null(ws)){
      ret <- IGCM_cpp(gcmN = gcm, dmN = dm, cmN = cm_add, step_pattern = step_pattern)
   } else {
      ret <- IGCM_Sakoe_cpp(gcmN = gcm, dmN = dm, cmN = cm_add, ws = ws, step_pattern = step_pattern)
   }
   
   #--- get warping path and diff-path
   if(return_diffp | return_diffM) diffM <- cbind(diffM, diffM_add)
   if(return_wp){
      if(return_diffp){
         if( is.integer(diffM[2,2]) & is.integer(diffM_add[2,1]) ){
            tmp <- BACKTRACK2II_cpp(ret$dm, diffM)   
         }else{
            tmp <- BACKTRACK2IN_cpp(ret$dm, diffM)
         }
         ret <- c(ret, list(diffp = tmp$diffp))
      }else {
         tmp <- BACKTRACK_cpp(ret$dm)
      }
      ret <- c(ret, list(ii = rev(tmp$ii),
                         jj = rev(tmp$jj),
                         wp = rev(tmp$wp)))
   }
   
   
   #--- add dtw_distance
   ret <- c(list(distance = ret$gcm[n, m2]), ret)
   
   #Normalization
   if(step_pattern == "symmetric2"){
      ret <- c(ret, normalized_distance = 
                  ret$distance/(nrow(gcm) + ncol(gcm) + nrow(newObs)) )
   }else{
      ret <- c(ret, normalized_distance = NA )
   }
   
   if(return_cm) ret <- c(ret, list(cm = cm_add))
   if(return_diffM) ret <- c(ret, list(diffM = diffM))
   if(return_QC) ret <- c(ret, list(Q = Q, C = c(C, newObs)))
   class(ret) <- append(class(ret), "idtw")
   return(ret)
}





#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


idtw2vec <- function(Q, newObs, dist_method = c("norm1", "norm2", "norm2_square"),
                     step_pattern = c("symmetric2", "symmetric1"),
                     gcm_lc = NULL, gcm_lr = NULL, nC = NULL, ws = NULL){
   
   dist_method <- match.arg(dist_method)
   step_pattern <- match.arg(step_pattern)
   initial_dim_check(Q = Q, C = newObs)
   
   if(is.character(newObs)){
      if(newObs != "cm"){
         stop("If Q is the costmatrix, C needs to be equal 'cm'.")
      }
      return(idtw2vec_cm(cm = Q, step_pattern = step_pattern,
                         gcm_lc = gcm_lc, gcm_lr = gcm_lr, nC = nC, ws = ws))      
   }
   
   
   if(is.vector(Q)){
      if(dist_method != "norm1"){
         warning("dist_method is set to 'norm1' for the univariate case")
      }
      return(idtw2vec_univ(Q = Q, newObs = newObs, gcm_lc = gcm_lc,
                           gcm_lr = gcm_lr, nC = nC, ws = ws, 
                           step_pattern = step_pattern))
   }else{
      if(ncol(Q) == 1 & ncol(newObs) == 1){
         if(dist_method != "norm1"){
            warning("dist_method is set to 'norm1' for the univariate case")
         }
         return(idtw2vec_univ(Q = Q, newObs = newObs, gcm_lc = gcm_lc, 
                              gcm_lr = gcm_lr, nC = nC, ws = ws, 
                              step_pattern = step_pattern))
      }else{
         return(idtw2vec_multiv(Q = Q, newObs = newObs, dist_method = dist_method,
                                gcm_lc = gcm_lc, gcm_lr = gcm_lr, 
                                nC = nC, ws = ws, step_pattern = step_pattern))
      }
   }
}


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


idtw2vec_cm <- function(cm, step_pattern = c("symmetric2", "symmetric1"),
                        gcm_lc = NULL, gcm_lr = NULL, nC = NULL, ws = NULL){
   
   step_pattern <- match.arg(step_pattern)
   
   if(is.null(gcm_lc)){
      # initial case
      if(ncol(cm) <= 1){
         stop("ERROR: in the initial calculation the cost matrix 
              cm needs to have at least 2 columns")
         return(NA)
      }
      
      init_gcm_lc <- cumsum(cm[,1])
      if(is.null(ws)){
         ret <- cpp_dtw2vec_cm_inc(gcm_lc = init_gcm_lc, cm = cm[,-1, drop = FALSE],
                                   step_pattern = step_pattern)
      }else {
         # sakoe chiba warping window
         ret <- cpp_dtw2vec_cm_ws_inc(gcm_lc = init_gcm_lc, cm = cm[,-1, drop = FALSE],
                                      step_pattern = step_pattern, ws = ws, ny = 1)
      }
      ret$gcm_lr_new <- c(tail(init_gcm_lc, 1),  ret$gcm_lr_new)
      
   }else{
      # running case
      if(is.null(ws)){
         ret <- cpp_dtw2vec_cm_inc(gcm_lc = gcm_lc, cm = cm,
                                   step_pattern = step_pattern)
         
      }else if (!is.null(ws) & !is.null(nC)){
         # sakoe chiba warping window
         ret <- cpp_dtw2vec_cm_ws_inc(gcm_lc = gcm_lc, cm = cm,
                                      step_pattern = step_pattern, ws = ws, ny = nC)
         
      }else {
         stop("ERROR: if ws is not NULL, then nC must not be NULL")
         return(NA)
      }   
      
      
      if(!is.null(gcm_lr)){#append former parts of last row of gcm
         ret$gcm_lr_new <- c(gcm_lr,  ret$gcm_lr_new)   
      }
   }
   
   #Normalization
   ret <- c(ret, normalized_distance = NA )
   if(step_pattern == "symmetric2"){
      if(is.null(gcm_lc)){#initial case
         ret$normalized_distance <- ret$distance/(nrow(cm) + ncol(cm)) 
      }else if(!is.null(nC)){
         ret$normalized_distance <-  ret$distance/(nrow(cm) + nC + ncol(cm)) 
      }
   }
return(ret)
}


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


idtw2vec_univ <- function(Q, newObs, step_pattern = c("symmetric2", "symmetric1"),
                          gcm_lc = NULL, gcm_lr = NULL, nC = NULL, ws = NULL){
   
   step_pattern <- match.arg(step_pattern)
   initial_dim_check(Q = Q, C = newObs)
   
   #--- initial checking
   if(!is.null(ws)){
      if(is.null(gcm_lc)){
         if(abs(length(Q) - (length(newObs) + 0)) > ws){
            stop("window size is too small, no warping path can be found")
         }  
      }else{
         if(abs(length(Q) - (length(newObs) + nC)) > ws){
            stop("window size is too small, no warping path can be found")
         }
      }
   } 
   
   
   if(is.null(gcm_lc)){
      # initial case
      if(length(newObs) <= 1){
         stop("ERROR: in the initial calculation the time 
              series newObs needs to be at least 2 observations long")
         return(NA)
      }
      
      init_gcm_lc <- cumsum(abs(Q-newObs[1]))
      if(is.null(ws)){
         ret <- cpp_dtw2vec_inc(x=Q, newObs = newObs[-1], 
                                gcm_lc = init_gcm_lc, step_pattern = step_pattern)
         
      }else {
         # sakoe chiba warping window
         ret <- cpp_dtw2vec_inc_ws(x=Q, newObs = newObs[-1], 
                                   gcm_lc = init_gcm_lc, 
                                   ws = ws, ny = 1, step_pattern = step_pattern)
      }
      ret$gcm_lr_new <- c(tail(init_gcm_lc, 1),  ret$gcm_lr_new)
      
      
   }else{
      # running case
      if(is.null(ws)){
         ret <- cpp_dtw2vec_inc(x=Q, newObs = newObs, gcm_lc = gcm_lc , step_pattern = step_pattern)
         
      }else if (!is.null(ws) & !is.null(nC)){
         # sakoe chiba warping window
         ret <- cpp_dtw2vec_inc_ws(x=Q, newObs = newObs, gcm_lc = gcm_lc,
                                   ws=ws, ny = nC, step_pattern = step_pattern)
         
      }else {
         stop("ERROR: if ws is not NULL, then nC must not be NULL")
         return(NA)
      }   
      
      
      if(!is.null(gcm_lr)){#append former parts of last row of gcm
         ret$gcm_lr_new <- c(gcm_lr,  ret$gcm_lr_new)   
      }
   }
   
   #Normalization
   ret <- c(ret, normalized_distance = NA )
   if(step_pattern == "symmetric2"){
      if(is.null(gcm_lc)){#initial case
         ret$normalized_distance <- ret$distance/(length(Q) + length(newObs)) 
      }else if(!is.null(nC)){
         ret$normalized_distance <-  ret$distance/(length(Q) + nC + length(newObs)) 
      }
   }
   
   return(ret)
}


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


idtw2vec_multiv <- function(Q, newObs, dist_method = c("norm1", "norm2", "norm2_square"),
                            step_pattern = c("symmetric2", "symmetric1"),
                            gcm_lc = NULL, gcm_lr = NULL, nC = NULL, ws = NULL){
   
   step_pattern <- match.arg(step_pattern)
   dist_method <- match.arg(dist_method)
   initial_dim_check(Q = Q, C = newObs)
   
   if(!is.null(ws)){
      if(is.null(gcm_lc)){
         if(abs(nrow(Q) - (nrow(newObs) + 0)) > ws){
            stop("window size is too small, no warping path can be found")
         }  
      }else{
         if(abs(nrow(Q) - (nrow(newObs) + nC)) > ws){
            stop("window size is too small, no warping path can be found")
         }
      }
   }
   
   
   if(is.null(gcm_lc)){
      # initial case
      if(length(newObs) <= 1){
         stop("ERROR: in the initial calculation the time 
              series newObs needs to be at least 2 observations long")
         return(NA)
      }
      
      # TODO: also possible to solve with is.NUll in C++ code
      # https://stackoverflow.com/questions/34205925/default-null-parameter-rcpp/34206602
      
      if(is.null(ws)){
         init_gcm_lc <- cpp_cm(Q, newObs[1, , drop = FALSE], dist_method = dist_method, ws = -1, nPrevObs = 0)
      }else{
         init_gcm_lc <- cpp_cm(Q, newObs[1, , drop = FALSE], dist_method = dist_method, ws = ws, nPrevObs = 0)
      }
      init_gcm_lc <- cumsum(init_gcm_lc)   
      
      if(is.null(ws)){
         ret <- cpp_dtw2vec_inc_mv(x=Q, newObs = newObs[-1, , drop = FALSE], 
                                   gcm_lc = init_gcm_lc, dist_method = dist_method, step_pattern = step_pattern)
         
      }else {
         # sakoe chiba warping window
         ret <- cpp_dtw2vec_inc_mv_ws(x=Q, newObs = newObs[-1, , drop = FALSE], 
                                      gcm_lc = init_gcm_lc, dist_method = dist_method,  
                                      ws = ws, ny = 1, step_pattern = step_pattern)
      }
      ret$gcm_lr_new <- c(tail(init_gcm_lc, 1),  ret$gcm_lr_new)
      
      
   }else{
      # running case
      if(is.null(ws)){
         ret <- cpp_dtw2vec_inc_mv(x=Q, newObs = newObs, gcm_lc = gcm_lc,
                                   dist_method = dist_method, step_pattern = step_pattern)
         
      }else if (!is.null(ws) & !is.null(nC)){
         # sakoe chiba warping window
         ret <- cpp_dtw2vec_inc_mv_ws(x=Q, newObs = newObs, gcm_lc = gcm_lc,
                                      dist_method = dist_method, ws=ws, ny = nC, step_pattern = step_pattern)
         
      }else {
         stop("ERROR: if ws is not NULL, then nC must not be NULL")
         return(NA)
      }   
      
      
      if(!is.null(gcm_lr)){#append former parts of last row of gcm
         ret$gcm_lr_new <- c(gcm_lr,  ret$gcm_lr_new)   
      }
   }
   
   #Normalization
   ret <- c(ret, normalized_distance = NA )
   if(step_pattern == "symmetric2"){
      if(is.null(gcm_lc)){#initial case
         ret$normalized_distance <- ret$distance/(nrow(Q) + nrow(newObs)) 
      }else if(!is.null(nC)){
         ret$normalized_distance <-  ret$distance/(nrow(Q) + nC + nrow(newObs)) 
      }
   }
   
   return(ret)
}


