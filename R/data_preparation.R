if(FALSE){# set to FALSE to prevent execution during devtools::build(".")
   
   
prepare_data <- function(activity, N = 0){
   # activity ... string, one of "Walk", "drink_glass", "brush_teeth"
   # N ... integer >=0, length of snippets. For example select:
   #        N <- 32 * 5# = 5 seconds at 32 Hz for WALK
   #        N <- 32 * 2# = 5 seconds at 32 Hz for brush_teeth
   require(data.table)
   # https://archive.ics.uci.edu/ml/datasets/Dataset+for+ADL+Recognition+with+Wrist-worn+Accelerometer
   fpath <- paste0("C:/Users/LeodolterM/Data/ADL_Dataset/HMP_Dataset/",activity,"/")
   lf <- list.files(path = fpath, full.names = T)
   d0 <- lapply(lf, fread)
   
   lot <- lapply(d0, function(x){
      tmp <- as.matrix(x)
      colnames(tmp) <- c("x", "y", "z")
      tmp
   })
   
   if(N > 0){
      
      lot <- lapply(lot, function(x){
         lapply(seq(1, nrow(x)-N, by = N), function(i){
            x[i:(i+N-1), ]
         })
      })
      lot <- do.call(c, lot)
   }  
   
   
   
   lot <- lapply(lot, function(ll){norm(ll, type="z")})
   return(lot)
}


# prepare data sets for package inclusion
Walk <- prepare_data("Walk", N = 0)
brush_teeth <- prepare_data("brush_teeth", N = 0)
drink_glass <- prepare_data("drink_glass", N = 0)

devtools::use_data(Walk, Walk, overwrite = TRUE)
devtools::use_data(brush_teeth, brush_teeth, overwrite = TRUE)
devtools::use_data(drink_glass, drink_glass, overwrite = TRUE)
   
}
