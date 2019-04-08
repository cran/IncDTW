

plot.idtw <- function(x, type = c("QC", "warp"), partial = NULL, selDim = 1, ...) {
   
   type <- match.arg(type)
   pm <- pmatch(type, c("QC", "warp"))
   
   switch(pm,
          plotQC(x, partial = partial, selDim = selDim, ...),
          plotWarp(x, partial = partial, selDim = selDim, ...)
   )
}

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


plot.dba <- function(x, type = c("barycenter", "m2m", "m2lot"), ...) {
   
   type <- match.arg(type)
   pm <- pmatch(type, c("barycenter", "m2m", "m2lot"))
   
   switch(pm,
          plotBary(x, ...),
          plotM2m(x, ...),
          plotM2lot(x, ...)
   )
}


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


## an alias
plot_idtw <- plot.idtw;
plot_dba <- plot.dba;


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

plotM2m <- function(x, ...){
   dfp <- data.frame(Iterations = 1:x$input$iterMax, y = x$iterDist_m2m)
   gg  <- ggplot(dfp, ...) + 
      geom_line(aes_string(x = 'Iterations', y = 'y')) + 
      geom_point(aes_string(x = 'Iterations', y = 'y')) +
      ylab("Distance of iterations")
   return(gg)
}


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


plotM2lot <- function(x, ...){
   if(x$input$step_pattern == "symmetric1"){
      dfp <- data.frame(iter = 1:x$input$iterMax,
                        meanDist = sapply(x$iterDist_m2lot, mean),
                        sdDist   = sapply(x$iterDist_m2lot, sd))
   }else{
      dfp <- data.frame(iter = 1:x$input$iterMax,
                        meanDist = sapply(x$iterDist_m2lot_norm, mean),
                        sdDist   = sapply(x$iterDist_m2lot_norm, sd))
   }
   
   dfp$ymin <- dfp$meanDist - dfp$sdDist
   dfp$ymax <- dfp$meanDist + dfp$sdDist
   
   gg  <- ggplot(dfp, ...) + geom_line(aes_string(x = 'iter', y = 'meanDist')) + 
      geom_point(aes_string(x = 'iter', y = 'meanDist')) +
      geom_ribbon(aes_string(x = 'iter', ymin = 'ymin', ymax = 'ymax'), alpha = 0.2) +
      xlab("Iterations")
   
   if(x$input$step_pattern == "symmetric1"){
      gg <- gg + ylab("Average distance of iterations\nto list of time series")
   }else{
      gg <- gg + ylab("Average normalized distance of iterations\nto list of time series")
   }
   return(gg)
}


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


plotBary <- function(x, ...){
   dfp <- x$iterations
   dfp <- lapply(seq_along(dfp), function(i){
      cbind(dfp[[i]], i = i, j = 1:nrow(dfp[[i]]))
   })
   dfp <- melt(as.data.table(do.call(rbind, dfp)), id.vars = c("i", "j"))
   
   gg <- ggplot(dfp, ...) + 
      geom_line(aes_string(x = 'j', y = 'value', group = 'variable', col = 'i')) +
      facet_grid( ~variable)
   return(gg)
   
   # to adjust existing ggplot object
   # q <- ggplot_build(gg)
   # q$data[[1]]$size <- 1
   # gg4 <- ggplot_gtable(q)
   # plot(gg4)
}





#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


plotQC <- function(x, Q = NULL, C = NULL, partial = NULL, selDim = 1, ...){
   # Q <- sin(1:10)#+rnorm(20)
   # # C <- sin(1:30)+rnorm(30)
   # C <- sin(-2:10)#+rnorm(15)
   # # C <- c(Q, cumsum(rnorm(20))) + rnorm(120, 0, 0.5)
   # x <- dtw(Q = Q, C = C,  return_diffM = FALSE, return_QC = T)
   
   if(is.null(Q) & !is.null(x$Q)) Q <- x$Q
   if(is.null(C) & !is.null(x$C)) C <- x$C
   if(is.null(Q) | is.null(C)){
      stop("Q and C either need to be returned from dtw() or idtw(), or defined manually")
   }
   if(!is.null(partial)){
      # adjust to partial matching
      new_wp <- BACKTRACK_cpp(x$dm[  partial$rangeQ[1]:partial$rangeQ[2],
                                     partial$rangeC[1]:partial$rangeC[2]  ])
      x$ii <- rev(new_wp$ii)
      x$jj <- rev(new_wp$jj)
   } 
   
   if(is.matrix(Q)) Q <- Q[, selDim]
   if(is.matrix(C)) C <- C[, selDim]
   
   # this makes the two time series to be plotted one over the other
   # Q <- Q-max(Q)
   # C <- C-min(C)
   
   tmp1 <- data.frame(val = c(Q, C),
                      x = c(1:length(Q), 1:length(C)),
                      id = c(rep("Q", length(Q)),
                             rep("C", length(C))
                      )
   )
   
   tmp2 <- data.frame(x = c(x$ii, x$jj), 
                      y = c(Q[x$ii], C[x$jj]),
                      id = rep(1:length(x$ii), 2))
   
   gg1 <- ggplot(tmp1, ...) +
      geom_line(aes_string(x = 'x', y = 'val', group = 'id', col = 'id')) + 
      xlab("Index") + ylab("Value") +
      geom_point(aes_string(x = 'x', y = 'val', col = 'id')) +
      geom_line(data = tmp2, aes_string(x = 'x', y = 'y', group = 'id'), lty = 2) +
      guides(col = guide_legend(title = NULL))
   ret <- gg1
   
   return(ret)
}

if(FALSE){
   Q <- sin(1:20)
   C <- cos(1:50)+1
   tmp <- IncDTW::dtw(Q = Q, C = C, return_QC = TRUE)
   par <- IncDTW::dtw_partial(tmp, partial_Q = FALSE, partial_C = TRUE)
   plot(tmp, partial = par, type="QC")
   plot(tmp, partial = par, type="warp")
}


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


plotWarp <- function(x, Q = NULL, C = NULL, partial = NULL, selDim = 1, ...){
   if(is.null(Q) & !is.null(x$Q)) Q <- x$Q
   if(is.null(C) & !is.null(x$C)) C <- x$C
   if(is.null(Q) | is.null(C)){
      stop("Q and C either need to be returned from dtw() or idtw(), or defined manually")
   }
   if(!is.null(partial)){
      # adjust to partial matching
      new_wp <- BACKTRACK_cpp(x$dm[  partial$rangeQ[1]:partial$rangeQ[2],
                                     partial$rangeC[1]:partial$rangeC[2]  ])
      x$ii <- rev(new_wp$ii)
      x$jj <- rev(new_wp$jj)
   } 
   
   if(is.matrix(Q)) Q <- Q[, selDim]
   if(is.matrix(C)) C <- C[, selDim]
   
   
   tmp3 <- data.frame(ii=x$ii, jj = x$jj)
   tmp1 <- data.frame(val = c(Q, C),
                      x = c(1:length(Q), 1:length(C)),
                      id = c(rep("Q", length(Q)),
                             rep("C", length(C))
                      )
   )
   text_yQ <- mean(Q)
   text_yC <- mean(C)
   text_xwp <- length(Q)
   text_ywp <- length(C)
   
   gg_wp <- ggplot(tmp3, ...) +
      geom_line(aes_string(x='ii', y='jj')) +
      geom_point(aes_string(x = 'ii', y = 'jj')) + 
      scale_x_continuous(position = "top", name = "", 
                         breaks = scales::pretty_breaks(n=5),
                         minor_breaks = NULL) +
      scale_y_continuous(trans = "reverse", name = "", 
                         breaks = scales::pretty_breaks(n=5),
                         minor_breaks = NULL) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank())+
      geom_text(label="Warping Path: ii", aes(y=1, x = text_xwp), hjust = 1, vjust = 1) +
      geom_text(label="Warping Path: jj", aes(x=1, y = text_ywp), hjust = 0, vjust = 1, angle = 90)
   
   gg_Q <- ggplot(tmp1[tmp1$id == "Q", ]) + 
      geom_line(aes_string(x = 'x', y = 'val')) + ylab("")+
      geom_point(aes_string(x = 'x', y = 'val')) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      theme(panel.grid.major.y =  element_line(colour = "#eaeaea"))+
      theme(panel.grid.minor.y =  element_line(colour = "#eaeaea"))+
      scale_x_continuous( breaks = scales::pretty_breaks(n=5),
                          minor_breaks = NULL) +
      geom_text(label="Q", aes(x=1, y=text_yQ))
   
   gg_C <- ggplot(tmp1[tmp1$id == "C", ]) + 
      geom_line(aes_string(x = 'x', y = 'val')) + 
      geom_point(aes_string(x = 'x', y = 'val')) 
   gg_C <- gg_C + coord_flip() + 
      scale_x_continuous(position = "top", trans = "reverse", name="", 
                         breaks = scales::pretty_breaks(n=5), 
                         minor_breaks = NULL) +
      theme(panel.grid.major.x =  element_line(colour = "#eaeaea"))+
      theme(panel.grid.minor.x =  element_line(colour = "#eaeaea"))+
      geom_text(label="C", aes(x=1, y=text_yC))
   gg_C <- gg_C + theme(axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank())
   
   gg0 <- ggplot()+ geom_blank()+theme_minimal()
   gg2 <- gridExtra::grid.arrange(gg_wp, gg_C, gg_Q, gg0,
                                  layout_matrix = rbind(c(1,1,1,2), 
                                                        c(1,1,1,2),
                                                        c(1,1,1,2),
                                                        c(3,3,3,4)))
   ret <- gg2
   return(ret)
}
