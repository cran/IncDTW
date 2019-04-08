#  --------------------------------------------------------------------------  
#  --------------------------------------------------------------------------
#
# TODO: before submission to JSS:
#                             - reproduction material:
#                                   zip the following:
#                                      + C:\Users\LeodolterM\Documents\SVN_AIT\trunk\JSS_IncDTW_Submission\non_svn
#                                      + IncDTW tar ball
#                             - reproduction script
#                             - version the submission version on overleaf.com
#                             - submit R package to CRAN         
#                             
#
# TODO: before submission to CRAN: 
#                             - adjust version number in Description File
#                             - change options in RStuio -> Tools -> Project Options -> Build Tools -> 
#                                  see the following link https://github.com/r-lib/devtools/issues/1667 
#                                  I added this option to omit vignette checking
#                                  
#
# TODO: test "inline" for help functions, especially <<mymin>>
# 
# TODO: adjust Version number in Description File
#
# TODO: upcoming features: - plot function for reverse (start-) partial alignment
#                          - TODO: add a rev.matrix:
#                                            rev.matrix <- function(x){ x[nrow(x):1, ,drop=FALSE] }
#                          - TODO: parallelized implementation of DBA
#                          - TODO: k-NN search for searching the k-NN in a set of time series of equal lengths 
#                                  by the same accelerating methods as applied for rundtw()
#                          - ...
#                          - ...
#                          - ...
#  
#  
#  --------------------------------------------------------------------------  
#  --------------------------------------------------------------------------
#  CHANGES SINCE VERSION 1.0.5
#     
#     - DONE TODO: change name of Vignette, the name visible online
#                                  
#     - DONE IDEA: new function: rundtw(), 
#                    inspird by Glocal cost matrix, compare with the paper 
#                   "Stream Monitoring under the Time Warping Distance"
#                    do a similar approach with running min-max normalization
#            
#     - DONE (in form of rundtw) TODO: maybe this makes no sense here in this package, but could be interesting
#              Welford's online algorithm for z-normalization
#              https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance 
#              
#     - DONE TODO: add labels to result of dtw_dismat() and dtw_disvec()
#                  see test_dtw_dismat.R for code to be included
#
#     - DONE new function: find_peaks()
#
#     - DONE new feature: for simulate_timewarp(), the parameter preserve_length
#     
#     - DONE ... vectorbased (also incremental) implementation for existing cost matrix
#     
#     - DONE as part of runDTW ... incremental DTW for stream data with running normalization 
#     
#     - DONE ... norm01 from caterpillar package without discretization
#     
#     - DONE ... plot functions for DBA for multivariate time series
#  
#  
#  
#  #  --------------------------------------------------------------------------  
#  --------------------------------------------------------------------------
#  CHANGES SINCE VERSION 1.0.4
#     
#     - bug fix: normalized dtw in dtw() for multivar time series 
#     
#     - simulate_timewarp(): new function
#     
#     - dtw2vec_cm() and idtw2vec_cm() : new functions included in dtw2vec() and idtw2vec()
#  
#  
#  
#  
#  
#  --------------------------------------------------------------------------  
#  --------------------------------------------------------------------------
#  
#  
#  
#  
# solved TO-DO: https://stackoverflow.com/questions/45618375/rcppparallel-parallelizing-distance-computation-segfault
# 
# solved TO-DO: have alook at dtwclust: 
#           https://cran.r-project.org/web/packages/dtwclust/vignettes/timing-experiments.html

#  solved TO-DO: stack imbalace error, have a look at
#            https://github.com/snoweye/EMCluster/commit/3802f212fbe9dda4dbdcf65710de66a05542d6a8
#           https://github.com/snoweye/EMCluster/issues/1
#  solved TO-DO: add dtw_dismat0.cpp to build ignore or delete it... versioned and deleted
# 
# solved TO-DO: include data: http://r-pkgs.had.co.nz/data.html  