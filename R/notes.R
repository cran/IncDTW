#  --------------------------------------------------------------------------  
#  --------------------------------------------------------------------------
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
# TODO: upcoming features: - incremental DTW for stream data with running normalization 
#                          - norm01 from caterpillar package without discretization
#                          - plot functions fpr DBA for multivariate time series
#                          - plot function for reverse (start-) partial alignment
#                          - ...
#                          - ...
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