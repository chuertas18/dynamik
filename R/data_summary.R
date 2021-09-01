#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
# http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
#+++++++++++++++++++++++++

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd= sd(x[[col]], na.rm=TRUE),
      se=(sd(x[[col]])/sqrt(length(x[[col]])-1)))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum$min<-data_sum$mean-data_sum$se
  data_sum$max<-data_sum$mean+data_sum$se
  # data_sum <- rename(data_sum, c("mean" = varname))
  #
  return(data_sum)
}
