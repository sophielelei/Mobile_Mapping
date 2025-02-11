# quick check/subset


#select columns make less overwhelming
subset <- data_bib[,c("TI","Screen1_Reject")]

table(subset$Screen1_Reject)


subset[which(subset$Screen1_Reject==TRUE),1]
    