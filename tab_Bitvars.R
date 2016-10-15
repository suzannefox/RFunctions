tab_BitVars <- function(myDataframe) {
  
  myDataframe <- as.data.frame(sapply(media.useddata, function(y) as.character(y)))
  
  count_names <- as.data.frame(names(myDataframe))
  count_tot <- as.data.frame(sapply(myDataframe, function(y) nrow(myDataframe)))
  count_0 <- as.data.frame(sapply(myDataframe, function(y) sum(length(which(y=="0")))))
  count_1 <- as.data.frame(sapply(myDataframe, function(y) sum(length(which(y=="1")))))
  count_na <- as.data.frame(sapply(myDataframe, function(y) sum(length(which(is.na(y))))))
  
  Crosstab  <- data.frame(count_names[1], count_tot[1], count_1[1], count_0[1], count_na[1])
  colnames(Crosstab)<- c("Variable","Total","1","0","NA")
  
  #print (Crosstab)
  return (Crosstab)
}