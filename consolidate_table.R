## Convert 3 columns to matrix

library (reshape2)
jeter_filtered_list_ARNAU <- read_csv("~/Desktop/jeter_filtered_list_ARNAU.csv")
x=jeter_filtered_list_ARNAU
#SAMPLE  CALL    Concatenate
#FHproject_NP1-2   0/1 16_1245582_A_G
#FHproject_NP1-5   0/1 16_1258188_C_G
#FHproject_NP1-16   0/1 16_1260072_A_G
#FHproject_NP1-17   0/1 16_1251821_C_T

x2=df[,c(1,3,2)]  ## Here I just change the order of the columns to make sure that the CALL columns is the last one 
data3 <- dcast(x2, ...~Concatenate)
data3[is.na (data3)] <- 0   ## Change NA's for 0 
data3[data3=="0/1"]<-1 
data3[data3=="1/1"]<-2
write.table(data3,"~/Desktop/my_output.txt", sep="\t", quotes=F)
