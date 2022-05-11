library('tidyverse')
library(readxl)
library(data.table)
library(magrittr)
# !!!Note,part of data ,order of open_plane buds_weight and avg_internode,has been modified by hand 
# tea_data
{
#use pattern of excel file name to import data
x <- c('一','二','三','四','五','六','七')
fresh_109 <- NULL
for (i in 1:7) {
  fresh_109[i] <- paste0("D:/living/002_execute/Class/統計諮詢/農林公司/茶菁/109茶菁-第",x[i],"水.xlsx")
}

fresh_109_ij <- data.frame(matrix(0,nrow=7,ncol=7))
colnames(fresh_109_ij) <- c("sheet1","sheet2","sheet3","sheet4","sheet5","sheet6","sheet7")
for (i in 1:7) {
  for (j in 1:7){
      fresh_109_ij[][i,j] <- excel_sheets(fresh_109[i])[j]
  }}

# import data to each variable 

for (i in 1:7) {
  for (j in 1:7) {
    try(assign(
      paste0('fresh_109_',i,'_',j),
      read_excel(fresh_109[i],sheet = fresh_109_ij[i,j])))   # assign value  to different variables
  } 
}


# create space in the beginning of each data frame to place 5 variables which is not in format.
for (i in 1:7) {
  for (j in 1:7) {
    try(
      assign(paste0('fresh_109_',i,'_',j),
             cbind(NA,NA,NA,NA,NA,
                   get(paste0('fresh_109_',i,'_',j))))) # assign value  to different variables  
  } 
}

# gather variables to a list
fresh_109_star <- list(fresh_109_1_1,
                       fresh_109_1_2,
                       fresh_109_2_1,
                       fresh_109_2_4,
                       fresh_109_2_5,
                       fresh_109_2_6,
                       fresh_109_2_7,
                       fresh_109_3_1,
                       fresh_109_3_2,
                       fresh_109_3_3,
                       fresh_109_3_4,
                       fresh_109_3_5,
                       fresh_109_4_1,
                       fresh_109_4_2,
                       fresh_109_4_3,
                       fresh_109_5_1,
                       fresh_109_5_2,
                       fresh_109_5_3,
                       fresh_109_5_4,
                       fresh_109_6_1,
                       fresh_109_6_2,
                       fresh_109_6_3,
                       fresh_109_7_1
                       )

# put values to each variables
for (i in 1:length(fresh_109_star)){
  fresh_109_star[[i]][,1] <- fresh_109_star[[i]][1,7]
  fresh_109_star[[i]][,2] <- fresh_109_star[[i]][2,7]
  fresh_109_star[[i]][,3] <- fresh_109_star[[i]][3,7]
  fresh_109_star[[i]][,4] <- fresh_109_star[[i]][4,7]
  fresh_109_star[[i]][,5] <- if(is.na(fresh_109_star[[i]][2,9])){fresh_109_star[[i]][2,10]}else{fresh_109_star[[i]][2,9]}
}

# delete the first 5 rows which is combine to the first 5 column in the step above
for (i in c(1:18,20:23)){
  print(i)
  fresh_109_star[[i]] <- fresh_109_star[[i]][6:35,1:11]
}
fresh_109_star[[19]] <- fresh_109_star[[19]][6:35,1:10]


# combine data to one data frame
ST=1
ED=0
dat <- data.frame(matrix(0,30*23,12))
for (i in 1:length(fresh_109_star)){
  for (j in length(rownames(fresh_109_star[[i]]))) {
    ED <- ED + j
    dat[ST:ED,] <- fresh_109_star[[i]][1:j,]
    ST <- ST + j
  }
  
}

# renaming column
colnames(dat) <- c('date','cultivar','location','season','cutting_height','replication','tea_buds_cm','leaf_number','open_plane','buds_weight_100','sample_label','avg_inter_node')

# filling sample_label
for (i in 1:length(dat$sample_label)) {
  if(is.na(dat$sample_label[i]))
    dat$sample_label[i] <- dat$sample_label[i-1]
  else if(str_detect(dat$sample_label[i],'採收'))
    dat$sample_label[i] <- dat$sample_label[i-1]
  else if((str_detect(dat$sample_label[i],'採錯高度')))
    dat$sample_label[i] <- "茶菁1"
}
dat$sample_label[541:570] <- c(rep('茶菁1',10),rep('茶菁2',10),rep('茶菁3',10))
# filling buds_weight_100
for (i in 1:length(dat$buds_weight_100)) {
  if(is.na(dat$buds_weight_100[i]))
    dat$buds_weight_100[i] <- dat$buds_weight_100[i-1]
}

# delete 'cm' in cutting_height and transform it to int
dat$cutting_height <- as.integer(str_sub(dat$cutting_height,1,2))
#transform numeric data
dat$tea_buds_cm <- as.numeric(dat$tea_buds_cm)
dat$leaf_number <- as.numeric(dat$leaf_number)
dat$open_plane <- as.numeric(dat$open_plane)
dat$buds_weight_100 <- as.numeric(dat$buds_weight_100)
#compute avg_inter_node
dat$avg_inter_node <- dat$tea_buds_cm/dat$leaf_number
#set the format as 108's
dat <- dat[,c('date','cultivar','location','season','cutting_height','replication','tea_buds_cm','leaf_number','avg_inter_node','open_plane','buds_weight_100','sample_label')]

#View the final output
table(dat$date)
table(dat$cultivar)
table(dat$location)
table(dat$season)
table(dat$cutting_height)
table(dat$replication)
table(dat$tea_buds_cm)
table(dat$leaf_number)
table(dat$avg_inter_node)
table(dat$open_plane)
table(dat$buds_weight_100)
table(dat$sample_label)
}
write.table(dat,file="D:/living/002_execute/Class/統計諮詢/農林公司/茶菁/nonglin_tea_109_2.csv",sep=',',na='NA',row.names=F)

nongling109 <- read.csv("D:/living/002_execute/Class/統計諮詢/農林公司/茶菁/nonglin_tea_109_2.csv")
nongling108 <- read.csv("D:/living/002_execute/Class/統計諮詢/農林公司/茶菁/nonglin_tea_108.csv")
year <- 108
nongling108 <- cbind(nongling108,year)
nongling109 <- cbind(nongling109,year)
nongling <- rbind(nongling108,nongling109)
nongling <- nongling[-11*(1:78),1:13] 
nongling <- nongling[,c('year','date','season','location','cultivar','sample_label','replication','cutting_height','tea_buds_cm','leaf_number','avg_inter_node','open_plane','buds_weight_100')]

write.table(nongling,file="D:/living/002_execute/Class/統計諮詢/農林公司/茶菁/nonglin_tea.csv",sep=',',na='NA',row.names=F)
