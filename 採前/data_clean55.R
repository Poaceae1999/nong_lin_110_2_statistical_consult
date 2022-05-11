library('tidyverse')
library(readxl)
library(data.table)
library(magrittr)
# !!!Note,part of data ,order of open_plane buds_weight and avg_internode,has been modified by hand 
# tea_data
{
  #use pattern of excel file name to import data
  x <- c('一','二','三','四','五','六','七')
  fresh <- NULL
  for (i in 1:7) {
    fresh[i+6] <- paste0("D:/living/002_execute/Class/統計諮詢/農林公司/採前/109採前生長量調查-第",x[i],"水.xlsx")
  }  
  fresh_ij <- data.frame(matrix(NA,1,8*13))
  colnames(fresh_ij) <- c("sheet1","sheet2","sheet3","sheet4","sheet5","sheet6","sheet7")
  for (i in 1:13){
  for (j in 1:8) {
      try(fresh_ij[(i-1)*8+j] <- excel_sheets(fresh[i])[j])
    }}
  fresh_ij
  fresh_star <-rep(list(matrix(NA,30,10)),8*13)
  # import data to each variable 
  for (i in 1:104)
  {
    fresh_star[[i]] <- try(read_xlsx(path=as.character(files[i]),sheet = as.character(fresh_ij[i])))
  }
  j=0
  fresh_star_star <-rep(list(matrix(NA,30,10)),49)
  for (i in 1:104) {
    if(length(rownames(fresh_star[[i]]))>21)
    {
      j=j+1
      fresh_star_star[[j]]<-fresh_star[[i]]
    }
  }
  a=data.frame(matrix(NA,50,10))
  # 
  for(i in 1:49)
  {
  a[i,] = fresh_star_star[[i]][5,]
  }
  print(a)
  
  for (i in 1:49) {
    fresh_star_star[[i]]<-cbind(NA,NA,NA,NA,NA,fresh_star_star[[i]])
  }
  for (i in 1:49) {
  fresh_star_star[[i]][,1] <- fresh_star_star[[i]][1,7]
  fresh_star_star[[i]][,2] <- fresh_star_star[[i]][2,7]
  fresh_star_star[[i]][,3] <- fresh_star_star[[i]][3,7]
  fresh_star_star[[i]][,4] <- fresh_star_star[[i]][4,7]
  if(is.na(fresh_star_star[[i]][1,9])){
    fresh_star_star[[i]][,5] <- paste(fresh_star_star[[i]][1,8],
                                      fresh_star_star[[i]][1,10],
                                      fresh_star_star[[i]][1,11],
                                      fresh_star_star[[i]][1,12],
                                      fresh_star_star[[i]][1,13],
                                      fresh_star_star[[i]][1,14])}
  else if((nchar(fresh_star_star[[i]][1,9])>12)||(nchar(fresh_star_star[[i]][1,9])<10))
    fresh_star_star[[i]][,5] <- fresh_star_star[[i]][1,9]
  else 
    fresh_star_star[[i]][,5] <- paste(fresh_star_star[[i]][1,10],
                                          fresh_star_star[[i]][1,11],
                                          fresh_star_star[[i]][1,12],
                                          fresh_star_star[[i]][1,13],
                                          fresh_star_star[[i]][1,14])}
 
  
  for (i in 1:49) {
    fresh_star_star[[i]] <- fresh_star_star[[i]][-4:-1,]
  }
  for (i in 1:49) {
    #colnames(fresh_star_star[[i]]) <- c('observe_date','cultivar','location','season','cutting_date',fresh_star_star[[i]][1,6:length(colnames(fresh_star_star[[i]]))])
    fresh_star_star[[i]] <- fresh_star_star[[i]][-1,]
  }
  
  fresh_table <-rep(list(matrix(NA,3,22)),49)
  for (i in 1:49) 
  {
   
  }
  for (i in 1:49) {
    if((i>=1)&&(i<=7))
      fresh_star_star[[i]] <- fresh_star_star[[i]][,-9:-8]
    if((i>=8)&&(i<=14))
      fresh_star_star[[i]] <- fresh_star_star[[i]][,-8]
    if((i>=30)&&(i<=48))
      fresh_star_star[[i]] <- fresh_star_star[[i]][,-8]
  }
  for (i in 1:49) {
    fresh_star_star[[i]] <-fresh_star_star[[i]][,-9]
  }
  for (i in 1:49) {
    fresh_star_star[[i]] <-cbind(fresh_star_star[[i]][,1:9],fresh_star_star[[i]][,'備註'])
  }
  for(i in 1:49)
    {
    fresh_star_star[[i]]$發育狀態 <- gsub('1心','',fresh_star_star[[i]]$發育狀態)
  }
 for (i in 1:49) 
  {
    t=0
    colnames(fresh_table[[i]]) <- c('observe_date','cultivar','location','season','cutting_date','replication','G2L','G3L','G4L','G5L','G6L','G7L','G8L','G9L','O2L','O3L','O4L','O5L','O6L','O7L','O8L','O9L')
      fresh_table[[i]][,1] <- fresh_star_star[[i]][1,1]
      fresh_table[[i]][,2] <- fresh_star_star[[i]][1,2]
      fresh_table[[i]][,4] <- fresh_star_star[[i]][1,4]
      fresh_table[[i]][,5] <- fresh_star_star[[i]][1,5]
    for (k in 1:length(rownames(fresh_star_star[[i]]))) 
    {
      if(is.na(fresh_star_star[[i]][k,7]))
      {
        next
      }
      if(not(is.na(fresh_star_star[[i]][k,6])))
      {
        t=t+1
        fresh_table[[i]][t,6] <- fresh_star_star[[i]][k,6]
        fresh_table[[i]][t,3] <- fresh_star_star[[i]][k,10]
      }
      for (j in 2:9) 
      {
        if(str_detect(fresh_star_star[[i]][k,7],as.character(j)))
          {
            fresh_table[[i]][t,j+5] <- fresh_star_star[[i]][k,8]
            fresh_table[[i]][t,j+13] <- fresh_star_star[[i]][k,9]
          }
      }
    }    
  }
  # combine data to one data frame
  ST=1
  ED=0
  dat <- data.frame(matrix(NA,3*49,22))
  for (i in 1:length(fresh_table)){
      j <- max(row(fresh_table[[i]])) 
      ED <- ED + j
      dat[ST:ED,] <- fresh_table[[i]][1:j,]
      ST <- ST + j
  }
  colnames(dat)<- c('observe_date','cultivar','location','season','cutting_date','replication','G2L','G3L','G4L','G5L','G6L','G7L','G8L','G9L','O2L','O3L','O4L','O5L','O6L','O7L','O8L','O9L')
  # renaming column
  colnames(dat) <- colnames(fresh_1)
  dat$cultivar <- gsub(pattern = '青心大冇'),replacement ='DP',dat$cultivar)
  dat$cultivar <- gsub(pattern = '四季春',replacement ='FS',dat$cultivar)
  dat$cultivar <- gsub(pattern = '號',replacement ='',dat$cultivar)
  dat$cultivar <- gsub(pattern = '台茶',replacement ='T',dat$cultivar)
  dat$season <- gsub(pattern = '第二水',replacement ='2nd',dat$season)
  dat$season <- gsub(pattern = '第三水',replacement ='3rd',dat$season)
  dat$season <- gsub(pattern = '第四水',replacement ='4th',dat$season)
  dat$season <- gsub(pattern = '第五水',replacement ='5th',dat$season)
  dat$season <- gsub(pattern = '第六水',replacement ='6th',dat$season)
  dat$season <- gsub(pattern = '第七水',replacement ='7th',dat$season)
  dat$season <- gsub(pattern = '第一水',replacement ='1st',dat$season)
  dat$cutting_date <- gsub(pattern = 'NA',replacement ='',dat$cutting_date)
  dat$cutting_date <- gsub(pattern = ' ',replacement ='',dat$cutting_date)
  dat$cutting_date <- gsub(pattern = '修剪日期：',replacement ='',dat$cutting_date)
  dat$cutting_date <- gsub(pattern = '前一水採收/',replacement ='',dat$cutting_date)
  dat$cutting_date <- gsub(pattern = '※第五水未採收，亦未修剪。',replacement ='',dat$cutting_date)
  
  dat <- dat[,c(-4,-6)]
  dat$replication <- rep(c(1,2,3),22)
  colnames(dat) <- c('season','cultivar','non_openplane_ratio','non_openplane_ratio','leaf_num_2','leaf_num_3','leaf_num_4','leaf_num_5','leaf_num_6','leaf_num_7','leaf_num_8','leaf_num_9','leaf_num_total','replication')
  dat <- dat[,c('season','cultivar','replication','non_openplane_ratio','non_openplane_ratio','leaf_num_2','leaf_num_3','leaf_num_4','leaf_num_5','leaf_num_6','leaf_num_7','leaf_num_8','leaf_num_9','leaf_num_total')]
  write.table(dat,file="D:/living/002_execute/Class/統計諮詢/農林公司/採前/nonglin_chaichian.csv",sep=',',na='NA',row.names=F)
