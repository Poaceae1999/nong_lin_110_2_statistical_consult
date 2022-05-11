library('tidyverse')
library(readxl)
library(data.table)
library(magrittr)
library(tibble)
# !!!Note,part of data ,3rd is reconsructed to new xlsx due to unknown bugs,has been modified by hand 
# tea_data

#use pattern of excel file name to import data
x <- c('五','六','七')
fresh_109_chem <- NULL
for (i in 1:3) {
  fresh_109_chem[i] <- paste0("D:/living/002_execute/Class/統計諮詢/農林公司/成分/109第",x[i],"水成分分析.xlsx")
}

fresh_109_chem_ij <- data.frame(1:5,1:5,1:5,1:5,1:5,1:5,1:5)
colnames(fresh_109_chem_ij) <- c("sheet1","sheet2","sheet3","sheet4","sheet5","sheet6","sheet7")
for (i in 1:7) {
  for (j in 1:7){
    fresh_109_chem_ij[][i,j] <- excel_sheets(fresh_109_chem[i])[j]
  }}
fresh_109_chem_ij
# import data to each variable 

for (i in 1:4) {
  try(assign(
    paste0('fresh_109_chem_',i),
    read_excel(fresh_109_chem[i],sheet = fresh_109_chem_ij[i,1])))   # assign value  to different variables
} 

fresh_109_chem_table=NULL
for (i in 1:5) {
  fresh_109_chem_table <- rbind(fresh_109_chem_table,colnames(get(paste0('fresh_109_chem_',i))))
  
}
fresh_109_chem_table



#fresh_108_chem_2 <- fresh_108_chem_2[,colnames(fresh_108_chem_1)]

fresh_109_chem_star <- list(fresh_109_chem_1,
                            fresh_109_chem_2,
                            fresh_109_chem_3
)

# combine data to one data frame
ST=1
ED=0
dat <- data.frame(matrix(0,24+18+6,15))
for (i in 1:length(fresh_109_chem_star)){
  for (j in length(rownames(fresh_109_chem_star[[i]]))) {
    ED <- ED + j
    dat[ST:ED,] <- fresh_109_chem_star[[i]][1:j,]
    ST <- ST + j
  }
  
}

dat$X18 <- NA

for (i in 1:48) {
  dat$X18[i] <- 'fresh leaves'
  if(str_detect(dat$X1[i],'B'))
    dat$X18[i] <- 'black tea'
  if(str_detect(dat$X1[i],'G'))
    dat$X18[i] <- 'green tea'
}
dat$X19 <- rep(1:3,16)
dat$X20 <- rep(c('第五水','第六水','第七水'),c(24,18,6))
dat$X1 <- gsub(pattern = '-B',replacement ='',dat$X1)
dat$X1 <- gsub(pattern = '-G',replacement ='',dat$X1)
dat$X1 <- gsub(pattern = '-1',replacement ='',dat$X1)
dat$X1 <- gsub(pattern = '-2',replacement ='',dat$X1)
dat$X1 <- gsub(pattern = '-3',replacement ='',dat$X1)
dat$X1 <- gsub(pattern = '農林',replacement ='',dat$X1)
dat$X1 <- gsub(pattern = '109',replacement ='',dat$X1)
dat$X1 <- gsub(pattern = '五',replacement ='',dat$X1)
dat$X1 <- gsub(pattern = '*台茶8號',replacement ='T8',dat$X1)
dat$X1 <- gsub(pattern = '*台茶18號',replacement ='T18',dat$X1)
dat$X1 <- gsub(pattern = '^[(]',replacement ='',dat$X1)
dat$X1 <- gsub(pattern = '^[)]',replacement ='',dat$X1)



#dat[,11:17] have ND
varname <- c('code','FAA','polyphenol','Theanine','caffeine','Gallic_Acid','GC','EGC','Catechin','EC','EGCG','GCG','ECG','CG','total_catechins','tea_kind','sample_label','season')
colnames(dat) <- varname
varname <- c('season','tea_kind','code','polyphenol','FAA','Theanine','caffeine','Gallic_Acid','Catechin','GC','EGC','EC','EGCG','GCG','ECG','total_catechins','sample_label','CG')
dat <- dat[,varname]

write.table(dat,file="D:/living/002_execute/Class/統計諮詢/農林公司/成分/nonglin_chem_109_5to7.csv",sep=',',na='NA',row.names=F)

nonglin_chem_108 <- read.csv("D:/living/002_execute/Class/統計諮詢/農林公司/成分/nonglin_chem_108.csv")
nonglin_chem_109_1 <-read.csv("D:/living/002_execute/Class/統計諮詢/農林公司/成分/nonglin_chem_109_1to4.csv")

colnames(nonglin_chem_108)[9:10]=colnames(nonglin_chem_109_1)[9:10]
nonglin_chem_merge <-rbind(nonglin_chem_108,nonglin_chem_109_1)
summary(nonglin_chem_merge)
nonglin_chem_merge$year <- rep(c(108,109),c(306,180))

nonglin_chem_avg <- data.frame(matrix(NA,243,19))
for (i in 1:243) {
  for (j in 1:19) {
    try(nonglin_chem_avg[i,j] <- (as.numeric(nonglin_chem_merge[2*i,j])+as.numeric(nonglin_chem_merge[2*i-1,j]))/2)

    }
}

for (i in 1:243) {
  for (j in 1:3) {
    try(nonglin_chem_avg[i,j] <- (nonglin_chem_merge[2*i-1,j]))
  }
}
colnames(nonglin_chem_avg)<-c(colnames(nonglin_chem_109_1),'year')
nonglin_chem_avg <- nonglin_chem_avg[,-4]

nonglin_chem_109_2 <-read.csv("D:/living/002_execute/Class/統計諮詢/農林公司/成分/nonglin_chem_109_5to7.csv")
nonglin_chem_109_2$year <-109
nonglin_chem_avg$CG <- NA
nonglin_chem_109_2 <- nonglin_chem_109_2[,colnames(nonglin_chem_avg)]

nonglin_chem_merge_final <- rbind(nonglin_chem_avg,nonglin_chem_109_2)
nonglin_chem_merge_final$season <- gsub(pattern = '第一',replacement ='1st',nonglin_chem_merge_final$season)
nonglin_chem_merge_final$season <- gsub(pattern = '第二',replacement ='2nd',nonglin_chem_merge_final$season)
nonglin_chem_merge_final$season <- gsub(pattern = '第三',replacement ='3rd',nonglin_chem_merge_final$season)
nonglin_chem_merge_final$season <- gsub(pattern = '第四',replacement ='4th',nonglin_chem_merge_final$season)
nonglin_chem_merge_final$season <- gsub(pattern = '第五',replacement ='5th',nonglin_chem_merge_final$season)
nonglin_chem_merge_final$season <- gsub(pattern = '第六',replacement ='6th',nonglin_chem_merge_final$season)
nonglin_chem_merge_final$season <- gsub(pattern = '第七',replacement ='7th',nonglin_chem_merge_final$season)


write.table(nonglin_chem_merge_final,file="D:/living/002_execute/Class/統計諮詢/農林公司/成分/nonglin_chem.csv",sep=',',na='NA',row.names=F)
