library('tidyverse')
library(readxl)
library(data.table)
library(magrittr)
library(tibble)
# !!!Note,part of data ,3rd is reconsructed to new xlsx due to unknown bugs,has been modified by hand 
# tea_data

#use pattern of excel file name to import data
x <- c('二','三','四','五','六')
fresh_108_chem <- NULL
for (i in 1:5) {
  fresh_108_chem[i] <- paste0("D:/living/002_execute/Class/統計諮詢/農林公司/成分/108農林第",x[i],"水化學成分(raw).xlsx")
}

fresh_108_chem_ij <- data.frame(1:5,1:5,1:5,1:5,1:5,1:5,1:5)
colnames(fresh_108_chem_ij) <- c("sheet1","sheet2","sheet3","sheet4","sheet5","sheet6","sheet7")
for (i in 1:7) {
  for (j in 1:7){
    fresh_108_chem_ij[][i,j] <- excel_sheets(fresh_108_chem[i])[j]
  }}
fresh_108_chem_ij
# import data to each variable 

for (i in 1:5) {
  try(assign(
    paste0('fresh_108_chem_',i),
    read_excel(fresh_108_chem[i],sheet = fresh_108_chem_ij[i,1])))   # assign value  to different variables
} 

fresh_108_chem_table=NULL
for (i in 1:5) {
  fresh_108_chem_table <- rbind(fresh_108_chem_table,colnames(get(paste0('fresh_108_chem_',i))))
  
}
fresh_108_chem_table

fresh_108_chem_5$repucation <- NA
fresh_108_chem_4$repucation <- NA
fresh_108_chem_3$repucation <- NA
fresh_108_chem_2$repucation <- NA

colnames(fresh_108_chem_1)
colnames(fresh_108_chem_2)[7] = "caffeine"
colnames(fresh_108_chem_2)[16] = "total catechins"
colnames(fresh_108_chem_3)[7] = "total catechins"
colnames(fresh_108_chem_3)[9] = "caffeine"
colnames(fresh_108_chem_4)[9] = 'Gallic Acid'
colnames(fresh_108_chem_5)[16] = "total catechins"
fresh_108_chem_3 <- fresh_108_chem_3[,-18]
fresh_108_chem_3 <- fresh_108_chem_3[,-9]


fresh_108_chem_2 <- fresh_108_chem_2[,colnames(fresh_108_chem_1)]
fresh_108_chem_3 <- fresh_108_chem_3[,colnames(fresh_108_chem_1)]
fresh_108_chem_4 <- fresh_108_chem_4[,colnames(fresh_108_chem_1)]
fresh_108_chem_5 <- fresh_108_chem_5[,colnames(fresh_108_chem_1)]

fresh_108_chem_table <- NULL
for (i in 1:5) {
  fresh_108_chem_table <- rbind(fresh_108_chem_table,colnames(get(paste0('fresh_108_chem_',i))))
}
fresh_108_chem_table

fresh_108_chem_star <- list(fresh_108_chem_1,
                            fresh_108_chem_2,
                            fresh_108_chem_3,
                            fresh_108_chem_4,
                            fresh_108_chem_5
)

# combine data to one data frame
ST=1
ED=0
dat <- data.frame(matrix(0,79+85+49*3,17))
for (i in 1:length(fresh_108_chem_star)){
  for (j in length(rownames(fresh_108_chem_star[[i]]))) {
    ED <- ED + j
    dat[ST:ED,] <- fresh_108_chem_star[[i]][1:j,]
    ST <- ST + j
  }
  
}

table(dat$X1)
dat <- dat[-grep('season',dat$X1),]
table(dat$X1)

table(dat$X2)
for (i in 1:length(rownames(dat)))
{
  if(dat$X2[i]=='blake tea')
    dat$X2[i] <- 'black tea'
  if(dat$X2[i]=='fresh tea'||dat$X2[i]=='茶菁')
    dat$X2[i] <- 'fresh leaves'
}
table(dat$X2)


table(dat$X3)
dat$X3 <- gsub(pattern = '-B-1',replacement ='',dat$X3)
dat$X3 <- gsub(pattern = '-B-2',replacement ='',dat$X3)
dat$X3 <- gsub(pattern = '-B-3',replacement ='',dat$X3)
table(dat$X3)

table(dat$X4)
dat$X4 <- rep(1:2,length(dat$X4)/2)
table(dat$X4)

#dat[,11:17] have ND
colnames(dat) <- colnames(fresh_108_chem_1)
colnames(dat)[2] <- 'tea_kind'
colnames(dat)[4] <- 'replication'
colnames(dat)[5] <- 'polyphenol'
colnames(dat)[17] <- 'total_catechins'
dat$sample_label <- rep(c(1,1,2,2,3,3),length(dat[,1])/6) 
write.table(dat,file="D:/living/002_execute/Class/統計諮詢/農林公司/成分/nonglin_chem_108.csv",sep=',',na='NA',row.names=F)


