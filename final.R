library('tidyverse')
library(readxl)
library(data.table)
library(magrittr)

chaician <- read.csv("D:/living/002_execute/Class/参璸吭高/笰狶そ/蹦玡/nonglin_chaichian.csv")
chem <- read.csv("D:/living/002_execute/Class/参璸吭高/笰狶そ/Θだ/nonglin_chem.csv")
tea <- read.csv("D:/living/002_execute/Class/参璸吭高/笰狶そ/底/nonglin_tea.csv")
nonling <- read.csv("D:/living/002_execute/Class/参璸吭高/笰狶そ/nonglin.csv")

tea_avg <- data.frame(matrix(NA,147,13))
colnames(tea_avg) <- colnames(tea)
for (i in 6:13) {
  tea[,i] <- as.numeric(tea[,i])
}
tea$location <- gsub('る','_',tea$location)
tea$location <- gsub('ら','',tea$location)
tea$location <- gsub('跋','',tea$location)

for (i in 1:147) {
  tea_avg[i,] <- tea[10*i,]
  tea_avg[i,6:13] <- colMeans(tea[(1+(i-1)*10):(i*10),6:13])
  
}
write.csv(tea_avg,"D:/living/002_execute/Class/参璸吭高/笰狶そ/底/nonglin_tea_avg.csv")

chem$code <- gsub('﹗琄','FS',chem$code)
chem$code <- gsub('獵み蒒','DP',chem$code)

tea_avg$key <- paste0(tea_avg$year,tea_avg$season,tea_avg$cultivar,tea_avg$sample_label)
chaician$key<- paste0(chaician$year,chaician$season,chaician$cultivar,chaician$replication)
chem$key <- paste0(chem$year,chem$season,chem$code,chem$sample_label)

dat <- data.frame(matrix(NA,291,50))
datcol <- c('key','year','season','observe_date','cutting_date','location_1','location_2','location_3','cultivar','tea_kind','sample_label','replication',colnames(chaician)[10:27],colnames(tea)[8:13],colnames(chem)[6:19])
colnames(dat) <- datcol

dat$key <- chem$key
dat$year <- chem$year
dat$season <- chem$season
dat$sample_label <-chem$sample_label
dat$replication <- chem$sample_label
dat$cultivar <- chem$code
dat$tea_kind <- chem$tea_kind
dat[,37:50] <-chem[,6:19]

for (i in 1:291) {
  for (j in 1:147) {
    if(dat$key[i]==chaician$key[j])
    {
      dat$observe_date[i] <- chaician$observe_date[j]
      dat$cutting_date[i] <- chaician$cutting_date[j]
      dat$location_1[i] <- chaician$location_1[j]
      dat$location_2[i] <- chaician$location_2[j]
      dat$location_3[i] <- chaician$location_3[j]
      dat[i,13:30] <- chaician[j,10:27]
    }
    if(dat$key[i]==tea_avg$key[j])
    {
      dat$cutting_height[i] <- tea_avg$cutting_height[j]
      dat$tea_buds_cm[i] <- tea_avg$tea_buds_cm[j]
      dat$leaf_number[i] <- tea_avg$leaf_number[j]
      dat$avg_inter_node[i] <- tea_avg$avg_inter_node[j]
      dat$open_plane[i] <- tea_avg$open_plane[j]
      dat$buds_weight_100[i] <- tea_avg$buds_weight_100[j]
    }
  }
}

for (i in 1:291) {
  for (j in 1:147) {
    if(dat$location_1[i]==''){
    if(dat$key[i]==tea_avg$key[j])
    {
      dat$location_1[i] <- tea_avg$location[j]
    }
  }}
}
for (i in 1:50) {
  if(table(is.na(dat[,i]))[1]!=291)
{  print(colnames(dat)[i])
  print(table(is.na(dat[,i])))}
}
for (i in 1:147) {
  if(is.na(tea_avg$cutting_height[i]))
    print(row(tea_avg)[i,1])
}

nonling$tea_buds_cm/nonling$leaf_number==nonling$avg_inter_node

write.csv(dat,"D:/living/002_execute/Class/参璸吭高/笰狶そ/nonglin.csv")

