library('tidyverse')
library(readxl)
library(data.table)
library(magrittr)
library(tibble)
# !!!Note,part of data ,3rd is reconsructed to new xlsx due to unknown bugs,has been modified by hand 
# tea_data

#use pattern of excel file name to import data
x <- c('','','','')
fresh_109_chem <- NULL
for (i in 1:4) {
  fresh_109_chem[i] <- paste0("D:/living/002_execute/Class/参璸吭高/笰狶そ/Θだ/109材",x[i],"Θだだ猂.xlsx")
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
fresh_109_chem_4 <- fresh_109_chem_4[,-17]



#fresh_108_chem_2 <- fresh_108_chem_2[,colnames(fresh_108_chem_1)]

fresh_109_chem_star <- list(fresh_109_chem_1,
                            fresh_109_chem_2,
                            fresh_109_chem_3,
                            fresh_109_chem_4
)

# combine data to one data frame
ST=1
ED=0
dat <- data.frame(matrix(0,27+60*2+36,17))
for (i in 1:length(fresh_109_chem_star)){
  for (j in length(rownames(fresh_109_chem_star[[i]]))) {
    ED <- ED + j
    dat[ST:ED,] <- fresh_109_chem_star[[i]][1:j,]
    ST <- ST + j
  }
  
}

table(dat$X1)
dat <- dat[-27:-25,]
table(dat$X1)

table(dat$X2)
for (i in 1:length(rownames(dat)))
{
  if(dat$X2[i]=='blake tea')
    dat$X2[i] <- 'black tea'
  if(dat$X2[i]=='fresh tea'||dat$X2[i]=='底')
    dat$X2[i] <- 'fresh leaves'
}
dat$X18 <- NA

for (i in 1:180) {
  dat$X18[i] <- 'fresh leaves'
  if(str_detect(dat$X2[i],'B'))
    dat$X18[i] <- 'black tea'
  if(str_detect(dat$X2[i],'G'))
    dat$X18[i] <- 'green tea'
}

table(dat$X2)
dat$X2 <- gsub(pattern = '-B',replacement ='',dat$X2)
dat$X2 <- gsub(pattern = '-G',replacement ='',dat$X2)



#dat[,11:17] have ND
varname <- c('season','code','replication','water_contained_ratio','polyphenol','FAA','Theanine','caffeine','Gallic_Acid','Catechin','GC','EGC','EC','EGCG','GCG','ECG','total_catechins','tea_kind')
colnames(dat) <- varname
dat$sample_label <- rep(c(1,1,2,2,3,3),length(dat[,1])/6) 
varname <- c('season','tea_kind','code','replication','polyphenol','FAA','Theanine','caffeine','Gallic_Acid','Catechin','GC','EGC','EC','EGCG','GCG','ECG','total_catechins','sample_label')
dat <- dat[,varname]

write.table(dat,file="D:/living/002_execute/Class/参璸吭高/笰狶そ/Θだ/nonglin_chem_109_1to4.csv",sep=',',na='NA',row.names=F)


