library(tidyverse)
library(lubridate)
library(rpart)

tea <- read_csv("D:/living/002_execute/Class/統計諮詢/農林公司/nonglin.csv")
weather0 <-
  read_csv("D:/living/002_execute/Class/統計諮詢/農林公司/氣象/nonglin_weather.csv")

# treat tea data and save RDS ----
tea$observe_ys <- paste(tea$year, tea$season)
tea$observe_date <- as.POSIXct(tea$observe_date)
tea$previous_cutting_date <- as.POSIXct(tea$previous_cutting_date)
Total_G <- tea %>%
  select(.,G1L:G9L) %>%
  rowSums()
Total_O <- tea %>%
  select(.,O1L:O9L) %>%
  rowSums()
Total_leaf <- Total_G + Total_O
GORatio <- Total_G / Total_O
OTRatio <- Total_O / Total_leaf
tea <- cbind(tea, Total_leaf, GORatio, OTRatio)
tea$total_catechins <-
  ifelse(is.na(tea$CG),
    tea$total_catechins,
    tea$total_catechins - tea$CG
  )


for (i in 1:length(tea$observe_date)) {
  if (i < 154) {
    year(tea$observe_date[i]) <- 2019
    year(tea$previous_cutting_date[i]) <- 2019
  }
  else {
    year(tea$observe_date[i]) <- 2020
    year(tea$previous_cutting_date[i]) <- 2020
  }
}
previous_cutting_date <- ifelse((tea$observe_date - tea$previous_cutting_date) < 0, tea$previous_cutting_date - 365 * 24 * 3600, tea$previous_cutting_date)
class(previous_cutting_date) <- c("POSIXct", "POSIXt") # attributes(previous_cutting_date) <- attributes(tea$observe_date)
tea$previous_cutting_date <- previous_cutting_date
tea$duration_oc <- tea$observe_date - tea$previous_cutting_date
level <- vector('character',length(tea$key))

level[tea$cultivar%in%c('T1','T8','T18')&tea$leaf_number<=4&tea$open_plane<=0.1] <- 'A'
level[tea$cultivar%in%c('T1','T8','T18')&tea$leaf_number<=4&tea$open_plane>0.1] <- 'B'
level[tea$cultivar%in%c('T1','T8','T18')&tea$leaf_number>4&tea$open_plane<=0.1] <- 'B'
level[tea$cultivar%in%c('T1','T8','T18')&tea$leaf_number>4&tea$open_plane>0.1] <- 'C'
level[tea$cultivar%in%c('T1','T8','T18')&tea$tea_buds_cm>15] <- 'C'

level[tea$cultivar%in%c('T12','T17','DP','FS')&tea$leaf_number<=3&tea$open_plane<=0.1] <- 'A'
level[tea$cultivar%in%c('T12','T17','DP','FS')&tea$leaf_number<=3&tea$open_plane>0.1] <- 'B'
level[tea$cultivar%in%c('T12','T17','DP','FS')&tea$leaf_number>3&tea$open_plane<=0.1] <- 'B'
level[tea$cultivar%in%c('T12','T17','DP','FS')&tea$leaf_number>3&tea$open_plane>0.1] <- 'C'
tea$level <- level

export <- tea %>%
  filter(,cultivar!='T20') %>% 
  select(, -c(CG, replication)) %>%
  select(, key:season, observe_ys, duration_oc, everything()) %>%
  select(, key:sample_label, Total_leaf, GORatio, OTRatio, everything())
saveRDS(export, "nonglin_tea.RDS")
# na treatment ----
weather <-
  read_csv("D:/living/002_execute/Class/統計諮詢/農林公司/氣象/nongling_weather_20220428.csv")
tea <- readRDS("nonglin_tea.RDS")

# to fill temp to previous day
weather <- as_tibble(weather)
summary(weather)

weather$month
Month_list <- list('January','February','March','April','May','June','July','August','September','October','November','December')
month_list <- list('-01','-02','-03','-04','-05','-06','-07','-08','-09','-10','-11','-12')
Month_tranform_list <- list(Month_list,month_list)
for (i in 1:12) {
  weather$month <- gsub(Month_tranform_list[[1]][[i]],Month_tranform_list[[2]][[i]],weather$month)
}
weather$date <- paste0(weather$month,'-',weather$DAY) %>% as.POSIXct()
Aug_NA <- filter(weather,month=='2020-08') %>% select(,`SOLAR RAD(H)`)
Aug_NA <- Aug_NA[[1]] %>% mean(,na.rm = TRUE)
Aug_NA2 <- filter(weather,month=='2020-08') %>% select(,`SOLAR RAD(MJ/M2)`)
Aug_NA2 <- Aug_NA2[[1]] %>% mean(,na.rm = TRUE)
Aug_NA3 <- filter(weather,month=='2020-08') %>% select(,`RH%`)
Aug_NA3 <- Aug_NA3[[1]] %>% mean(,na.rm = TRUE)
weather[is.na(weather$`SOLAR RAD(H)`),11:13] <- cbind(Aug_NA3,Aug_NA,Aug_NA2)

View(weather[is.na(weather$MEAN.TEMP),])

for (i in 1:length(weather$MEAN.TEMP)) {
  if (is.na(weather$MEAN.TEMP[i])) {
    weather$MEAN.TEMP[i] <- weather$MEAN.TEMP[i - 1]
    weather$HIGH[i] <- weather$HIGH[i - 1]
    weather$LOW[i] <- weather$LOW[i - 1]
    weather$RAIN[i] <- weather$RAIN[i - 1]
  }
}



# combine weather and tea----
acu_mean_temp <- vector("double", length(tea$previous_cutting_date))
Solar_rad_H <- vector("double", length(tea$previous_cutting_date))
Solar_rad_MJM2 <- vector("double", length(tea$previous_cutting_date))
RH <- vector("double", length(tea$previous_cutting_date))
growth_mean_temp <- vector("double", length(tea$previous_cutting_date))
rain <- vector("double", length(tea$previous_cutting_date))
temp_differ <- vector("double", length(tea$previous_cutting_date))
for (i in 1:length(tea$previous_cutting_date)) {
  idx_c <- which(weather$date == tea$previous_cutting_date[i])
  idx_o <- which(weather$date == tea$observe_date[i])
  if (length(idx_c) == 0 || length(idx_o) == 0) {
    # acu_mean_temp[i] <- 'can not map date'
    next
  }
  l_mean_temp <- weather$MEAN.TEMP[idx_c:idx_o] %>% cumsum()%>% last()
  l_rain <- weather$RAIN[idx_c:idx_o] %>% cumsum() %>% last()
  l_solar_h <- weather$`SOLAR RAD(H)`[idx_c:idx_o] %>% cumsum() %>% last()
  l_solar_mjm2 <- weather$`SOLAR RAD(MJ/M2)`[idx_c:idx_o] %>% cumsum() %>% last()
  l_RH <- weather$`RH%`[idx_c:idx_o] %>% mean()
  if (is.na(l_mean_temp)) {
    acu_mean_temp[i] <- "contain_NA"
    rain[i] <- "contain_NA"
    growth_mean_temp[i] <- "contain_NA"
    temp_differ[i] <-  "contain_NA"
    Solar_rad_H[i] <-  "contain_NA"
    Solar_rad_MJM2[i] <-  "contain_NA"
    RH[i] <-  "contain_NA"
  } else {
    temp_differ[i] <- (weather$HIGH[idx_c:idx_o]-weather$LOW[idx_c:idx_o])%>% cumsum() %>% last()
    acu_mean_temp[i] <- l_mean_temp
    rain[i] <- l_rain
    growth_mean_temp[i] <- acu_mean_temp[i]/(length(idx_c:idx_o) - 1)
    Solar_rad_H[i] <-  l_solar_h
    Solar_rad_MJM2[i] <-  l_solar_mjm2
    RH[i] <-  l_RH
  }
}

# 問生長速度
# 總高度H=採取長度l+裁切高度h
# 第1次生長高度=(h1+l1-h0)
# 缺少紀錄的問題，出現斷點即重計
# 
# 問斷點：
# 依照時間建立各品種間的次序
# 根據各品種次序核對本次裁切時間與下一次的前次採切時間一致
# 如果不一致且相差日數小於10天則將下一次的前次裁切時間改為本次裁切時間
#地點告訴我們不能這樣做...，但採摘時間是雷同的，或許當作同一個母體試試?
analyze_data <- tea %>% 
      cbind(.,acu_mean_temp,rain,growth_mean_temp,temp_differ,RH,Solar_rad_H,Solar_rad_MJM2) %>%
      filter(.,acu_mean_temp!=0) %>%
       filter(tea_kind=='fresh leaves') %>% 
       arrange(cultivar,sample_label,observe_ys)

Cont <- vector('logical',length(analyze_data$key))
Cont <- Cont*NA
for (i in 1:length(analyze_data$key))
  {
    if(analyze_data$observe_ys[i]=='108 2nd')
      {
        Cont[i] = FALSE
        next
      }
    else
    {
      if(analyze_data$previous_cutting_date[i]==analyze_data$observe_date[i-1])
      {
        Cont[i] =TRUE
        next
      } 
      if(abs(analyze_data$previous_cutting_date[i]-analyze_data$observe_date[i-1]<10))
        {
          Cont[i] =TRUE
        }
      else
        {
          Cont[i]=FALSE
        }
    }
  }
Cont <- as.logical(Cont)
Growth_length = vector('double', length(analyze_data$key))
for (i in 2:length(analyze_data$key)) {
    Growth_length[i] = Cont[i]*(analyze_data$cutting_height[i]+analyze_data$tea_buds_cm[i]-analyze_data$cutting_height[i-1])
}
analyze_data$Growth_length <- Growth_length
analyze_data <- analyze_data %>%  filter(,level != 'A')
saveRDS(analyze_data,'analyze.RDS')
