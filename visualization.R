library(tidyverse)
library(grid)
library('xaringan')
tea <- readRDS('nonglin_tea.RDS')
tea_and_weather <- readRDS('tea_and_weather.RDS')
number <- tea %>% select(duration_oc,Total_leaf:total_catechins) %>% select(-(G1L:O9L))
dat <- tea_and_weather%>% select(duration_oc,Total_leaf:total_catechins) %>% select(-(G1L:O9L))
ggplot(data=tea,aes(observe_ys,tea_kind,color=cultivar))+
  geom_point()+
  facet_wrap(~cultivar)

ggplot(data=tea,aes(observe_ys,cutting_height,color=cultivar))+
  geom_point()+
  facet_wrap(~cultivar)


ggplot(data=tea,aes(observe_ys,cutting_height,color=cultivar))+
  geom_point()+
  facet_wrap(~cultivar)

ggplot(data=tea,aes(duration_oc,cutting_height,color=cultivar))+
  geom_point()+
  facet_wrap(~cultivar)

ggplot(data=tea,aes(observe_ys,cutting_height,size=duration_oc))+
  geom_point()+
  facet_wrap(~cultivar)

analyze_data %>% 
  filter(Growth_length!=0) %>% 
  ggplot(data=)+ 
  geom_point(aes(x=temp_differ,y=polyphenol))+
  facet_wrap(~cultivar)

ggplot(data = analyze_data)+
  geom_boxplot(aes(x=level,y= polyphenol))

ggplot(data = analyze_data)+
  geom_point(aes(x=acu_mean_temp,y= polyphenol,color = level))+
  facet_wrap(~cultivar)

png('var_hist.png',width = 1000,height=1000)
for (i in 1:17) {
  test <- ggplot(data=number,aes(get(colnames(number[i]))))+
    geom_histogram()+
    labs(x=paste(i,colnames(number[i]),y=NULL))
  print(test,vp=viewport(width=1/4,height=1/5,x=((i-1)%/%5)/4+1/8,y=9/10-((i-1)%%5)/5))
}
dev.off()


# below can merge in my tool box
scatter_hist_plot <- function(dat) {
  n = length(colnames(dat))
  for (i in 1:n) 
    {
     for(j in 1:n)
      {
        if(i==j)
        {
          test <- ggplot(data=dat,aes(get(colnames(dat[i]))))+
            geom_histogram(bins = 30,fill = 'darkgreen')+
            labs(x=paste(i,colnames(dat[i]),y=NULL))
        }
        else
        {
          test <- ggplot(data=dat,aes(get(colnames(dat[i])),get(colnames(dat[j]))),colour='green')+
            geom_point()+
            labs(x=paste(i,colnames(dat[i])),y=paste(j,colnames(dat[j])))
        }
        print(test,vp=viewport(width=1/n, 
                               height=1/n,
                               x=((i-1)%%n)/n+1/(2*n),
                               y=((j-1)%%n)/n+1/(2*n)
                               )
              )
      }
    }
}
png('var_scatter_hist.png',width = 3400,height=3400)
scatter_hist_plot(number2)
dev.off()



ggplot(data=analyze_data)+
  geom_point(mapping = aes(x=rain,y=acu_mean_temp,size=total_catechins,col=buds_weight_100))+
  facet_grid(~cultivar)


# importance ----

#Conditional=True, adjusts for correlations between predictors.
i_scores <- varImp(and_rf, conditional=TRUE)
#Gathering rownames in 'var'  and converting it to the factor
#to provide 'fill' parameter for the bar chart. 
i_scores <- i_scores %>% tibble::rownames_to_column("var") 
i_scores$var<- i_scores$var %>% as.factor()

#Plotting the bar and polar charts for comparing variables
i_bar <- ggplot(data = i_scores) + 
  geom_bar(
    stat = "identity",#it leaves the data without count and bin
    mapping = aes(x = var, y=Overall, fill = var), 
    show.legend = FALSE,
    width = 1
  ) + 
  labs(x = NULL, y = NULL)
i_bar + coord_flip() + theme_minimal()


  