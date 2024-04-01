library(tidyverse)

df = read.csv("ward_cluster.csv")
dim(df)
items = colnames(df)[c(10:35,37)] # 修正熱量、飽和脂肪

# geom_boxplot
for (i in items){
  df %>%
    gather(key = item, value = values, items) %>%
    filter(item==i) %>%
    ggplot(aes(x = factor(0), y = values))+
    geom_boxplot()+
    facet_wrap(~CLUSTER, ncol = 5)+
    labs(title = i)+
    theme(text = element_text(family = "BiauKaiTC"))+
    scale_x_discrete(breaks = NULL) +
    xlab(NULL)
  ggsave(paste("eda_boxplot/",i,".png",sep = ""), width = 5, height = 3)
}

# geom_violins
for (i in items){
  df %>%
    gather(key = item, value = values, items) %>%
    filter(item==i) %>%
    ggplot(aes(x = factor(0), y = values))+
    geom_violin()+
    geom_boxplot(width = 0.1)+
    facet_wrap(~CLUSTER, ncol = 5)+
    labs(title = i)+
    theme(text = element_text(family = "BiauKaiTC"))+
    scale_x_discrete(breaks = NULL) +
    xlab(NULL)
  ggsave(paste("eda_violin/",i,".png",sep = ""), width = 5, height = 3)
}

# 剖面圖
group_mean = df[,-c(9,36)] %>%
  gather(key = item, value = values, items) %>%
  select(4,5,9,10) %>%
  group_by(CLUSTER,item) %>%
  summarise(ave = mean(values)) %>%
  spread(key = item, value = ave)
anova(group_mean)#?
  
ggplot(aes(x = item, y = ave))+
  geom_line(aes(color = CLUSTER))+
  labs(title = "各群體營養成分平均剖面圖", y = "平均數")+
  theme(text = element_text(family = "BiauKaiTC"))




