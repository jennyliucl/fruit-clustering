library(tidyverse)

# 食物根據維生素與礦物質分群，期待：營養成分高與低的食物被分開

df = read.csv("ingredient_raw.csv")
df$分析項分類 %>% unique()

# 篩選維生素與礦物值
check = df %>%
  filter(分析項分類=="維生素A"|
           分析項分類=="維生素B群 & C"|
           分析項分類=="維生素D"|
           分析項分類=="維生素E"|
           分析項分類=="維生素K"|
           分析項分類=="礦物質")

colnames(check)

# 寬表格（含單位）
check2 = check %>%
  mutate(values = ifelse(is.na(每單位含量),NA,paste(每單位含量, "-", 含量單位, sep = ""))) %>%
  select(1:8, 10, 18) %>%
  spread(key = 分析項, value = values)

colnames(check2) # item in 9:42

# 檢查每個成分單位
for (i in 9:42){
  print(colnames(check2)[i])
  split_result = check2[,i] %>% str_split(pattern = "-", simplify = TRUE)
  split_result[,2] %>% unique() %>% print()
}

# final
all = check %>%
  select(1:8, 10, 15) %>%
  mutate(values = ifelse(is.na(每單位含量), 0, 每單位含量)) %>%
  select(1:9, 11) %>%
  spread(key = 分析項, value = values)

dim(all) # 2161   42

write.csv(all, "ingredient_df.csv", row.names = F, fileEncoding = "UTF-8")

# 檢查有無重複數值
all$樣品名稱[duplicated(all$樣品名稱)]
all[all$樣品名稱=="無花果",] %>% View()

all$食品分類 %>% unique()

# all
all_new = df %>%
  select(1:8, 10, 15) %>%
  mutate(values = ifelse(is.na(每單位含量), 0, 每單位含量)) %>%
  select(1:9, 11) %>%
  spread(key = 分析項, value = values)

write.csv(all_new, "ingredient_spread.csv", row.names = F, fileEncoding = "UTF-8")





