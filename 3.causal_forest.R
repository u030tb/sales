source("code/2.rain.R")

dataset

# -------------------------------------------------------------------------
# causal forest学習

set.seed(20221201)
df_split <- initial_split(dataset, prop = 0.9, strata = "job")
train_data <- training(df_split)
test_data  <- testing(df_split)

# 
test_data %>% count(gender,age,job)

train_data %>% dim
test_data %>% dim

# causal forest適用

library(grf)

train_X = train_data %>% select(gender,age,job) %>% data.matrix()
train_W = train_data$rainy_flg
train_Y = train_data$use_flg
test_X = test_data %>% select(gender,age,job) %>% data.matrix()
test_W = test_data$rainy_flg
test_Y = test_data$use_flg

cf_model = causal_forest(X = train_X, 
                         Y = train_Y, 
                         W = train_W)

pred_data <- 
  test_data %>% 
  as.data.frame() %>% 
  mutate(hte = predict(cf_model,newdata=test_X)[,1])

# 集計関数
agg_func  <- function(g){
  pred_data %>% 
    group_by(across(all_of(g))) %>% 
    summarise(HTE = mean(hte) %>% round(3)) %>% 
    ungroup() %>% 
    arrange(desc(HTE))
}

agg_func(g=c("gender"))
agg_func(g=c("age"))
agg_func(g=c("job"))
agg_func(g=c("gender","age"))
agg_func(g=c("gender","job"))
agg_func(g=c("age","job"))
agg_func(g=c("gender","age","job"))

pred_data %>% 
  fwrite("data/prep_data.csv",row.names = F)
