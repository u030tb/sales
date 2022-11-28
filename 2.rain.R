source("code/1.preprocessing.R")

# -------------------------------------------------------------------------

dataset
dataset %>% 
  count(rainy_flg,use_flg) %>% 
  pivot_wider(id_cols = rainy_flg,
              names_from = use_flg,
              values_from = n,
              names_prefix = "use") %>% 
  mutate(use_prop = use1/(use0+use1))

# 雨の日の方が使用率高い

# 雨：使用率19.4%
# 晴：使用率16.4%


# -------------------------------------------------------------------------

dataset %>% 
  count(gender,age,rainy_flg,use_flg) %>% 
  pivot_wider(id_cols = c(gender,age,rainy_flg),
              names_from = use_flg,
              values_from = n,
              names_prefix = "use") %>% 
  mutate(use_prop = use1/(use0+use1)) %>% 
  select(-use0,-use1) %>% 
  pivot_wider(id_cols = c(gender,age),
              names_from = rainy_flg,
              values_from = use_prop,
              names_prefix = "rain") %>% 
  mutate(diff = rain1 - rain0)


### 結果
# 女性の方が晴れの日も社食利用多い

### 妄察
# 女性は雨の日でも外食を継続
# 男性は雨の日の行動変容強い

# 女性は社食利用層と、外食固辞層がはっきり分かれている？
