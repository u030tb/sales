
# -------------------------------------------------------------------------
# 雨の日の利用者は？
# -------------------------------------------------------------------------

rainy_male20_vec <- 
  purchase_table %>% 
  left_join(meteoro_ts_daily_table,by="date") %>% 
  left_join(HR_table,by="haku_id_new") %>% 
  filter(gender == "男性",
         age == 20) %>%
  select(haku_id_new,date,rainy_flg) %>% 
  distinct() %>% 
  count(haku_id_new,rainy_flg) %>% 
  pivot_wider(id_cols = haku_id_new,
              names_from = rainy_flg,
              names_prefix = "rain",
              values_from = n) %>% 
  filter(is.na(rain0)) %>%
  # filter(!is.na(rain1) & (is.na(rain0) | rain0 <= 3)) %>%
  pull(haku_id_new)


map(rainy_male20_vec,function(x){
  purchase_table %>% 
    left_join(meteoro_ts_daily_table,by="date") %>% 
    filter(haku_id_new %in% rainy_male20_vec) %>% 
    filter(rainy_flg == 1) %>%
    as.data.frame() %>% 
    filter(haku_id_new == x)
})

rainy_male20_old_vec <- str_remove(rainy_male20_vec,"_1")

# -------------------------------------------------------------------------
raw_data2 <- 
  fread("data/sample_data.csv",
        data.table = F) %>% 
  mutate(time = ymd_hms(glue("{年}-{月}-{日} {時}:{分}:{秒}"))) %>% 
  mutate(date = date(time)) %>% 
  select(haku_id,
         
         time,
         date,
         hour = '時',
         wday = '曜日',
         menu = 'メニュー',
         price = '価格',
         num = '個数',
         gender = '性別',
         age = '年齢',
         job = '職種')

map(rainy_male20_old_vec,function(x){
  raw_data2 %>% 
    filter(haku_id %in% x) %>% 
    select(-date,-hour)
})


# -------------------------------------------------------------------------

new_user_vec <- 
  purchase_table %>% 
  mutate(before_1206_flg= ifelse(date < ymd("2018-12-06"),1,0),
         use_1206_flg   = ifelse(date == ymd("2018-12-06"),1,0),
         after_1206_flg = ifelse(date > ymd("2018-12-06"),1,0)) %>% 
  group_by(haku_id_new) %>% 
  summarise(before_1206_flg = max(before_1206_flg),
            use_1206_flg = max(use_1206_flg),
            after_1206_flg = max(after_1206_flg)) %>% 
  ungroup() %>% 
  filter(before_1206_flg == 0,
         use_1206_flg == 1,
         after_1206_flg == 1) %>% 
  left_join(HR_table,by="haku_id_new") %>% 
  filter(gender=="男性",
         age=="20") %>% 
  pull(haku_id_new)


# 全員_1なので大丈夫
new_user_old_vec <- str_remove(new_user_vec,"_1")

map(new_user_old_vec,function(x){
  raw_data2 %>% 
    filter(haku_id == x)
})

# -------------------------------------------------------------------------

diff_HR_table <- 
  HR_table %>% 
  filter(haku_id %in% c(rainy_male20_old_vec,new_user_old_vec)) %>% 
  mutate(diff = ifelse(haku_id %in% rainy_male20_old_vec,
                       "only_rain","after_rain"))

map(diff_HR_table$haku_id_new,function(x){
  purchase_table %>% 
    filter(haku_id_new %in% x) %>% 
    left_join(diff_HR_table,
              by = "haku_id_new")
})

purchase_table %>% 
  mutate(before_1206_flg= ifelse(between(date,ymd("2018-11-22"),ymd("2018-12-05")),1,0),
         use_1206_flg   = ifelse(date == ymd("2018-12-06"),1,0),
         after_1206_flg = ifelse(between(date,ymd("2018-12-07"),ymd("2018-12-13")),1,0)) %>% 
  group_by(haku_id_new) %>% 
  summarise(before_1206_flg = max(before_1206_flg),
            use_1206_flg = max(use_1206_flg),
            after_1206_flg = max(after_1206_flg)) %>% 
  ungroup() %>% 
  filter(before_1206_flg == 0,
         use_1206_flg == 1) %>% 
  left_join(HR_table,by="haku_id_new") %>% 
  count(gender,age,after_1206_flg) %>% 
  pivot_wider(id_cols = c(gender,age),
              names_from = after_1206_flg,
              values_from = n,
              names_prefix = "after1206_") %>% 
  mutate(prop = round(after1206_1 / (after1206_0 + after1206_1)*100,1))


