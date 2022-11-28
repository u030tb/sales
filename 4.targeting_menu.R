
# n_purchase：トータルの注文数
# n_use:トータルの使用回数

targeting_data <- 
  purchase_table %>% 
  group_by(haku_id_new,menu) %>% 
  summarise(n_purchase = sum(num)) %>% 
  ungroup() %>% 
  left_join(HR_table,by="haku_id_new") %>% 
  group_by(gender,age,menu) %>% 
  summarise(n_purchase = sum(n_purchase)) %>% 
  ungroup() %>% 
  left_join(
    purchase_table %>% 
      left_join(HR_table,by="haku_id_new") %>% 
      select(haku_id_new,date,gender,age) %>% 
      count(gender,age) %>% 
      rename(n_use = n),
    by = c("gender","age")
  ) %>% 
  mutate(use_prop = n_purchase / n_use) %>% 
  arrange(gender,age,desc(use_prop)) %>% 
  group_by(gender,age) %>% 
  mutate(row_num = row_number()) %>% 
  ungroup() %>% 
  filter(row_num <= 10)


male_20_vec <- 
  targeting_data %>% 
  filter(gender == "男性",
         age == 20) %>% 
  pull(menu)


map(male_20_vec,function(x){
  targeting_data %>% 
    filter(menu == x)
})

# お肉弁当×バランスサラダL

# -------------------------------------------------------------------------
purchase_table %>% 
  group_by(haku_id_new,menu) %>% 
  summarise(n_purchase = sum(num)) %>% 
  ungroup() %>% 
  left_join(HR_table,by="haku_id_new") %>% 
  group_by(gender,age,menu) %>% 
  summarise(n_purchase = sum(n_purchase)) %>% 
  ungroup() %>% 
  left_join(
    purchase_table %>% 
      left_join(HR_table,by="haku_id_new") %>% 
      select(haku_id_new,date,gender,age) %>% 
      count(gender,age) %>% 
      rename(n_use = n),
    by = c("gender","age")
  ) %>% 
  mutate(use_prop = n_purchase / n_use) %>% 
  arrange(gender,age,desc(use_prop)) %>% 
  group_by(gender,age) %>% 
  mutate(row_num = row_number()) %>% 
  ungroup() %>% 
  filter(menu == "バランスサラダＬ")


# 女性      20 お肉弁当 116
# 女性      30 お肉弁当 59
# 女性      40 お肉弁当 44
# 男性      20 お肉弁当 7
# 男性      30 お肉弁当 11
# 男性      40 お肉弁当 12

# 女性      20 バランスサラダＬ 22
# 女性      30 バランスサラダＬ 63
# 女性      40 バランスサラダＬ 13
# 男性      20 バランスサラダＬ 8
# 男性      30 バランスサラダＬ 18
# 男性      40 バランスサラダＬ 14



# お肉弁当注文(480)の中でもバランスサラダＬ注文(2)
purchase_table %>% 
  left_join(
    purchase_table %>% 
      filter(menu == "お肉弁当") %>% 
      select(haku_id_new,date,hour) %>% 
      mutate(oniku_flg = 1),
    by = c("haku_id_new","date","hour")
  ) %>% 
  filter(oniku_flg == 1) %>% 
  mutate(saladL_flg = ifelse(menu == "バランスサラダＬ",1,0)) %>% 
  group_by(haku_id_new,date,hour) %>% 
  summarise(saladL_flg = max(saladL_flg)) %>% 
  ungroup() %>% 
  count(saladL_flg)

# お肉弁当単体しか注文しないのは半分くらい
purchase_table %>% 
  left_join(
    purchase_table %>% 
      filter(menu == "お肉弁当") %>% 
      select(haku_id_new,date,hour) %>% 
      mutate(oniku_flg = 1),
    by = c("haku_id_new","date","hour")
  ) %>% 
  filter(oniku_flg == 1) %>% 
  count(haku_id_new,date,hour) %>% 
  filter(n==1)

# 310円は12/17のみなので、400円でよいでしょう
purchase_table %>% 
  filter(menu == "バランスサラダＬ") %>% 
  select(date,menu,price) %>% 
  distinct() %>% 
  as.data.frame()


purchase_table %>% 
  filter(menu == "バランスサラダＭ") %>% 
  select(date,menu,price) %>% 
  distinct() %>% 
  as.data.frame()

# 結論
# 800円でお肉弁当&バランスサラダLのセットを販売してはどうか
