pacman::p_load(
  tidyverse,
  rsample,
  data.table,
  openxlsx,
  glue,
  lubridate
)


# -------------------------------------------------------------------------
# to csv file

# read.xlsx("data/サンプルデータ.xlsx") %>%
#   fwrite("data/sample_data.csv",
#          row.names = F)

# -------------------------------------------------------------------------
# read data
# -------------------------------------------------------------------------

raw_data <- 
  fread("../data/sample_data.csv",
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
         job = '職種') %>% 
  # 60歳がたった1人だったので除外
  filter(!age %in% 50:60) %>% 
  # 20&男&その他がnon-positivity
  filter(job != "その他職種") %>% 
  # 昼食に限定(11:00-15:00)
  # 1500yen以上はアブノーマルかなと。
  filter(hour %in% 11:14,
         price <= 1500) %>% 
  # challenge!!!
  filter(price >= 50)


# haku_idそれ自体を信用してはいけない理由
# 特に"d4dff48"に注意
raw_data %>% 
  select(haku_id,gender,age,job) %>% 
  distinct() %>% 
  count(haku_id) %>% 
  filter(n>1)

# 新しいhaku_id作成
# (1)人材テーブル
HR_table <- 
  raw_data %>% 
  select(haku_id,gender,age,job) %>% 
  distinct() %>% 
  group_by(haku_id) %>% 
  mutate(row = row_number()) %>% 
  ungroup() %>% 
  mutate(haku_id_new = paste(haku_id,row,sep="_"))

# 新しいID付与
raw_data <- 
  raw_data %>% 
  left_join(HR_table,by=c("haku_id","gender","age","job"))

### (2) 購買テーブル
purchase_table <- 
  raw_data %>% 
  select(haku_id_new,date,wday,hour,menu,price,num) %>% 
  group_by(haku_id_new,date,wday,hour,menu) %>% 
  summarise(price = mean(price),
            num = sum(num)) %>% 
  ungroup()

# minusは意味がわからないので
purchase_table %>% 
  filter(num < 0)

# プラスのみに限定
purchase_table <- 
  purchase_table %>% 
  filter(num > 0)

### (3) menuテーブル
# 2018/11/19(月)から値段が変わっているものが。
menu_pre <- 
  purchase_table %>% 
  mutate(cutoff = ifelse(date<as.Date("2018-11-19"),"before","after")) %>% 
  # mutate() %>% 
  select(menu,price,cutoff) %>% 
  distinct() %>% 
  group_by(menu,cutoff) %>% 
  mutate(n = n()) %>% 
  ungroup()

menu_table <- 
  bind_rows(
    # 相違なし
    menu_pre %>% 
      filter(n==1) %>% 
      select(-n),
    
    # 相違あり
    # 平均を計算します。
    menu_pre %>% 
      filter(n>1) %>% 
      group_by(menu,cutoff) %>% 
      summarise(price = mean(price)) %>% 
      ungroup()
  )

# 成果物
purchase_table
HR_table
menu_table

# -------------------------------------------------------------------------
# 気温&降水量data取得
# -------------------------------------------------------------------------

"https://www.data.jma.go.jp/obd/stats/etrn/view/hourly_s1.php?prec_no=44&block_no=47662&year=2018&month=11&day=1&view="

library(rvest)

meteoro_data <-   
  map_dfr(seq(as.Date("2018-11-01"),
              as.Date("2019-01-31"),
              by=1),function(f_date){
                
                paste0(
                  "https://www.data.jma.go.jp/obd/stats/etrn/view/hourly_s1.php?prec_no=44&block_no=47662&year=",
                  year(f_date),
                  "&month=",month(f_date),
                  "&day=",day(f_date),
                  "&view="
                ) %>% 
                  read_html() %>% 
                  html_table(fill=TRUE) %>% 
                  .[[5]] %>% 
                  select('時','降水量(mm)','気温(℃)') %>% 
                  setNames(c("hour","precipitation","temperature")) %>% 
                  slice(2:n()) %>% 
                  mutate(precipitation = str_replace(precipitation,
                                                     pattern = "--",
                                                     replacement = "0")) %>% 
                  mutate(across(.cols=everything(),
                                .fns=~{as.numeric(.x)})) %>% 
                  mutate(date = f_date)
                
              })

# -------------------------------------------------------------------------

# 昼食時の降雨情報取得(hourly)
meteoro_ts_hourly_table <- 
  purchase_table %>% 
  select(date,hour) %>% 
  distinct() %>% 
  arrange(date,hour) %>% 
  left_join(meteoro_data %>% 
              filter(hour %in% 11:14) %>% 
              mutate(rainy_flg = ifelse(precipitation>0,1,0)) %>% 
              select(date,hour,rainy_flg),
            by=c("date","hour"))

# 昼食時の降雨情報取得(daily)
meteoro_ts_daily_table <- 
  meteoro_ts_hourly_table %>% 
  group_by(date) %>% 
  summarise(rainy_flg = max(rainy_flg)) %>% 
  ungroup()

meteoro_ts_daily_table %>% filter(rainy_flg ==1)

# 社食利用フラグuse_flg
use_ts_daily_table <- 
  purchase_table %>% 
  select(haku_id_new,date) %>% 
  distinct() %>% 
  mutate(use_flg = 1)

# モデル投入前のデータセット
# 2065人 * 48日
meteoro_ts_daily_table$date %>% length
HR_table$haku_id_new %>% length

2065*48

dataset <- 
  expand.grid(
    haku_id_new = HR_table$haku_id_new,
    date = meteoro_ts_daily_table$date
  ) %>% 
  left_join(meteoro_ts_daily_table,by="date") %>% 
  left_join(use_ts_daily_table,by=c("haku_id_new","date")) %>% 
  mutate(use_flg = replace_na(use_flg,0)) %>% 
  left_join(HR_table,by="haku_id_new")

