## code to prepare `DATASET` dataset goes here
library(tidyverse)

# ファイルは総務省の全国地方公共団体コードのページからダウンロード
# https://www.soumu.go.jp/denshijiti/code.html

# 「都道府県コード及び市区町村コード」
currentcode_path <-  "data-raw/000730858.xlsx"

# 「都道府県コード及び市区町村コード」改正一覧表
oldcode_path <- "data-raw/000562731.xls"

# 市区町村
city01 <-
  readxl::read_excel(currentcode_path, sheet = 1) %>%
  select(c(1,3)) %>%
  `names<-`(c("code","city_name"))

# 政令指定都市
city02 <-
  readxl::read_excel(currentcode_path,sheet = 2) %>%
  select(c(1,3)) %>%
  `names<-`(c("code","city_name"))

# 廃止されたもの
city_old <-
  readxl::read_excel(oldcode_path,
                     sheet = 1,
                     skip = 4,
                     col_names = F) %>%
  dplyr::select(c(6,7)) %>%
  `names<-`(c("code","city_name"))

# 都道府県コード DB
pref_db <- tibble::tribble(
  ~pref_code, ~pref_name,
  "01","\u5317\u6d77\u9053",#"北海道",
  "02","\u9752\u68ee\u770c",#"青森県",
  "03","岩手県",
  "04","宮城県",
  "05","秋田県",
  "06","山形県",
  "07","福島県",
  "08","茨城県",
  "09","栃木県",
  "10","群馬県",
  "11","埼玉県",
  "12","千葉県",
  "13","東京都",
  "14","神奈川県",
  "15","新潟県",
  "16","富山県",
  "17","石川県",
  "18","福井県",
  "19","山梨県",
  "20","長野県",
  "21","岐阜県",
  "22","静岡県",
  "23","愛知県",
  "24","三重県",
  "25","滋賀県",
  "26","京都府",
  "27","大阪府",
  "28","兵庫県",
  "29","奈良県",
  "30","和歌山県",
  "31","鳥取県",
  "32","島根県",
  "33","岡山県",
  "34","広島県",
  "35","山口県",
  "36","徳島県",
  "37","香川県",
  "38","愛媛県",
  "39","高知県",
  "40","福岡県",
  "41","佐賀県",
  "42","長崎県",
  "43","熊本県",
  "44","大分県",
  "45","宮崎県",
  "46","鹿児島県",
  "47","沖縄県")

local_address_DB <-
  dplyr::bind_rows(city01,city02,city_old) %>%
  dplyr::filter(!is.na(city_name)) %>%
  unique() %>%
  mutate(address_code = stringr::str_sub(code, 1,5),
         pref_code = stringr::str_sub(code, 1,2),
         city_code = stringr::str_sub(code,3,5)) %>%
  left_join(pref_db, by = c( "pref_code" = "pref_code")) %>%
  select(address_code,
         pref_name,
         city_name)

usethis::use_data(local_address_DB,internal = T, overwrite = TRUE)
