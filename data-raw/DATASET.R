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
  "03","\u5ca9\u624b\u770c",#"岩手県",
  "04","\u5bae\u5d0e\u770c",#"宮城県",
  "05","\u79cb\u7530\u770c",#"秋田県",
  "06","\u5c71\u5f62\u770c",#"山形県",
  "07","\u798f\u5cf6\u770c",#"福島県",
  "08","\u8328\u57ce\u770c",#"茨城県",
  "09","\u6803\u6728\u770c",#"栃木県",
  "10","\u7fa4\u99ac\u770c",#"群馬県",
  "11","\u57fc\u7389\u770c",#"埼玉県",
  "12","\u5343\u8449\u770c",#"千葉県",
  "13","\u6771\u4eac\u90fd",#"東京都",
  "14","\u795e\u5948\u5ddd\u770c",#"神奈川県",
  "15","\u65b0\u6f5f\u770c",#"新潟県",
  "16","\u5bcc\u5c71\u770c",#"富山県",
  "17","\u77f3\u5ddd\u770c",#"石川県",
  "18","\u798f\u4e95\u770c",#"福井県",
  "19","\u5c71\u68a8\u770c",#"山梨県",
  "20","\u9577\u91ce\u770c",#"長野県",
  "21","\u5c90\u961c\u770c",#"岐阜県",
  "22","\u9759\u5ca1\u770c",#"静岡県",
  "23","\u611b\u77e5\u770c",#"愛知県",
  "24","\u4e09\u91cd\u770c",#"三重県",
  "25","\u6ecb\u8cc0\u770c",#"滋賀県",
  "26","\u4eac\u90fd\u5e9c",#"京都府",
  "27","\u5927\u962a\u5e9c",#"大阪府",
  "28","\u5175\u5eab\u770c",#"兵庫県",
  "29","\u5948\u826f\u770c",#"奈良県",
  "30","\u548c\u6b4c\u5c71\u770c",#"和歌山県",
  "31","\u9ce5\u53d6\u770c",#"鳥取県",
  "32","\u5cf6\u6839\u770c",#"島根県",
  "33","\u5ca1\u5c71\u770c",#"岡山県",
  "34","\u5e83\u5cf6\u770c",#"広島県",
  "35","\u5c71\u53e3\u770c",#"山口県",
  "36","\u5fb3\u5cf6\u770c",#"徳島県",
  "37","\u9999\u5ddd\u770c",#"香川県",
  "38","\u611b\u5a9b\u770c",#"愛媛県",
  "39","\u9ad8\u77e5\u770c",#"高知県",
  "40","\u798f\u5ca1\u770c",#"福岡県",
  "41","\u4f50\u8cc0\u770c",#"佐賀県",
  "42","\u9577\u5d0e\u770c",#"長崎県",
  "43","\u718a\u672c\u770c",#"熊本県",
  "44","\u5927\u5206\u770c",#"大分県",
  "45","\u5bae\u5d0e\u770c",#"宮崎県",
  "46","\u9e7f\u5150\u5cf6\u770c",#"鹿児島県",
  "47","\u6c96\u7e04\u770c") #"沖縄県"

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
