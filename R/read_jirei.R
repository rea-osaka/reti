#' Read the jirei data file
#'
#' Read the special jirei data file, which file
#' someone can allow to use.
#'
#' @param file csv file of jirei data
#'
#' @return data.frame
#' @export
reti_read_kjten <- function(file){

  jirei_fd <-

    # ファイルから読み込み
    readr::read_csv(file = file,
                    col_names = jirei_col_name,
                    show_col_types = F,
                    locale = readr::locale(encoding = "cp932")) %>%

    # データの加工
    dplyr::mutate(
      `県コード`        = sprintf("%02d",as.numeric(`県コード`)),
      `市区町村コード`  = sprintf("%02d",as.numeric(`市区町村コード`)),

      `位置座標Ｘ`       = as.numeric(`位置座標Ｘ`),
      `位置座標Ｙ`       = as.numeric(`位置座標Ｙ`),

      `類型区分`         = decode_ruikei(`類型区分`),
      `土地種別`         = decode_syubetu(`土地種別`),

      `形状`             = decode_keijyou(`形状`),

      `接面状況`         = decode_setumen(`接面状況`),
      `前面道路方位`     = decode_houi(`前面道路方位`),
      `接面１方位`       = decode_houi(`接面１方位`),
      `接面２方位`       = decode_houi(`接面２方位`),
      `接面３方位`       = decode_houi(`接面３方位`),
      `最寄駅方位`       = decode_houi(`最寄駅方位`),

      `用途地域`         = decode_youtochiiki(`用途地域`),

      `契約日`           = lubridate::ymd(as.character(`契約日`)),
      `登記原因日`       = lubridate::ymd(as.character(`登記原因日`)),
      t_date             = ifelse(!is.na(`契約日`),
                                  as.character(`契約日`),
                                  as.character(`登記原因日`)),
      t_date             = lubridate::ymd(t_date),

      address_code       = stringr::str_c(`県コード`,`市区町村コード`),

      `事情区分`         = decode_jijyou(`事情区分`),
      `事情有無`         = decode_bool(`事情有無`),
      `買主`             = decode_toujisyazokusei(`買主`),
      `売主`             = decode_toujisyazokusei(`売主`),

      `水道`             = decode_suidou(`水道`),
      `ガス`             = decode_gas(`ガス`),
      `公共下水道`       = decode_gesui(`公共下水道`),

      `前面道路舗装`     = decode_hosou(`前面道路舗装`),
      `接面１舗装`       = decode_hosou(`接面１舗装`),
      `接面２舗装`       = decode_hosou(`接面２舗装`),
      `接面３舗装`       = decode_hosou(`接面３舗装`),

      `前面道路歩道`     = decode_bool(`前面道路歩道`),
      `前面道路系統`     = decode_dourokeitou(`前面道路系統`),


      ) %>%

    dplyr::left_join(local_address_DB, by = "address_code")

  return(jirei_fd)
}

###############################################
# 内部サブルーチン
# デーコード用関数
###############################################

# ありなし
decode_bool <- function(int_vector){

  # デコード定義
  decode_vector <-
    c(
      "無",
      "有"
    )
  sub_decode_func(int_vector, decode_vector)
}

# 舗装
decode_dourokeitou <- function(int_vector){

  # デコード定義
  decode_vector <-
    c(
      "",
      "優",
      "劣",
      "行き止まり",
      "普通",
      "階段"
    )
  sub_decode_func(int_vector, decode_vector)
}

# 舗装
decode_hosou <- function(int_vector){

  # デコード定義
  decode_vector <-
    c(
      "未舗装",
      "アスファルト",
      "コンクリート",
      "他"
    )
  sub_decode_func(int_vector, decode_vector)
}


# 公共下水道
decode_gesui <- function(int_vector){

  # デコード定義
  decode_vector <-
    c(
      "無",
      "処理区域内",
      "処理区域外"
    )
  sub_decode_func(int_vector, decode_vector)
}

# 水道
decode_suidou <- function(int_vector){

  # デコード定義
  decode_vector <-
    c(
      "無",
      "水道",
      "専用水道",
      "引込可"
    )
  sub_decode_func(int_vector, decode_vector)
}

# ガス
decode_gas <- function(int_vector){

  # デコード定義
  decode_vector <-
    c(
      "無",
      "都市ガス",
      "簡易ガス",
      "引込可"
    )
  sub_decode_func(int_vector, decode_vector)
}


# 事情
decode_jijyou <- function(int_vector){

  # デコード定義
  decode_vector <-
    c(
      "その他",
      "買進み",
      "売急ぎ",
      "限定価格"
    )
  sub_decode_func(int_vector, decode_vector)
}


# 当事者属性
decode_toujisyazokusei <- function(int_vector){

  # デコード定義
  decode_vector <-
    c(
      "個人",
      "法人",
      "業者",
      "公共"
    )
  sub_decode_func(int_vector, decode_vector)
}


# 類型
decode_ruikei <- function(int_vector){

  # デコード定義
  decode_vector <-
    c(
      "その他",
      "更地",
      "底地",
      "借地権",
      "建付地",
      "貸家建付地",
      "区分地上権",
      "敷地利用権"
    )
  sub_decode_func(int_vector, decode_vector)
}

# 種別
decode_syubetu <- function(int_vector){

  # デコード定義
  decode_vector <-
    c(
      "その他",               #0
      rep("",39),             #1-39

      "",                     #40 たくみ
      "宅地見込み地",         #41
      rep("",8),

      rep("",10),             #50-59

      "",                     #60 住宅地
      "住宅地（高級）",       #61
      "住宅地（共同）",       #62
      "住宅地（戸建）",       #63
      "住宅地（混在）",       #64
      "住宅地（農家）",       #65
      "住宅地（別荘その他）", #66
      "",
      "",
      "",
      "",                     #70 商業地
      "商業地（高度）",       #71
      "商業地（オフィス街）", #72
      "",
      "商業地（準高度）",     #74
      "商業地（普通）",       #75
      "商業地（近隣）",       #76
      "商業地（路線）",       #77
      "",
      "",
      "",                     #80 工業地
      "工業地（工場）",       #81
      "",
      "",
      "工業地（家内）",       #84
      "工業地（流通業務）",   #85
      ""
    )
  sub_decode_func(int_vector, decode_vector)
}



# 用途地域
decode_youtochiiki <- function(int_vector){

  # デコード定義
  decode_vector <-
    c(
      "記載なし",
      rep("",3),
      "近商",
      "商業",
      "工専",
      "工業",
      "準工",
      "","",
      "１低専","２低専",
      "１中専","２中専",
      "１住居","２住居",
      "準住居","田園"
    )

  sub_decode_func(int_vector, decode_vector)
}

# 方位・駅方位
decode_houi <- function(int_vector){

  # デコード定義
  decode_vector <-
    c(
      "その他",
      "東",
      "南",
      "西",
      "北",
      "南東",
      "南西",
      "北西",
      "北東"
    )

  sub_decode_func(int_vector, decode_vector)
}


# 形状
decode_keijyou <- function(int_vector){

  # デコード定義
  decode_vector <-
    c(
      "記載なし",
      "正方形",
      "ほぼ正方形",
      "長方形",
      "ほぼ長方形",
      "台形",
      "ほぼ台形",
      "不整形",
      "ほぼ整形",
      "袋地等"
    )

  sub_decode_func(int_vector, decode_vector)
}

# 街路接面状況
decode_setumen <- function(int_vector){

  # デコード定義
  decode_vector <-
    c(
      "",
      "角地",
      "準角地",
      "二方路",
      "三方路",
      "四方路",
      "中間画地",
      "無道路地"
    )

  sub_decode_func(int_vector, decode_vector)
}


########################
# デコード用の汎用関数
########################
sub_decode_func <- function(encoded_int, decode_array){

  ans <- ""
  index <- as.integer(encoded_int)

  ans <- ifelse(is.na(index),
                "",
                decode_array[index + 1]
  )

  return(ans)

}

########################
# 事例の列名
########################

jirei_col_name <- c(
  "事例区分",
  "事例番号１",
  "事例番号２",
  "整理番号１",
  "整理番号２",
  "県コード",
  "市区町村コード",
  "分科会番号",
  "評価員番号",
  "公示年",
  "調査年月日",
  "郵便番号",
  "所在地",
  "地番",
  "住居表示",
  "仮換地番号",
  "所有者前",
  "所有者後",
  "土地種別",
  "類型区分",
  "類型その他",
  "現況地目",
  "総額",
  "土地価格",
  "土地単価",
  "更地価格等補正",
  "建物価格",
  "建物単価",
  "その他価格",
  "配分法区分",
  "配分法その他",
  "月額年額区分",
  "地代",
  "借地開始時期区分",
  "借地開始時期年",
  "相続税路線価年度１",
  "相続税路線価１",
  "建築時期年号",
  "建築時期年",
  "建築時期月",
  "構造区分",
  "構造その他",
  "地上",
  "地下",
  "建築面積",
  "延床面積",
  "用途",
  "契約日",
  "登記原因日",
  "事情有無",
  "事情区分",
  "事情限定価格",
  "事情その他",
  "事情補正",
  "時点修正",
  "月率変動率",
  "建付減価スラッシュフラグ",
  "建付減価",
  "標準化補正",
  "街路",
  "交通接近",
  "環境",
  "画地",
  "行政",
  "その他",
  "コード１",
  "文言１",
  "率１",
  "コード２",
  "文言２",
  "率２",
  "コード３",
  "文言３",
  "率３",
  "コード４",
  "文言４",
  "率４",
  "コード５",
  "文言５",
  "率５",
  "コード６",
  "文言６",
  "率６",
  "コード７",
  "文言７",
  "率７",
  "コード８",
  "文言８",
  "率８",
  "コード９",
  "文言９",
  "率９",
  "コード１０",
  "文言１０",
  "率１０",
  "コード１１",
  "文言１１",
  "率１１",
  "コード１２",
  "文言１２",
  "率１２",
  "契約当事者",
  "自己取扱",
  "事例収集源その他",
  "買主",
  "売主",
  "地域特性",
  "前面道路方位",
  "前面道路幅員",
  "前面道路駅前区分",
  "前面道路歩道",
  "前面道路舗装",
  "前面道路種類",
  "前面道路道路名",
  "前面道路系統",
  "前面道路距離",
  "最寄駅路線名",
  "最寄駅駅名",
  "最寄駅方位",
  "最寄駅道路距離",
  "最寄駅直線距離",
  "最寄駅近接区分",
  "最寄バス停留所名",
  "最寄バス方位",
  "最寄バス道路距離",
  "最寄バス直線距離",
  "最寄公共施設名",
  "最寄公共道路距離",
  "最寄商業施設名",
  "最寄商業道路距離",
  "最寄その他施設名",
  "最寄その他道路距離",
  "日照通風",
  "地質地盤地勢",
  "隣接状態",
  "水道",
  "ガス",
  "公共下水道",
  "危険施設状態",
  "危険施設道路距離",
  "規模公簿",
  "規模実測",
  "規模私道",
  "間口",
  "奥行",
  "高低差区分",
  "高低差自",
  "高低差至",
  "傾斜度有無",
  "傾斜度向き",
  "傾斜度",
  "形状",
  "接面状況",
  "接面１方位",
  "接面１幅員",
  "接面１舗装",
  "接面１道路種類",
  "接面２方位",
  "接面２幅員",
  "接面２舗装",
  "接面２道路種類",
  "接面３方位",
  "接面３幅員",
  "接面３舗装",
  "接面３道路種類",
  "セットバック有無",
  "セットバック面積",
  "画地その他",
  "区域区分",
  "用途地域",
  "地域１コード",
  "地域１その他",
  "地域２コード",
  "地域２その他",
  "地域３コード",
  "地域３その他",
  "地域１種",
  "地域１高度区分",
  "地域１高度",
  "地域２種",
  "地域２高度区分",
  "地域２高度",
  "地域３種",
  "地域３高度区分",
  "地域３高度",
  "建ぺい率指定",
  "建ぺい率基準",
  "容積率指定",
  "容積率基準",
  "防火地域",
  "その他特記事項",
  "その他特記事項１の２",
  "その他特記事項１の３",
  "その他特記事項１の４",
  "その他特記事項１の５",
  "特記事項２項目１",
  "特記事項２項目２",
  "特記事項２項目３",
  "特記事項２項目４",
  "特記事項２項目４の２",
  "特記事項２項目４の３",
  "特記事項２項目４の４",
  "相続税路線価年度２",
  "相続税路線価２",
  "相続税路線価年度３",
  "相続税路線価３",
  "測地系区分",
  "座標コード",
  "位置座標Ｘ",
  "位置座標Ｙ"
)




###############################################
# 内部サブルーチン
# 列名の定義
###############################################

sub_name_define <- function(jirei_df){

  ans <-
    jirei_df %>%

    dplyr::rename(
      `事例番号１` = X2,
      `事例番号２` = X3,
      `整理番号１` = X4,
      `整理番号２` = X5,
      `都道府県コード` = X6,
      `市町村コード` = X7,
      `分科会番号` = X8,
      `評価員番号` = X9,
      `公示年度` = X10,
      `調査年月日` = X11,
      `所在` = X13,
      `地番` = X14,
      `住居表示` = X15,
      `売主` = X17,
      `買主` = X18,
      `種別` = X19,
      `類型` = X20,
      `現況地目` = X22,
      `取引価格総額` = X23,
      `土地総額` = X24,
      `土地単価` = X25,
      `更地価格等補正` = X26,
      `建物総額` = X27,
      `建物単価` = X28,
      `相続路線価年度` = X36,
      `相続路線価価格` = X37,
      `建築時点元号` = X38,
      `建築時点年` = X39,
      `建築時点月` = X40,
      `建物構造` = X41,
      `地上階数` = X43,
      `地下階数` = X44,
      `建築面積` = X45,
      `延床面積` = X46,
      `建物用途` = X47,
      `契約日` = X48,
      `登記原因日` = X49,
      `事情` = X50,
      `方位` = X108,
      `幅員` = X109,
      `歩道` = X111,
      `舗装` = X112,
      `街路種類` = X113,
      `街路名称` = X114,
      `系統連続性` = X115,
      `鉄道路線` = X117,
      `駅名` = X118,
      `駅方位` = X119,
      `駅道路距離` = X120,
      `駅直線距離` = X121,
      `土地面積公簿` = X141,
      `土地面積実測` = X142,
      `私道面積` = X143,
      `間口` = X144,
      `奥行` = X145,
      `土地形状` = X152,
      `街路接面状況` = X153,
      `別街路１方位` = X154,
      `別街路１幅員` = X155,
      `別街路１舗装` = X156,
      `別街路１種類` = X157,
      `別街路２方位` = X158,
      `別街路２幅員` = X159,
      `別街路２舗装` = X160,
      `別街路２種類` = X161,
      `別街路３方位` = X162,
      `別街路３幅員` = X163,
      `別街路３舗装` = X164,
      `別街路３種類` = X165,
      `区域区分` = X169,
      `用途地域` = X170,
      `その他行政条件` = X171,
      `建ぺい率主` = X186,
      `建ぺい率基` = X187,
      `容積率主` = X188,
      `容積率基` = X189,
      `防火指定` = X190,
      `左特記事項１` = X191,
      `左特記事項２` = X192,
      `左特記事項３` = X193,
      `左特記事項４` = X194,
      `東経` = X209,
      `北緯` = X210

    )

  return(ans)
}
