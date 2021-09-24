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
                    col_names = F,
                    show_col_types = F,
                    locale = readr::locale(encoding = "cp932")) %>%

    # 可読性のある名前にリネーム
    sub_name_define()

  jirei_fd$`都道府県コード` = sprintf("%02d",as.numeric(jirei_fd$`都道府県コード`))
  jirei_fd$`市町村コード` = sprintf("%03d",as.numeric(jirei_fd$`市町村コード`))


  jirei_fd$`東経` = as.numeric(jirei_fd$`東経`)/10000000
  jirei_fd$`北緯` = as.numeric(jirei_fd$`北緯`)/10000000

  # デコード
  jirei_fd$`類型` <- decode_ruikei(jirei_fd$`類型`)
  jirei_fd$`種別` <- decode_syubetu(jirei_fd$`種別`)

  jirei_fd$`方位` <- decode_houi(jirei_fd$`方位`)
  jirei_fd$`別街路１方位` <- decode_houi(jirei_fd$`別街路１方位`)
  jirei_fd$`別街路２方位` <- decode_houi(jirei_fd$`別街路２方位`)
  jirei_fd$`別街路３方位` <- decode_houi(jirei_fd$`別街路３方位`)
  jirei_fd$`駅方位` <- decode_houi(jirei_fd$`駅方位`)

  jirei_fd$`土地形状` <- decode_keijyou(jirei_fd$`土地形状`)
  jirei_fd$`街路接面状況` <- decode_setumen(jirei_fd$`街路接面状況`)
  jirei_fd$`用途地域` <- decode_youtochiiki(jirei_fd$`用途地域`)

  # 役に立つ行のみのフィルタリング
  ans_jirei <-
    jirei_fd %>%
    dplyr::select(-matches("X\\d+")) %>%

  # 追加する列
    dplyr::mutate(
      #取引時点相当日の確定
      t_date = ifelse(is.na(`契約日`) | `契約日` == "",
                      `登記原因日`,
                      `契約日`) %>% lubridate::ymd(),
      address_code = stringr::str_c(`都道府県コード`,`市町村コード`)
      ) %>%

    # 都道府県、市町村を文字列化
    dplyr::left_join(local_address_DB, by = "address_code")


  return(ans_jirei)
}

###############################################
# 内部サブルーチン
# デーコード用関数
###############################################

# 類型
decode_ruikei <- function(int_vector){

  # デコード定義
  decode_vector <-
    c(
      "その他",
      "更地",
      "底地",
      "",
      "建付地",
      "貸家建付地"
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
