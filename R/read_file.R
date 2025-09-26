##############################################################################
#
# ファイルの読み込み関数
#
##############################################################################

#' Read csvfiles for real estate transaction price data
#'
#' Reads csvfiles for real estate transaction price data, which are
#' provied by Ministry of Land, Infrastructure and Transport (MLIT),
#'
#' @param path Vector of csvfile's path.
#' @param timecount A logical scalar. you can see how much time did this
#'   function take to finish the work.
#' @return A tibble data.
#' @export
reti_read_base <- function(path, timecount = TRUE)
{
    # 時間計測用処理
    t_start <-proc.time()

    #############################################################
    # 列名の決定はここで行う
    #############################################################
    my_col_name <- c(
                  #"dummy_num",  フォーマット替えで無くなった
                  "\u7a2e\u985e",              # 1  種類
                  "price_category",            # 1-1価格情報区分
                  "\u5730\u57df",              # 2  地域
                  "code",                      # 3  code
                  "\u770c\u540d",              # 4  県名
                  "\u5e02\u540d",              # 5  市名
                  "\u5730\u533a\u540d",        # 6  地区名
                  "\u99c5\u540d",              # 7  駅名
                  "\u99c5\u5206",              # 8  駅分
                  "\u53d6\u5f15\u7dcf\u984d",  # 9  取引総額
                  "\u576a\u5358\u4fa1",        # 10 坪単価
                  "\u9593\u53d6\u308a",        # 11 間取り
                  "\u9762\u7a4d",              # 12 面積
                  "\u571f\u5730\u5358\u4fa1",  # 13 土地単価
                  "\u571f\u5730\u5f62\u72b6",  # 14 土地形状
                  "\u9593\u53e3",              # 15 間口
                  "\u5ef6\u5e8a\u9762\u7a4d",  # 16 延床面積
                  "\u5efa\u7bc9\u5e74",        # 17 建築年
                  "\u5efa\u7269\u69cb\u9020",  # 18 建物構造
                  "\u5efa\u7269\u7528\u9014",  # 19 建物用途
                  "\u4eca\u5f8c",              # 20 今後
                  "\u9053\u8def\u65b9\u4f4d",  # 21 道路方位
                  "\u9053\u8def\u7a2e\u985e",  # 22 道路種類
                  "\u9053\u8def\u5e45\u54e1",  # 23 道路幅員
                  "\u90fd\u5e02\u8a08\u753b",  # 24 都市計画
                  "\u5efa\u853d\u7387",        # 25 建蔽率
                  "\u5bb9\u7a4d\u7387",        # 26 容積率
                  "\u53d6\u5f15\u6642\u70b9",  # 27 取引時点
                  "\u6539\u88c5",              # 28 改装
                  "\u53d6\u5f15\u4e8b\u60c5")  # 29 取引事情

    #############################################################
    # 列のタイプの決定はここで行う
    #############################################################
    my_col_type <- readr::cols(#"c", # 仮の番号
                            "f", # 1  種類
                            "f", # 1-1  価格情報区分
                            "f", # 2  地域
                            "i", # 3  code
                            "f", # 4  県名
                            "f", # 5  市名
                            "f", # 6  地区名
                            "f", # 7  駅名
                            "f", # 8  駅分
                            "n", # 9  取引総額
                            "i", # 10 坪単価
                            "f", # 11 間取り
                            "f", # 12 面積
                            "i", # 13 土地単価
                            "f", # 14 土地形状
                            "f", # 15 間口
                            "f", # 16 延床面積
                            "f", # 17 建築年
                            "f", # 18 建物構造
                            "f", # 19 建物用途
                            "f", # 20 今後
                            "f", # 21 道路方位
                            "f", # 22 道路種類
                            "n", # 23 道路幅員
                            "f", # 24 都市計画
                            "i", # 25 建蔽率
                            "i", # 26 容積率
                            "f", # 27 取引時点
                            "f", # 28 改装
                            "f"  # 29 取引事情
                            )

    #############################################################
    # csvファイルを読み込む
    #############################################################

    dfs <- lapply(path,
                  readr::read_csv,
                  skip = 1,
                  col_names = my_col_name,
                  col_types = my_col_type,
                  locale = readr::locale(encoding = "cp932"))
    df <- dplyr::bind_rows(dfs)
    df$`price_category` <- NULL
    #df <- df[,-1] #１列目は外す

    #############################################################
    # 類型がNAの列は初めから外す（栃木のエラーデータ対策）
    #############################################################

    df <- subset(df, !is.na(df[[1]]))

    #############################################################
    # 取引時点を扱いやすい形にする
    #############################################################

    tmptdata <- make_date_col(df[[27]])
    # 27 -> 取引時点
    df <- dplyr::mutate(df,
                        t_date = tmptdata,
                        t_year = lubridate::year(tmptdata),
                        t_qtr  = lubridate::quarter(tmptdata)
                        )

    #############################################################
    # show how much time to have spent
    #############################################################
    if(timecount == TRUE){
        print(proc.time() - t_start)
    }

    return(df)
}


#' Making Building and Land data from reti csvfile
#'
#' This function Reads csvfiles for real estate transaction price data,
#' which are provied by Ministry of Land, Infrastructure and Transport (MLIT),
#' and  makes building and land data.
#'
#' @inheritParams reti_read_base
#' @inheritParams reti_filter_by_kind
#' @return A tibble data.
#' @export
reti_read_LB <- function(path, timecount = FALSE, kind = "RCFU") {

    # kindの初期値は全ての種別

    # 時間計測用処理
    t_start <-proc.time()

    # read csvfile
    df <- reti_read_base(path, timecount = F)

    # choose type
    #df <- subset(df, df[[1]] == "宅地(土地と建物)")
    df <- subset(df, df[[1]] == "\u5b85\u5730(\u571f\u5730\u3068\u5efa\u7269)")
    #df <- dplyr::filter(df, !! names(df)[1]  == "\u5b85\u5730(\u571f\u5730\u3068\u5efa\u7269)")

    # choose kind
    df <- reti_filter_by_kind(df, kind)

    # how far station data
    df <- add_cols_station(df)

    # land size data
    df <- add_cols_landsize(df, "2000")

    # building size
    # 16 -> 延床面積
    df <- dplyr::mutate(df,
                        building_size = suppressWarnings(as.numeric(as.character(df[[16]]))),
                        small_building = str_detect(df[[16]], "\u672a\u6e80"), #"未満"
                        huge_building = str_detect(df[[16]], "\u4ee5\u4e0a")   #"以上"
                        )

    # How old is the building
    df <- add_cols_building(df)


    #############################################################
    # show how much time to have spent
    #############################################################
    if(timecount == TRUE){
        print(proc.time() - t_start)
    }

    return(df)

}

#' Making Land only data from reti csvfile
#'
#' This function Reads csvfiles for real estate transaction price data,
#' which are provied by Ministry of Land, Infrastructure and Transport (MLIT),
#' and  makes land only data.
#'
#' @inheritParams reti_read_base
#' @inheritParams reti_filter_by_kind
#' @return A tibble data.
#' @export
reti_read_LO <- function(path, timecount = FALSE, kind = "RCFU") {

    # kindの初期値は全ての種別

    # 時間計測用処理
    t_start <-proc.time()

    # read csvfile
    df <- reti_read_base(path, timecount = F)

    # choose type
    #df <- subset(df, df[[1]] == "宅地(土地)")
    df <- subset(df, df[[1]] == "\u5b85\u5730(\u571f\u5730)")
    #df <- dplyr::filter(df, !! names(df)[1]  == "\u5b85\u5730(\u571f\u5730)")

    # choose kind
    df <- reti_filter_by_kind(df, kind)

    # how far station data
    df <- add_cols_station(df)

    # land size data
    df <- add_cols_landsize(df, "2000")

    # 大規模画地について総額と単価から面積を推測
    df$land_size <- ifelse(df$huge_land, signif( df[[9]] / df[[13]], 2), df$land_size)


    #############################################################
    # show how much time to have spent
    #############################################################
    if(timecount == TRUE){
        print(proc.time() - t_start)
    }

    return(df)

}

#' Making apartment data from reti csvfile
#'
#' This function Reads csvfiles for real estate transaction price data,
#' which are provied by Ministry of Land, Infrastructure and Transport (MLIT),
#' and  makes apartment data.
#'
#' @inheritParams reti_read_base
#' @return A tibble data.
#' @export
reti_read_MS <- function(path, timecount = FALSE) {


    # kindの初期値は全ての種別

    # 時間計測用処理
    t_start <-proc.time()

    # read csvfile
    df <- reti_read_base(path, timecount = F)

    # choose type
    #df <- subset(df, df[[1]] == "中古マンション等")
    df <- subset(df, df[[1]] == "\u4e2d\u53e4\u30de\u30f3\u30b7\u30e7\u30f3\u7b49")
    #df <- dplyr::filter(df, !! names(df)[1]  == "\u4e2d\u53e4\u30de\u30f3\u30b7\u30e7\u30f3\u7b49")

    # how far station data
    df <- add_cols_station(df)


    # room size
    # 12 -> 面積
    df <- dplyr::mutate(df,
                        room_size = suppressWarnings(as.numeric(as.character(df[[12]]))),
                        huge_room = stringr::str_detect(df[[12]], "2000")
                        )


    # How old is the building
    df <- add_cols_building(df)


    #############################################################
    # show how much time to have spent
    #############################################################
    if(timecount == TRUE){
        print(proc.time() - t_start)
    }

    return(df)

}

#' Making farm and woods data from reti csvfile
#'
#' This function Reads csvfiles for real estate transaction price data,
#' which are provied by Ministry of Land, Infrastructure and Transport (MLIT),
#' and  makes farm and woods data.
#'
#' @inheritParams reti_read_base
#' @return A tibble data.
#' @export
reti_read_FW <- function(path, timecount = FALSE) {

    # kindの初期値は全ての種別

    # 時間計測用処理
    t_start <-proc.time()

    # read csvfile
    df <- reti_read_base(path, timecount = F)

    # choose type
    #df <- subset(df, df[[1]] == "農地" | df[[1]] == "林地")
    df <- subset(df, df[[1]] == "\u8fb2\u5730" | df[[1]] == "\u6797\u5730")
    #df <- dplyr::filter(df,
    #                    !! names(df)[1]  == "\u8fb2\u5730" |
    #                    !! names(df)[1]  == "\u6797\u5730"
    #                )

    # land size data
    df <- add_cols_landsize(df, "5000")


    #############################################################
    # show how much time to have spent
    #############################################################
    if(timecount == TRUE){
        print(proc.time() - t_start)
    }

    return(df)
}

##############################################################################
#
# 古い関数
#
##############################################################################

#' @rdname reti_read_base
#' @export
read_csvfile <- reti_read_base

#' @rdname reti_read_LB
#' @export
get_LBdata <- reti_read_LB


#' @rdname reti_read_LO
#' @export
get_LOdata <- reti_read_LO


#' @rdname reti_read_MS
#' @export
get_Mdata <- reti_read_MS


#' @rdname reti_read_FW
#' @export
get_FWdata <- reti_read_FW


