#' Read csvfiles for real estate transaction-price data
#'
#' Reads csvfiles for real estate transaction-price data, which are
#' provied by Ministry of Land, Infrastructure and Transport (MLIT),
#' and Returns a data.frame type data.
#'
#' @param path vector of csvfile's path. If you give more than two
#'        paths as vector, each given data are bound. So you will
#'        get just one data.frame type data.
#'
#' @importFrom data.table fread
#' @export
#'
read_csvfile <- function(path)
{
    dfs <- lapply(path, read.csv, 
                  stringsAsFactors=TRUE, na.strings = c("","NULL"))

    df <- do.call(rbind, dfs)

    df <- df[,-1]

    # データの列名を決定

    #####################################
    # deciding col names
    #####################################
    col_name <- c("\u7a2e\u985e",              # 1  種類
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

    names(df) <- col_name

    return(df)
}

#' Making Building and Land data from real estate transaction-price infomation data
#'
#' Makes Building and Land data from csvfiles for real estate
#' transaction-price data, which are provied by Ministry of Land,
#' Infrastructure and Transport (MLIT)
#'
#' @param path vector of csvfile's path. If you give more than two
#'        paths as vector, each given data are bound. So you will
#'        get just one data.frame type data.
#' @param kind string which contains Four character "R" or "C" or "F" or "U";
#'        each character represents kind of land. There are four kind of land, 
#'        residence, commrercial, factory and unresidence, which represent
#'        "R", "C", "F", "U", respectively.
#' @param timecount logi type. you can see how much time did this function
#'        take to finish work. 
#'
#' @importFrom stringr str_detect
#' @importFrom lubridate ymd
#' @importFrom data.table year
#' @export
#'
get_LBdata <- function(path, timecount = FALSE, kind = "RCFU") {
    
    # kindの初期値は全ての種別

    ###############################
    # counting time 
    ###############################
    t_start <-proc.time()


    ###############################
    # read csvfile 
    ###############################
    df <- read_csvfile(path)


    ###############################
    # choose type
    ###############################
    #df <- subset(df, df[[1]] == "宅地(土地と建物)")
    df <- subset(df, df[[1]] == "\u5b85\u5730(\u571f\u5730\u3068\u5efa\u7269)")


    ###############################
    # choose kind
    ###############################
    df <- subset_with_kind(df, kind)


    ###############################
    # date data
    ###############################
    # 27 -> 取引時点
    t_date <- make_date_col(df[[27]])


    ###############################
    # how far station data
    ###############################
    # 8 -> 駅分
    howfar_st <- make_hfs_col(df[[8]])
    howfar_st_category <- make_hfs_category_col(df[[8]])


    ###############################
    # land size
    ###############################
    # 12 -> 面積 
    land_size <- suppressWarnings(as.numeric(as.character(df[[12]])))
    huge_land <- str_detect(df[[12]], "2000")


    ###############################
    # building size
    ###############################
    # 16 -> 延床面積 
    building_size <- suppressWarnings(as.numeric(as.character(df[[16]])))

    #too_small_fsize <- str_detect(df[[16]],"未満")
    small_building <- str_detect(df[[16]], "\u672a\u6e80")

    #huge_fsize <- str_detect(df[[16]],"以上")
    huge_building <- str_detect(df[[16]], "\u4ee5\u4e0a")


    ###############################
    # How old is the building
    ###############################
    # 17 -> 建築年
    # t_date 取引年
    start <- suppressWarnings(ymd(paste0(conv_jc2ad(df[[17]]), "0101")))
    end <- t_date
    howold_building <- year(end) - year(start)

    #building_before_war <- str_detect(df[[17]], "戦前")
    building_before_war <- str_detect(df[[17]], "\u6226\u524d")


    ###############################
    # bind data and make ans 
    ###############################
    add_df <- data.frame(t_date,
                         howfar_st,
                         howfar_st_category,
                         land_size,
                         huge_land,
                         building_size,
                         small_building,
                         huge_building,
                         howold_building,
                         building_before_war
                         )

    ans <- cbind(df, add_df)
    
    
    ###############################
    # show how much time to have spent 
    ###############################
    t_ans <- proc.time() - t_start
    if(timecount == TRUE){
        print(t_ans)
    }
    
    return(ans)

}

#' Making Land only data from real estate transaction-price infomation data
#'
#' Makes Land only data from csvfiles for real estate transaction-price data,
#' which are provied by Ministry of Land, Infrastructure and Transport (MLIT)
#'
#' @param path vector of csvfile's path. If you give more than two
#'        paths as vector, each given data are bound. So you will
#'        get just one data.frame type data.
#' @param kind string which contains Four character "R" or "C" or "F" or "U";
#'        each character represents kind of land. There are four kind of land, 
#'        residence, commrercial, factory and unresidence, which represent
#'        "R", "C", "F", "U", respectively.
#' @param timecount logi type. you can see how much time did this function
#'        take to finish work. 
#'
#' @importFrom stringr str_detect
#' @importFrom lubridate ymd
#' @importFrom data.table year
#' @export
#'
get_LOdata <- function(path, timecount = FALSE, kind = "RCFU") {
    
    # kindの初期値 全部

    ###############################
    # counting time 
    ###############################
    t_start <-proc.time()


    ###############################
    # read csvfile 
    ###############################
    df <- read_csvfile(path)


    ###############################
    # choose type
    ###############################
    #df <- subset(df, df[[1]] == "宅地(土地)")
    df <- subset(df, df[[1]] == "\u5b85\u5730(\u571f\u5730)")


    ###############################
    # choose kind
    ###############################
    df <- subset_with_kind(df, kind)


    ###############################
    # date data
    ###############################
    # 27 -> 取引時点
    t_date <- make_date_col(df[[27]])


    ###############################
    # how far station data
    ###############################
    # 8 -> 駅分
    howfar_st <- make_hfs_col(df[[8]])
    howfar_st_category <- make_hfs_category_col(df[[8]])


    ###############################
    # land size
    ###############################
    # 12 -> 面積 
    # 9 -> 取引総額
    # 13 -> 土地単価
    land_size <- suppressWarnings(as.numeric(as.character(df[[12]])))
    huge_land <- str_detect(df[[12]], "2000")

    land_size <- ifelse(huge_land, signif( df[[9]] / df[[13]], 2), land_size)

    ###############################
    # bind data and make ans 
    ###############################

    add_df <- data.frame(t_date,
                         howfar_st,
                         howfar_st_category,
                         land_size,
                         huge_land
                         )

    ans <- cbind(df, add_df)
    
    
    ###############################
    # show how much time to have spent 
    ###############################
    t_ans <- proc.time() - t_start
    if(timecount == TRUE){
        print(t_ans)
    }
    
    return(ans)

}

#' Making apartment data from real estate transaction-price infomation data
#'
#' Makes apartment data from csvfiles for real estate
#' transaction-price data, which are provied by Ministry of Land,
#' Infrastructure and Transport (MLIT)
#'
#' @param path vector of csvfile's path. If you give more than two
#'        paths as vector, each given data are bound. So you will
#'        get just one data.frame type data.
#' @param timecount logi type. you can see how much time did this function
#'        take to finish work. 
#'
#' @importFrom stringr str_detect
#' @importFrom lubridate ymd
#' @importFrom data.table year
#' @export
#'
get_Mdata <- function(path, timecount = FALSE) {
    
    ###############################
    # counting time 
    ###############################
    t_start <-proc.time()


    ###############################
    # read csvfile 
    ###############################
    df <- read_csvfile(path)


    ###############################
    # choose type
    ###############################
    #df <- subset(df, df[[1]] == "中古マンション等")
    df <- subset(df, df[[1]] == "\u4e2d\u53e4\u30de\u30f3\u30b7\u30e7\u30f3\u7b49")


    ###############################
    # date data
    ###############################
    # 27 -> 取引時点
    t_date <- make_date_col(df[[27]])


    ###############################
    # how far station data
    ###############################
    # 8 -> 駅分
    howfar_st <- make_hfs_col(df[[8]])
    howfar_st_category <- make_hfs_category_col(df[[8]])


    ###############################
    # area size
    ###############################
    # 12 -> 面積 
    room_size <- suppressWarnings(as.numeric(as.character(df[[12]])))
    huge_room <- str_detect(df[[12]], "2000")


    ###############################
    # How old is the building
    ###############################
    # 17 -> 建築年
    # t_date 取引年
    start <- suppressWarnings(ymd(paste0(conv_jc2ad(df[[17]]), "0101")))
    end <- t_date
    howold_building <- year(end) - year(start)

    #building_before_war <- str_detect(df[[17]], "戦前")
    building_before_war <- str_detect(df[[17]], "\u6226\u524d")


    ###############################
    # bind data and make ans 
    ###############################

    add_df <- data.frame(t_date,
                         howfar_st,
                         howfar_st_category,
                         room_size,
                         huge_room,
                         howold_building,
                         building_before_war
                         )

    ans <- cbind(df, add_df)
    
    
    ###############################
    # show how much time to have spent 
    ###############################
    t_ans <- proc.time() - t_start
    if(timecount == TRUE){
        print(t_ans)
    }
    
    return(ans)

}

#' Making farm and woods data from real estate transaction-price infomation data
#'
#' Makes farm and woods data from csvfiles for real estate
#' transaction-price data, which are provied by Ministry of Land,
#' Infrastructure and Transport (MLIT)
#'
#' @param path vector of csvfile's path. If you give more than two
#'        paths as vector, each given data are bound. So you will
#'        get just one data.frame type data.
#' @param timecount logi type. you can see how much time did this function
#'        take to finish work. 
#'
#' @importFrom stringr str_detect
#' @export
#'
get_FWdata <- function(path, timecount = FALSE) {
    
    ###############################
    # counting time 
    ###############################
    t_start <-proc.time()

    ###############################
    # read csvfile 
    ###############################
    df <- read_csvfile(path)

    ###############################
    # choose type
    ###############################
    #df <- subset(df, df[[1]] == "農地" | df[[1]] == "林地")
    df <- subset(df, df[[1]] == "\u8fb2\u5730" | df[[1]] == "\u6797\u5730")

    ###############################
    # date data
    ###############################
    # 27 -> 取引時点
    t_date <- make_date_col(df[[27]])

    ###############################
    # area size
    ###############################
    # 12 -> 面積 
    land_size <- suppressWarnings(as.numeric(as.character(df[[12]])))
    huge_land <- str_detect(df[[12]], "5000")


    ###############################
    # bind data and make ans 
    ###############################

    add_df <- data.frame(t_date,
                         land_size,
                         huge_land
                         )

    ans <- cbind(df, add_df)
    
    
    ###############################
    # show how much time to have spent 
    ###############################
    t_ans <- proc.time() - t_start
    if(timecount == TRUE){
        print(t_ans)
    }
    
    return(ans)

}

