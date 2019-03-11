#' making date cols from original date col
#'
#' @param date_strings Vector of transaction date string.
make_date_col <- function(date_strings){

    # df$取引時点
    # 形式は「平成[0-9]{2}年第[１２３４]四半期」

    ad  <- conv_jc2ad(date_strings)
    qtr <- stringr::str_match(date_strings, "\u7b2c(.)\u56db\u534a\u671f")[,2]
    ans <- numeric(length(ad))

    for( i in 1:length(ans)){
        if(qtr[i] == "\uff11"){
            ans[i] <- paste0(ad[i],"0101")
        }
        if(qtr[i] == "\uff12"){
            ans[i] <- paste0(ad[i],"0401")
        }
        if(qtr[i] == "\uff13"){
            ans[i] <- paste0(ad[i],"0701")
        }
        if(qtr[i] == "\uff14"){
            ans[i] <- paste0(ad[i],"1001")
        }
    }

    ans <- lubridate::ymd(ans)
    
    return(ans)
}

#' adding col of howfar_st and howfar_st_category
#'
#' @param df A data which has reti data.
add_cols_station <- function(df){

    hfs_strings <- df[[8]]
    tmp <- as.character(hfs_strings)

    # 30分未満の数値を取り出す処理
    ans <- ifelse(nchar(tmp) > 2, 99, ifelse(tmp == "", NA, tmp))
    hfs <- as.integer(ans)


    # カテゴリ分けする処理
    ans <- tmp

    #ans <- ifelse(ans =="30分?60分", 30, ans)
    ans <- ifelse(ans =="30\u5206?60\u5206", 30, ans)
    ans <- ifelse(ans =="1H?1H30", 60, ans)
    ans <- ifelse(ans =="1H30?2H", 90, ans)
    ans <- ifelse(ans =="2H?", 120, ans)
    ans <- ifelse(ans =="", NA, ans)

    ans <- as.integer(ans)
    ans <- ifelse(0 <= ans & ans < 30, 0, ans)

    hfs_c <- ans


    # 列を追加する
    df <- dplyr::mutate(df,
                        howfar_st = hfs,
                        howfar_st_category = hfs_c
                        )

    return(df)
}

#' adding col of land_size and huge_land
#'
#' @param df A data which has reti data.
#' @param ptn A pattern string which is considered as huge
add_cols_landsize <- function(df, ptn){

    # 12 -> 面積
    df <- dplyr::mutate(df,
                        land_size = suppressWarnings(as.numeric(as.character(df[[12]]))),
                        huge_land = stringr::str_detect(df[[12]], ptn)
                        )

    return(df)
}

#' adding col of howold_building and uilding_before_war
#'
#' @param df A data which has reti data.
add_cols_building <- function(df){

    ###############################
    # How old is the building
    ###############################
    # 17 -> 建築年
    start <- suppressWarnings(lubridate::ymd(paste0(conv_jc2ad(df[[17]]), "0101")))
    end <- df$t_date

    df <- dplyr::mutate(df,
                        howold_building = lubridate::year(end) - lubridate::year(start),
                        building_before_war = stringr::str_detect(df[[17]], "\u6226\u524d") #"戦前"
                        )
    return(df)
}




