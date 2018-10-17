#' making date cols from original date col
#'
#' @param date_strings vector of transaction date string
#'
#' @importFrom stringr str_match
#' @importFrom lubridate ymd
#' @importFrom lubridate days
#' @importFrom lubridate interval

make_date_col <- function(date_strings){

    # df$取引時点
    # 形式は「平成[0-9]{2}年第[１２３４]四半期」

    ad  <- conv_jc2ad(date_strings)
    qtr <- str_match(date_strings, "\u7b2c(.)\u56db\u534a\u671f")[,2]
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

    start <- ymd(ans)
    end <- start + months(3) - days(1)
    
    ans <- interval(start,end) 

    return(ans)
}

#' making how far station cols from original how far col
#'
#' @param date_strings vector of transaction how far string
#'
make_hfs_col <- function(hfs_strings){
    tmp <- as.character(hfs_strings)
    ans <- ifelse(nchar(tmp) > 2, 99, ifelse(tmp == "", NA, tmp))
    return(as.integer(ans))
}

#' making how far station category cols from original how far col
#'
#' @param date_strings vector of transaction how far string
#'
make_hfs_category_col <- function(hfs_strings){
    ans <- as.character(hfs_strings)

    #ans <- ifelse(ans =="30分?60分", 30, ans)
    ans <- ifelse(ans =="30\u5206?60\u5206", 30, ans)
    ans <- ifelse(ans =="1H?1H30", 60, ans)
    ans <- ifelse(ans =="1H30?2H", 90, ans)
    ans <- ifelse(ans =="2H?", 120, ans)
    ans <- ifelse(ans =="", NA, ans)

    ans <- as.integer(ans)
    ans <- ifelse(0 <= ans & ans < 30, 0, ans)

    return(as.integer(ans))
}
