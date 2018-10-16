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
