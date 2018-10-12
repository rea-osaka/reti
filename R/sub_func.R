#' making date cols from original date col
#' 
#' @param df data.frame data which is inported by csv_read(), or etc. 
#' @importFrom stringr str_match

add_data_cols <- function(df){

    # df$取引時点
    # 形式は「平成[0-9]{2}年第[１２３４]四半期」

    # ad
    ad <- conv_jc2ad(df[[27]])

}
