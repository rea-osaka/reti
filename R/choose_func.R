#' sub function for extract data
#'
#' @param df source data.frame
#' @param favorite vector of favorite data
#' @param index index for df
#'
#' @importFrom stringr str_detect
#'
reti_extract_func <- function(df, favorite, index){
    
    ans <- FALSE

    for(c in favorite){
        pattern <- paste0("^", c)
        ans <- ans | str_detect(df[[index]], pattern)
    }

    ans <- droplevels(subset(df, ans))

    return(ans)
}


#' extract data for favorite cities
#'
#' @param df source data.frame
#' @param cities vector of favorite city names
#'
reti_extract_by_city <- function(df, cities){
    # 市名 -> 5
    reti_extract_func(df, cities, 5)
}

#' extract data for favorite prefectures
#'
#' @param df source data.frame
#' @param prefectures vector of favorite prefecture names
#'
reti_extract_by_prefecture <- function(df, prefectures){
    # 県名 -> 4
    reti_extract_func(df, prefectures, 4)
}

#' extract data for favorite station
#'
#' @param df source data.frame
#' @param station vector of favorite station names
#'
reti_extract_by_station <- function(df, stations){
    # 駅名 -> 7
    reti_extract_func(df, stations, 7)
}
