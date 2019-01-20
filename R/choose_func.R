#' sub function for extract data
#'
#' @param df A reti data
#' @param favorite vector of favorite data
#' @param index index for df
reti_extract_func <- function(df, favorite, index){
    
    ans <- FALSE

    for(c in favorite){
        pattern <- paste0("^", c)
        ans <- ans | stringr::str_detect(df[[index]], pattern)
    }

    ans <- droplevels(subset(df, ans))

    return(ans)
}


#' filter data by favorite cities
#'
#' This function filters data by city names. This matching is prefix search.
#' And each vector elements is OR condision.
#' 
#' @param df A reti data
#' @param cities vector of favorite city names
#' @export
reti_filter_by_city <- function(df, cities){
    # 市名 -> 5
    reti_extract_func(df, cities, 5)
}


#' filter data by favorite prefectures
#'
#' This function filters data by prefecture names. This matching is
#' prefix search. And each vector elements is OR condision.
#' 
#' @param df A reti data
#' @param prefectures vector of favorite prefecture names
#' @export
reti_filter_by_prefecture <- function(df, prefectures){
    # 県名 -> 4
    reti_extract_func(df, prefectures, 4)
}


#' filter data by favorite station
#'
#' This function filters data by station names. This matching is
#' prefix search. And each vector elements is OR condision.
#' 
#' @param df A reti data
#' @param stations vector of favorite station names
#' @export
reti_filter_by_station <- function(df, stations){
    # 駅名 -> 7
    reti_extract_func(df, stations, 7)
}


#' filter reti data by kind of land
#'
#' This function filters data by kind of land. There are four kind of land; 
#' residence, commrercial, factory and unresidence.
#' 
#' @param df A data which was read from reti csvfile.
#' @param kind String which contains Four character "R" or "C" or "F" or "U";
#'  each character represents kind of land. There are four kind of land, 
#'  residence, commrercial, factory and unresidence, which represent
#'  "R", "C", "F", "U", respectively.
#' @importFrom stringr str_detect
#' @export
reti_filter_by_kind <- function(df, kind){

    tmplogi <- FALSE

    if(str_detect(kind, "R")){
        #tmplogi <- tmplogi or str_detect(df[[2]], "住宅地")
        tmplogi <- tmplogi | str_detect(df[[2]], "\u4f4f\u5b85\u5730")
    }

    if(str_detect(kind, "C")){
        #tmplogi <- tmplogi or str_detect(df[[2]], "商業地")
        tmplogi <- tmplogi | str_detect(df[[2]], "\u5546\u696d\u5730")
    }

    if(str_detect(kind, "F")){
        #tmplogi <- tmplogi or str_detect(df[[2]], "工業地")
        tmplogi <- tmplogi | str_detect(df[[2]], "\u5de5\u696d\u5730")
    } 

    if(str_detect(kind, "U")){
        #tmplogi <- tmplogi or str_detect(df[[2]], "宅地見込地")
        tmplogi <- tmplogi | str_detect(df[[2]], "\u5b85\u5730\u898b\u8fbc\u5730")
    }

    ans <- subset(df, tmplogi)
    return(ans)
}
