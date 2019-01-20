#' Convert Japan Calendar to AD
#'
#' @param gengou_year gengou is strings in multibite charactor
#'      which represents year name of Japanese Calender
#' @importFrom magrittr %>%
#' @return integer or NA if can't find proper gengou, this function return NA
conv_jc2ad <- function(gengou_year){

    ###################################################
    # if gengou is added, you can add a function here
    ###################################################
    syouwa2ad <- jc2ad_func("\u662d\u548c",1926)
    heisei2ad <- jc2ad_func("\u5e73\u6210",1989)

    ans <- gengou_year %>% syouwa2ad %>% heisei2ad

    # if cant conv, I puts NA
    tmp <- stringr::str_match(ans, "^\\d+$")
    ans <- ifelse(is.na(tmp[,1]), NA, as.integer(tmp[,1]))

    return(ans)
}

#' Convert Japan Calendar to AD sub function
#'
#' @param gengou gengou is strings in multibite charactor which represents
#'    year name of Japanese Calender
#' @param first_year what AD year a first year of Japanese calendar is
#' @return function jc2ad return convert function which you've set
jc2ad_func <- function(gengou, first_year){

    # "元号(.+)年"でマッチ
    pattern <- paste0(gengou, "(.+)\u5e74")

    ans_func <- function(input_string){

        input_string <- as.character(input_string)

        #str <- sub("元年", "1年", input_string)
        str <- sub("\u5143\u5e74", "1\u5e74", input_string)

        tmp <- stringr::str_match(str, pattern)
        ans <- ifelse(is.na(tmp[,1]), input_string,
                      as.integer(tmp[,2]) + first_year - 1)

        return(ans)
    }

    return(ans_func)
}

