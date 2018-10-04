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
    dfs <- lapply(path, fread, integer64="numeric", data.table=FALSE,
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
                  "\u5730\u7a4d",              # 12 地積
                  "\u571f\u5730\u5358\u4fa1",  # 13 土地単
                  "\u571f\u5730\u5f62\u72b6",  # 14 土地形
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
                  "\u5099\u8003",              # 28 備考
                  "\u305d\u306e\u4ed6")        # 29 その他

    names(df) <- col_name

    return(df)
}
