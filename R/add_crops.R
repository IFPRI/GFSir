#' add_crops
#'
#' @param df dataframe with "j" or "c" column
#'
#' @return Cleaned scenario name
#' @export
#' @examples
#' \dontrun{
#' add_crops()
#' }
#' @author Abhijeet Mishra
add_crops <- function(df) {
    origin_cols <- colnames(df)
    # Find regional mapping
    map <- getMapping(type = "crops")

    # Find if "j" or "c" exist in original columns
    if ("j" %in% origin_cols && !("c" %in% origin_cols)) {
        map <- map[startsWith(map$j_c, "j"), ]
        colnames(map)[1] <- "j"
    }

    if ("c" %in% origin_cols && !("j" %in% origin_cols)) {
        map <- map[startsWith(map$j_c, "c"), ]
        colnames(map)[1] <- "c"
    }

    # Merge with data on cty level
    df <- merge(df, map)

    return(df)
}
