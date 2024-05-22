#' add_regions
#'
#' @param df dataframe with "cty" column
#' @param mapping Mapping file for regions
#'
#' @return Cleaned scenario name
#' @export
#' @importFrom tidyr pivot_longer
#' @examples
#' \dontrun{
#' add_regions()
#' }
#' @author Abhijeet Mishra
add_regions <- function(df, mapping = "mapping.xlsx") {
    origin_cols <- colnames(df)
    # Find regional mapping
    map <- getMapping(type = "region", file = mapping)

    # Merge with data on cty level
    df <- merge(df, map)

    # Pivot longer to convert new "regions" as rows
    df <- df |>
        pivot_longer(cols = colnames(df)[!colnames(df) %in% origin_cols],
                     names_to = "reg_desc",
                     values_to = "region")
    df <- df[!is.na(df$region), ]
    return(df)
}
