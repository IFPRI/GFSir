#' add_regions
#'
#' @param df dataframe with "cty" column
#'
#' @return Cleaned scenario name
#' @export
#' @importFrom tidyr pivot_longer
#' @examples
#' \dontrun{
#' add_regions()
#' }
#' @author Abhijeet Mishra
add_regions <- function(df) {
    origin_cols <- colnames(df)
    # Find regional mapping
    map <- getMapping(type = "region")

    # Merge with data on cty level
    df <- merge(df, map)

    # Pivot longer to convert new "regions" as rows
    df <- df |>
        pivot_longer(cols = colnames(df)[!colnames(df) %in% origin_cols],
                     names_to = "reg_desc",
                     values_to = "region")
    return(df)
}
