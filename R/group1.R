#' Results for population and GDP
#'
#' @param gdx GDX of an IMPACT run
#'
#' @return dataframe results for population and GDP
#' @importFrom DOORMAT readGDX
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' group1()
#' }
group1 <- function(gdx) {
    value <- NULL
    scenario <- clean_filename(gdx)

    # Population in Million
    pop <- readGDX(gdx = gdx, name = "POPX0", quick_df = FALSE)$data
    pop$scenario <- scenario

    # Income in billion 2005 USD
    income <- readGDX(gdx = gdx, name = "GDPX0", quick_df = FALSE)$data
    income$scenario <- scenario

    # Merge
    df <- rbind(pop, income)

    # Clean
    df <- create_identifier_columns(df)
    df$value <- round(df$value)

    # Get regions
    df <- add_regions(df)

    # Do aggregation <- everything is an "absoulte" quantity so we can directly
    # sum over everything when making "groups"
    cols <- c("description", "yrs", "ssp", "gcm", "rcp", "co2", "region")

    df <- df %>%
        group_by_at(cols) %>%
        summarise(value = sum(value)) %>%
        arrange("yrs")

    return(df)
}
