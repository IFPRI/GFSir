#' Results for population at risk of hunger
#'
#' @param gdx GDX of an IMPACT run
#'
#' @return Results for population at risk of hunger
#' @importFrom dplyr group_by_at summarise
#' @export
#'
#' @examples
#' \dontrun{
#' group5()
#' }
#' @author Abhijeet Mishra
group5 <- function(gdx) {

    scenario <- clean_filename(gdx)

    # Population at risk of hunger
    PopulationAtRisk <- readGDX(gdx = gdx,
                                name = "PopulationAtRisk", quick_df = TRUE)$data
    PopulationAtRisk$value[PopulationAtRisk$value < 0] <- 0

    # Population in Million
    pop <- readGDX(gdx = gdx, name = "POPX0", quick_df = TRUE)$data

    # Merge PopulationAtRisk and population
    df <- merge(PopulationAtRisk, pop,
                by = c("cty", "yrs"),
                suffixes = c(".PopulationAtRisk", ".population"))
    # Clean
    df$scenario <- scenario
    df <- create_identifier_columns(df)

    # Get regions
    df <- add_regions(df)

    # Do aggregation <- everything is an "absoulte" quantity so we can directly
    # sum over everything when making "groups"
    cols <- c("yrs", "ssp", "gcm", "rcp", "co2", "region")

    value.populationatrisk <- value.population <- NULL

    df <- df %>%
        group_by_at(cols) %>%
        summarise(risk_pop = sum(value.populationatrisk),
                  pop = sum(value.population)) %>%
        arrange("yrs")

    df$value <- round(df$risk_pop, 2)
    df$share <- round(df$risk_pop / df$pop, 4)
    df$description <-
        "Population at risk of hunger"
    df <- df[, !colnames(df) %in% c("risk_pop", "pop")]
    df <- df[, c("description", colnames(df)[!colnames(df) %in% "description"])]
    return(df)
}
