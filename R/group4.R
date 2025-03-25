#' Results for per capital kcal availability
#'
#' @param gdx GDX of an IMPACT run
#' @param mapping mapping file name
#'
#' @return dataframe results for per capital kcal availability
#' @importFrom DOORMAT readGDX
#' @importFrom collapse join
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' group4()
#' }
#' @author Abhijeet Mishra
group4 <- function(gdx, mapping = "mapping.xlsx") {
    c <- cty <- yrs <- value <- NULL
    scenario <- clean_filename(gdx)

    # Per capita kcal by commodity
    pckcalC <- readGDX(gdx = gdx, name = "PerCapKcal_com", quick_df = TRUE)$data

    # "Other" per capita kcal
    other_kcal <-
        readGDX(gdx = gdx, name = "OtherCalorie", quick_df = TRUE)$data
    other_kcal$c <- "cothr" # add to other category

    # Merge with per cap kcal - this would mean two "instances" of cothr
    pckcalC <- rbind(pckcalC, other_kcal)

    # Sum over cother
    pckcalC <- pckcalC %>%
        group_by(c, cty, yrs) %>%
        summarise(value = sum(value)) %>%
        arrange(c, cty, yrs)

    # Population in Million
    pop <- readGDX(gdx = gdx, name = "POPX0", quick_df = TRUE)$data

    # Merge pckcalC and population
    df <- join(pckcalC, pop,
                on = c("cty", "yrs"),
                suffix = c(".pckcalC", ".population"))
    df$value <- df$value.pckcalC * df$value.population
    df$value <- round(df$value, 2)

    # Clean
    df$scenario <- scenario
    df <- create_identifier_columns(df)

    # Get regions
    df <- add_regions(df, mapping = mapping)

    # Get Crops
    df <- add_crops(df, mapping = mapping)

    # Do aggregation <- everything is an "absoulte" quantity so we can directly
    # sum over everything when making "groups"
    cols <- c("yrs", "ssp", "gcm", "rcp", "co2", "region", "name")

    df <- df %>%
        group_by_at(cols) %>%
        summarise(value = sum(value)) %>%
        arrange("yrs")

    # Population aggregation
    pop_agg <- add_regions(pop, mapping = mapping)

    cols <- c("yrs", "region")

    pop_agg <- pop_agg %>%
        group_by_at(cols) %>%
        summarise(value = sum(value)) %>%
        arrange("yrs")

    # Bring back aggregated "total calories" and "population" together

    df <- join(df, pop_agg,
                on = c("yrs", "region"),
                suffix = c(".TKcal", ".population"))
    df$value <- df$value.TKcal / df$value.population
    df$description <-
        "per capita calorie by commodity (KCal per person per day)"
    df <- df[, !startsWith(x = colnames(df), prefix = "value.")]
    df <- df[, c("description", colnames(df)[!colnames(df) %in% "description"])]
    df$value <- round(df$value)

    return(df)
}
