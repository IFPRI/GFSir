#' Group 3 data processing (Area, yields, production, prices, net trade, demand)
#'
#' @param gdx GDX of an IMPACT run
#' @param indicator Which indicator to return in aggregated format. Defaults to
#' "population". Available settings are "area", "production", "yield", "prices",
#' "trade", "demand", "population" and "perCapDemand".
#'
#' @return dataframe results for Area, yields, production, prices, net trade,
#' demand
#' @importFrom DOORMAT readGDX
#' @import dplyr
#' @importFrom tidyr separate_wider_delim
#' @export
#'
#' @examples
#' \dontrun{
#' group3()
#' }
#' @author Abhijeet Mishra
group3 <- function(gdx, indicator = "population") {
    value <- NULL
    scenario <- clean_filename(gdx)

    .area <- function() {
        j <- cty <- riverbasin <- yrs <- NULL
        df <- readGDX(gdx = gdx, name = "AREAX0", quick_df = FALSE)$data
        df$scenario <- scenario

        df <- df |>
            separate_wider_delim("fpu",
                                 delim = "_",
                                 names = c("riverbasin", "cty"))
        # Clean
        df <- create_identifier_columns(df)
        # Get regions
        df <- add_regions(df)
        # Get crops
        df <- add_crops(df)
        # Sort
        df <- df %>%
            arrange(j, cty, riverbasin, yrs)

        # Do aggregation <- everything is an "absoulte" quantity so we
        # can directly sum over everything when making "groups"
        cols <- c("description", "yrs", "fctr",
                  "ssp", "gcm", "rcp", "co2",
                  "region", "name")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value)) %>%
            arrange("yrs")

        # Sum over factor (irrigation)
        cols <- c("description", "yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region", "name")

        df2 <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value)) %>%
            arrange("yrs")
        df2$fctr <- "ar+rf"
        df <- rbind(df, df2)

        return(df)
    }

    .production <- function() {
        j <- cty <- riverbasin <- yrs <- NULL
        area  <- readGDX(gdx = gdx, name = "AREAX0", quick_df = FALSE)$data
        yield <- readGDX(gdx = gdx, name = "YLDX0",  quick_df = FALSE)$data

        df <- merge(area, yield,
                    by = colnames(
                        area)[!colnames(area) %in% c("description", "value")],
                    suffixes = c(".area", ".yield"))
        df <- df[, colnames(df)[
            !startsWith(x = colnames(df), prefix = "description")]]
        df$description <- "Crop production (000 mt)"
        df$value <- df$value.area * df$value.yield

        # Set scenario
        df$scenario <- scenario

        # Split FPU
        df <- df |>
            separate_wider_delim("fpu",
                                 delim = "_",
                                 names = c("riverbasin", "cty"))
        # Clean
        df <- create_identifier_columns(df)
        # Get regions
        df <- add_regions(df)
        # Get crops
        df <- add_crops(df)
        # Sort
        df <- df %>%
            arrange(j, cty, riverbasin, yrs)

        # Do aggregation <- everything is an "absoulte" quantity so we
        # can directly sum over everything when making "groups"
        cols <- c("description", "yrs", "fctr",
                  "ssp", "gcm", "rcp", "co2",
                  "region", "name")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value)) %>%
            arrange("yrs")

        # Sum over factor (irrigation)
        cols <- c("description", "yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region", "name")

        df2 <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value)) %>%
            arrange("yrs")
        df2$fctr <- "ar+rf"
        df <- rbind(df, df2)

        return(df)
    }

    .yield <- function() {
        area <- .area()
        production <- .production()
        df <- merge(area,
                    production,
                    by = c("yrs", "fctr",
                           "ssp", "gcm", "rcp", "co2",
                           "region", "name"),
                    suffixes = c(".area", ".production"))
        df <- df[, colnames(df)[
            !startsWith(x = colnames(df), prefix = "description")]]
        df$description <- "Crop Yields (mt per ha)"
        df$value <- df$value.production / df$value.area
        df <- df[, colnames(area)]
    }

    .prices <- function() {
        cty <- yrs <- description <- NULL
        prices <- readGDX(gdx = gdx, name = "PWX0", quick_df = FALSE)$data
        demand <- readGDX(gdx = gdx, name = "QDX0", quick_df = FALSE)$data
        value  <- merge(demand, prices, by = c("c", "yrs", "model"),
                        suffixes = c(".demand", ".world_price"))
        value <- value[, colnames(value)[
            !startsWith(x = colnames(value), prefix = "description")]]
        value$value <- value$value.demand * value$value.world_price
        value <- value[, colnames(value)[
            !startsWith(x = colnames(value), prefix = "value.")]]
        value$description <- "Solution total VALUE for commodity (000 2005USD)"

        df <- rbind(value, demand)[, colnames(demand)]

        df$scenario <- scenario

        # Clean
        df <- create_identifier_columns(df)
        # Get regions
        df <- add_regions(df)
        # Get crops
        df <- add_crops(df)
        # Sort
        df <- df %>%
            arrange(c, cty, yrs)

        # Do aggregation <- everything is an "absoulte" quantity so we
        # can directly sum over everything when making "groups"
        cols <- c("description", "yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region", "name")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value)) %>%
            arrange("yrs")

        # Do price calculation - kick out description
        df$description <- as.factor(df$description)
        levels(df$description)
        exist_factors <- c("Solution total demand for commodity (000 mt)",
                           "Solution total VALUE for commodity (000 2005USD)")
        df$description <- factor(df$description,
                                 levels = exist_factors,
                                 labels = c("Demand", "Demand Value"))
        cols <- c("yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region", "name")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = round(value[description == "Demand Value"] /
                                        value[description == "Demand"])) %>%
            arrange("yrs")
        df$description <- gsub(pattern = "world ", replacement = "",
                               x = unique(prices$description))
        df <- df[, c(colnames(df)[length(colnames(df))],
                     colnames(df)[-length(colnames(df))])]

        return(df)
    }

    .nettrade <- function() {
        cty <- yrs <- NULL
        df <- readGDX(gdx = gdx, name = "QNX0", quick_df = FALSE)$data

        df$scenario <- scenario

        # Clean
        df <- create_identifier_columns(df)
        # Get regions
        df <- add_regions(df)
        # Get crops
        df <- add_crops(df)
        # Sort
        df <- df %>%
            arrange(c, cty, yrs)

        # Do aggregation <- everything is an "absoulte" quantity so we
        # can directly sum over everything when making "groups"
        cols <- c("description", "yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region", "name")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value)) %>%
            arrange("yrs")

        return(df)
    }

    .demand <- function() {
        cty <- yrs <- NULL
        df <- readGDX(gdx = gdx, name = "QFX0", quick_df = FALSE)$data

        df$scenario <- scenario

        # Clean
        df <- create_identifier_columns(df)
        # Get regions
        df <- add_regions(df)
        # Get crops
        df <- add_crops(df)
        # Sort
        df <- df %>%
            arrange(c, cty, yrs)

        # Do aggregation <- everything is an "absoulte" quantity so we
        # can directly sum over everything when making "groups"
        cols <- c("description", "yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region", "name")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value)) %>%
            arrange("yrs")

        return(df)
    }

    .population <- function() {
        cty <- yrs <- NULL
        df <- readGDX(gdx = gdx, name = "POPX0", quick_df = FALSE)$data

        df$scenario <- scenario

        # Clean
        df <- create_identifier_columns(df)
        # Get regions
        df <- add_regions(df)
        # Sort
        df <- df %>%
            arrange(cty, yrs)

        # Do aggregation <- everything is an "absoulte" quantity so we
        # can directly sum over everything when making "groups"
        cols <- c("description", "yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value)) %>%
            arrange("yrs")

        return(df)
    }

    .perCapDemand <- function() {
        demand <- .demand()
        pop    <- .population()
        df <- merge(demand,
                    pop,
                    by = c("yrs",
                           "ssp", "gcm", "rcp", "co2",
                           "region"),
                    suffixes = c(".demand", ".population"))
        df <- df[, colnames(df)[
            !startsWith(x = colnames(df), prefix = "description")]]
        df$description <- "Per capita demand for commodity (kg per cap)"
        df$value <- df$value.demand / df$value.population
        df <- df[, colnames(demand)]
    }

    if (indicator == "area")         return(.area())
    if (indicator == "production")   return(.production())
    if (indicator == "yield")        return(.yield())
    if (indicator == "prices")       return(.prices())
    if (indicator == "trade")        return(.nettrade())
    if (indicator == "demand")       return(.demand())
    if (indicator == "population")   return(.population())
    if (indicator == "perCapDemand") return(.perCapDemand())
}
