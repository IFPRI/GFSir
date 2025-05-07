#' Group 3 data processing (Area, yields, production, prices, net trade, demand)
#'
#' @param gdx GDX of an IMPACT run
#' @param indicator Which indicator to return in aggregated format. Defaults to
#' "population". Available settings are "area", "production", "yield", "prices",
#' "trade", "demand", "population", "perCapDemand", "bluewater" and "greenwater"
#' @param mapping mapping file name
#'
#' @return dataframe results for Area, yields, production, prices, net trade,
#' demand, demand_all (demand from all components)
#' @importFrom DOORMAT readGDX
#' @importFrom collapse join
#' @import dplyr
#' @importFrom tidyr separate_wider_delim
#' @export
#'
#' @examples
#' \dontrun{
#' group3()
#' }
#' @author Abhijeet Mishra
group3 <- function(gdx, indicator = "population", mapping = "mapping.xlsx") {
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
        df <- add_regions(df, mapping = mapping)
        # Get crops
        df <- add_crops(df, mapping = mapping)
        # Sort
        df <- df %>%
            arrange(j, cty, riverbasin, yrs)

        df <- df[df$fctr %in% c("air", "arf"), ]

        # Do aggregation <- everything is an "absoulte" quantity so we
        # can directly sum over everything when making "groups"
        cols <- c("description", "yrs", "fctr",
                  "ssp", "gcm", "rcp", "co2",
                  "region", "name")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")

        # Sum over factor (irrigation)
        cols <- c("description", "yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region", "name")

        df2 <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")
        df2$fctr <- "ar+rf"
        df <- rbind(df, df2)

        return(df)
    }

    .production <- function() {
        j <- cty <- riverbasin <- yrs <- NULL
        area  <- readGDX(gdx = gdx, name = "AREAX0", quick_df = FALSE)$data
        area <- area[area$fctr %in% c("air", "arf"), ]
        yield <- readGDX(gdx = gdx, name = "YLDX0",  quick_df = FALSE)$data
        yield <- yield[yield$fctr %in% c("air", "arf"), ]

        df <- join(area, yield,
                    on = colnames(
                        area)[!colnames(area) %in% c("description", "value")],
                    suffix = c(".area", ".yield"),
                   overid = 0)
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
        df <- add_regions(df, mapping = mapping)
        # Get crops
        df <- add_crops(df, mapping = mapping)
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
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")

        # Sum over factor (irrigation)
        cols <- c("description", "yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region", "name")

        df2 <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")
        df2$fctr <- "ar+rf"
        df <- rbind(df, df2)

        return(df)
    }

    .production_anml <- function() {
        j <- cty <- riverbasin <- yrs <- NULL
        nmbr  <- readGDX(gdx = gdx, name = "AnmlNumX0", quick_df = FALSE)$data
        yield <- readGDX(gdx = gdx, name = "AnmlYldX0",  quick_df = FALSE)$data

        df <- join(nmbr, yield,
                    on = colnames(
                        nmbr)[!colnames(nmbr) %in% c("description", "value")],
                    suffix = c(".nmbr", ".yield"),
                   overid = 0)
        df <- df[, colnames(df)[
            !startsWith(x = colnames(df), prefix = "description")]]
        df$description <- "Animal production (000 mt)"
        df$value <- df$value.nmbr * df$value.yield

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
        df <- add_regions(df, mapping = mapping)
        # Get crops
        df <- add_crops(df, mapping = mapping)
        # Sort
        df <- df %>%
            arrange(j, cty, riverbasin, yrs)

        # Do aggregation <- everything is an "absoulte" quantity so we
        # can directly sum over everything when making "groups"
        cols <- c("description", "yrs", "lvsys",
                  "ssp", "gcm", "rcp", "co2",
                  "region", "name")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")

        # Sum over factor (irrigation)
        cols <- c("description", "yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region", "name")

        df2 <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")
        df2$lvsys <- "All"
        df <- rbind(df, df2)

        return(df)
    }

    .production_fish <- function() {
        # Fish only has direct production, no area or number or yield
        j <- cty <- yrs <- NULL
        jfish  <- readGDX(gdx = gdx, name = "jfish", quick_df = TRUE)$data$j
        qsx0 <- readGDX(gdx = gdx, name = "QSX0",  quick_df = FALSE)$data
        df <- qsx0[qsx0$j %in% jfish, ]

        # Set scenario
        df$scenario <- scenario

        # Clean
        df <- create_identifier_columns(df)
        # Get regions
        df <- add_regions(df, mapping = mapping)
        # Get crops
        df <- add_crops(df, mapping = mapping)
        # Sort
        df <- df %>%
            arrange(j, cty, yrs)

        # Do aggregation <- everything is an "absoulte" quantity so we
        # can directly sum over everything when making "groups"
        cols <- c("description", "yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region", "name")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")

        return(df)
    }

    .yield <- function() {
        area <- .area()
        production <- .production()
        df <- join(area,
                    production,
                    on = c("yrs", "fctr",
                           "ssp", "gcm", "rcp", "co2",
                           "region", "name"),
                    suffix = c(".area", ".production"))
        df <- df[, colnames(df)[
            !startsWith(x = colnames(df), prefix = "description")]]
        df$description <- "Crop Yields (mt per ha)"
        df$value <- df$value.production / df$value.area
        df <- df[, colnames(area)]
    }

    .animalnumbers <- function() {
        j <- cty <- riverbasin <- yrs <- NULL
        df <- readGDX(gdx = gdx, name = "anmlnumx0", quick_df = FALSE)$data
        df$scenario <- scenario

        df <- df |>
            separate_wider_delim("fpu",
                                 delim = "_",
                                 names = c("riverbasin", "cty"))
        # Clean
        df <- create_identifier_columns(df)
        # Get regions
        df <- add_regions(df, mapping = mapping)
        # Get crops
        df <- add_crops(df, mapping = mapping)
        # Sort
        df <- df %>%
            arrange(j, cty, riverbasin, yrs)

        # Do aggregation <- everything is an "absoulte" quantity so we
        # can directly sum over everything when making "groups"
        cols <- c("description", "yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region", "name")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")

        return(df)
    }

    .yield_anml <- function() {
        numbers <- .animalnumbers()
        production <- .production_anml()
        production <- production[production$lvsys %in% "All", ]
        df <- join(numbers,
                    production,
                    on = c("yrs",
                           "ssp", "gcm", "rcp", "co2",
                           "region", "name"),
                    suffix = c(".number", ".production"))
        df <- df[, colnames(df)[
            !startsWith(x = colnames(df), prefix = "description")]]
        df$description <- "Animal Yields (mt per animal)"
        df$value <- df$value.production / df$value.number
        df <- df[, colnames(numbers)]
    }

    .prices <- function() {
        cty <- yrs <- description <- NULL
        prices <- readGDX(gdx = gdx, name = "PWX0", quick_df = FALSE)$data
        demand <- readGDX(gdx = gdx, name = "QDX0", quick_df = FALSE)$data
        value  <- join(demand, prices, on = c("c", "yrs", "model"),
                        suffix = c(".demand", ".world_price"), overid = 0)
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
        df <- add_regions(df, mapping = mapping)
        # Get crops
        df <- add_crops(df, mapping = mapping)
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
            summarise(value = sum(value, na.rm = TRUE)) %>%
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
            summarise(value = ifelse(value[description == "Demand"],
                                     round(value[description == "Demand Value"] /
                                               value[description == "Demand"]),
                                     NA)) %>%
            arrange("yrs")
        df$description <- gsub(pattern = "world ", replacement = "",
                               x = unique(prices$description))
        df <- df[, c(colnames(df)[length(colnames(df))],
                     colnames(df)[-length(colnames(df))])]

        return(df)
    }

    .netprices <- function() {
        j <- cty <- yrs <- description <- NULL
        prices <- readGDX(gdx = gdx, name = "PNETX0", quick_df = FALSE)$data
        prod <- readGDX(gdx = gdx, name = "QSX0", quick_df = FALSE)$data
        value  <- join(prod, prices, on = c("j", "cty", "yrs", "model"),
                        suffix = c(".prod", ".net_price"), overid = 0)
        value <- value[, colnames(value)[
            !startsWith(x = colnames(value), prefix = "description")]]
        value$value <- value$value.prod * value$value.net_price
        value <- value[, colnames(value)[
            !startsWith(x = colnames(value), prefix = "value.")]]
        value$description <- "Solution total VALUE for commodity (000 2005USD)"

        df <- rbind(value, prod)[, colnames(prod)]

        df$scenario <- scenario

        # Clean
        df <- create_identifier_columns(df)
        # Get regions
        df <- add_regions(df, mapping = mapping)
        # Get crops
        df <- add_crops(df, mapping = mapping)
        # Sort
        df <- df %>%
            arrange(j, cty, yrs)

        # Do aggregation <- everything is an "absoulte" quantity so we
        # can directly sum over everything when making "groups"
        cols <- c("description", "yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region", "name")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")

        # Do price calculation - kick out description
        df$description <- as.factor(df$description)
        levels(df$description)
        exist_factors <- c("Solution total production (000 mt)",
                           "Solution total VALUE for commodity (000 2005USD)")
        df$description <- factor(df$description,
                                 levels = exist_factors,
                                 labels = c("prod", "prod Value"))
        cols <- c("yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region", "name")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = ifelse(value[description == "prod"],
                                     round(value[description == "prod Value"] /
                                               value[description == "prod"]),
                                     NA)) %>%
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
        df <- add_regions(df, mapping = mapping)
        # Get crops
        df <- add_crops(df, mapping = mapping)
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
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")

        return(df)
    }

    .exports <- function() {
        cty <- yrs <- NULL
        df <- readGDX(gdx = gdx, name = "QEX0", quick_df = FALSE)$data

        df$scenario <- scenario

        # Clean
        df <- create_identifier_columns(df)
        # Get regions
        df <- add_regions(df, mapping = mapping)
        # Get crops
        df <- add_crops(df, mapping = mapping)
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
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")

        return(df)
    }

    .imports <- function() {
        cty <- yrs <- NULL
        df <- readGDX(gdx = gdx, name = "QMX0", quick_df = FALSE)$data

        df$scenario <- scenario

        # Clean
        df <- create_identifier_columns(df)
        # Get regions
        df <- add_regions(df, mapping = mapping)
        # Get crops
        df <- add_crops(df, mapping = mapping)
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
            summarise(value = sum(value, na.rm = TRUE)) %>%
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
        df <- add_regions(df, mapping = mapping)
        # Get crops
        df <- add_crops(df, mapping = mapping)
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
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")

        return(df)
    }

    .demand_all <- function() {
        cty <- yrs <- NULL
        df <- readGDX(gdx = gdx, name = "QDX0", quick_df = FALSE)$data

        df$scenario <- scenario

        # Clean
        df <- create_identifier_columns(df)
        # Get regions
        df <- add_regions(df, mapping = mapping)
        # Get crops
        df <- add_crops(df, mapping = mapping)
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
            summarise(value = sum(value, na.rm = TRUE)) %>%
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
        df <- add_regions(df, mapping = mapping)
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
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")

        return(df)
    }

    .perCapDemand <- function() {
        demand <- .demand()
        pop    <- .population()
        df <- join(demand,
                    pop,
                    on = c("yrs",
                           "ssp", "gcm", "rcp", "co2",
                           "region"),
                    suffix = c(".demand", ".population"))
        df <- df[, colnames(df)[
            !startsWith(x = colnames(df), prefix = "description")]]
        df$description <- "Per capita demand for commodity (kg per cap)"
        df$value <- df$value.demand / df$value.population
        df <- df[, colnames(demand)]
    }

    .bluewater <- function() {
        j <- cty <- riverbasin <- yrs <- NULL
        df <- readGDX(gdx = gdx,
                      name = "bluewaterfpux0",
                      quick_df = FALSE)$data
        df$description <- "Blue water or water from irrigation (cubic km)"
        df$scenario <- scenario

        df <- df |>
            separate_wider_delim("fpu",
                                 delim = "_",
                                 names = c("riverbasin", "cty"))
        # Clean
        df <- create_identifier_columns(df)
        # Get regions
        df <- add_regions(df, mapping = mapping)
        # Get crops
        df <- add_crops(df, mapping = mapping)
        # Sort
        df <- df %>%
            arrange(j, cty, riverbasin, yrs)

        # Do aggregation <- everything is an "absoulte" quantity so we
        # can directly sum over everything when making "groups"

        cols <- c("description", "yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region", "name")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")

        return(df)
    }

    .greenwater <- function() {
        j <- cty <- riverbasin <- yrs <- NULL
        df <- readGDX(gdx = gdx,
                      name = "greenwaterfpux0",
                      quick_df = FALSE)$data
        df$description <- "Green water or water from precipitation (cubic km)"
        df$scenario <- scenario

        df <- df |>
            separate_wider_delim("fpu",
                                 delim = "_",
                                 names = c("riverbasin", "cty"))
        # Clean
        df <- create_identifier_columns(df)
        # Get regions
        df <- add_regions(df, mapping = mapping)
        # Get crops
        df <- add_crops(df, mapping = mapping)
        # Sort
        df <- df %>%
            arrange(j, cty, riverbasin, yrs)

        # Do aggregation <- everything is an "absoulte" quantity so we
        # can directly sum over everything when making "groups"
        names(df)[names(df) %in% "lnd"] <- "fctr"
        cols <- c("description", "yrs", "fctr",
                  "ssp", "gcm", "rcp", "co2",
                  "region", "name")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")

        # Sum over factor (irrigation)
        cols <- c("description", "yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region", "name")

        df2 <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")
        df2$fctr <- "ar+rf"

        df <- rbind(df, df2)

        return(df)
    }

    .watdemand_dom <- function() {
        cty <- riverbasin <- yrs <- NULL
        df <- readGDX(gdx = gdx,
                      name = "wcd_d_fpu",
                      quick_df = FALSE)$data
        df$description <- "Domestic water demand (cubic km)"
        df$scenario <- scenario

        df <- df |>
            separate_wider_delim("fpu",
                                 delim = "_",
                                 names = c("riverbasin", "cty"))
        # Clean
        df <- create_identifier_columns(df)
        # Get regions
        df <- add_regions(df, mapping = mapping)

        # Sort
        df <- df %>%
            arrange(cty, riverbasin, yrs)

        # Do aggregation <- everything is an "absoulte" quantity so we
        # can directly sum over everything when making "groups"

        cols <- c("description", "yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")

        return(df)
    }

    .watdemand_ind <- function() {
        cty <- riverbasin <- yrs <- NULL
        df <- readGDX(gdx = gdx,
                      name = "wci_d_fpu",
                      quick_df = FALSE)$data
        df$description <- "Industrial water demand (cubic km)"
        df$scenario <- scenario

        df <- df |>
            separate_wider_delim("fpu",
                                 delim = "_",
                                 names = c("riverbasin", "cty"))
        # Clean
        df <- create_identifier_columns(df)
        # Get regions
        df <- add_regions(df, mapping = mapping)

        # Sort
        df <- df %>%
            arrange(cty, riverbasin, yrs)

        # Do aggregation <- everything is an "absoulte" quantity so we
        # can directly sum over everything when making "groups"

        cols <- c("description", "yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")

        return(df)
    }

    .watdemand_lvs <- function() {
        cty <- riverbasin <- yrs <- NULL
        df <- readGDX(gdx = gdx,
                      name = "wcl_d_fpu",
                      quick_df = FALSE)$data
        df$description <- "Livestock water demand (cubic km)"
        df$scenario <- scenario

        df <- df |>
            separate_wider_delim("fpu",
                                 delim = "_",
                                 names = c("riverbasin", "cty"))
        # Clean
        df <- create_identifier_columns(df)
        # Get regions
        df <- add_regions(df, mapping = mapping)

        # Sort
        df <- df %>%
            arrange(cty, riverbasin, yrs)

        # Do aggregation <- everything is an "absoulte" quantity so we
        # can directly sum over everything when making "groups"

        cols <- c("description", "yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")

        return(df)
    }

    .watdemand_irr <- function() {
        cty <- riverbasin <- yrs <- NULL
        df <- readGDX(gdx = gdx,
                      name = "giwd_d_fpu",
                      quick_df = FALSE)$data
        df$description <- "Irrigation water demand (cubic km)"
        df$scenario <- scenario

        df <- df |>
            separate_wider_delim("fpu",
                                 delim = "_",
                                 names = c("riverbasin", "cty"))
        # Clean
        df <- create_identifier_columns(df)
        # Get regions
        df <- add_regions(df, mapping = mapping)

        # Sort
        df <- df %>%
            arrange(cty, riverbasin, yrs)

        # Do aggregation <- everything is an "absoulte" quantity so we
        # can directly sum over everything when making "groups"

        cols <- c("description", "yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")

        return(df)
    }

    .watsupply_dom <- function() {
        cty <- riverbasin <- yrs <- NULL
        df <- readGDX(gdx = gdx,
                      name = "wcd_s_fpu",
                      quick_df = FALSE)$data
        df$description <- "Domestic water supply (cubic km)"
        df$scenario <- scenario

        df <- df |>
            separate_wider_delim("fpu",
                                 delim = "_",
                                 names = c("riverbasin", "cty"))
        # Clean
        df <- create_identifier_columns(df)
        # Get regions
        df <- add_regions(df, mapping = mapping)

        # Sort
        df <- df %>%
            arrange(cty, riverbasin, yrs)

        # Do aggregation <- everything is an "absoulte" quantity so we
        # can directly sum over everything when making "groups"

        cols <- c("description", "yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")

        return(df)
    }

    .watsupply_ind <- function() {
        cty <- riverbasin <- yrs <- NULL
        df <- readGDX(gdx = gdx,
                      name = "wci_s_fpu",
                      quick_df = FALSE)$data
        df$description <- "Industrial water supply (cubic km)"
        df$scenario <- scenario

        df <- df |>
            separate_wider_delim("fpu",
                                 delim = "_",
                                 names = c("riverbasin", "cty"))
        # Clean
        df <- create_identifier_columns(df)
        # Get regions
        df <- add_regions(df, mapping = mapping)

        # Sort
        df <- df %>%
            arrange(cty, riverbasin, yrs)

        # Do aggregation <- everything is an "absoulte" quantity so we
        # can directly sum over everything when making "groups"

        cols <- c("description", "yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")

        return(df)
    }

    .watsupply_lvs <- function() {
        cty <- riverbasin <- yrs <- NULL
        df <- readGDX(gdx = gdx,
                      name = "wcl_s_fpu",
                      quick_df = FALSE)$data
        df$description <- "Livestock water supply (cubic km)"
        df$scenario <- scenario

        df <- df |>
            separate_wider_delim("fpu",
                                 delim = "_",
                                 names = c("riverbasin", "cty"))
        # Clean
        df <- create_identifier_columns(df)
        # Get regions
        df <- add_regions(df, mapping = mapping)

        # Sort
        df <- df %>%
            arrange(cty, riverbasin, yrs)

        # Do aggregation <- everything is an "absoulte" quantity so we
        # can directly sum over everything when making "groups"

        cols <- c("description", "yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")

        return(df)
    }

    .watsupply_irr <- function() {
        cty <- riverbasin <- yrs <- NULL
        df <- readGDX(gdx = gdx,
                      name = "giwd_s_fpu",
                      quick_df = FALSE)$data
        df$description <- "Irrigation water supply (cubic km)"
        df$scenario <- scenario

        df <- df |>
            separate_wider_delim("fpu",
                                 delim = "_",
                                 names = c("riverbasin", "cty"))
        # Clean
        df <- create_identifier_columns(df)
        # Get regions
        df <- add_regions(df, mapping = mapping)

        # Sort
        df <- df %>%
            arrange(cty, riverbasin, yrs)

        # Do aggregation <- everything is an "absoulte" quantity so we
        # can directly sum over everything when making "groups"

        cols <- c("description", "yrs",
                  "ssp", "gcm", "rcp", "co2",
                  "region")

        df <- df %>%
            group_by_at(cols) %>%
            summarise(value = sum(value, na.rm = TRUE)) %>%
            arrange("yrs")

        return(df)
    }

    if (indicator == "area")            return(.area())
    if (indicator == "production")      return(.production())
    if (indicator == "production_anml") return(.production_anml())
    if (indicator == "production_fish") return(.production_fish())
    if (indicator == "animalnumbers")   return(.animalnumbers())
    if (indicator == "yield")           return(.yield())
    if (indicator == "yield_anml")      return(.yield_anml())
    if (indicator == "prices")          return(.prices())
    if (indicator == "netprices")       return(.netprices())
    if (indicator == "trade")           return(.nettrade())
    if (indicator == "exports")         return(.exports())
    if (indicator == "imports")         return(.imports())
    if (indicator == "demand")          return(.demand())
    if (indicator == "demand_all")      return(.demand_all())
    if (indicator == "population")      return(.population())
    if (indicator == "perCapDemand")    return(.perCapDemand())
    if (indicator == "bluewater")       return(.bluewater())
    if (indicator == "greenwater")      return(.greenwater())
    if (indicator == "watdemand_dom")      return(.watdemand_dom())
    if (indicator == "watdemand_ind")      return(.watdemand_ind())
    if (indicator == "watdemand_lvs")      return(.watdemand_lvs())
    if (indicator == "watdemand_irr")      return(.watdemand_irr())
    if (indicator == "watsupply_dom")      return(.watsupply_dom())
    if (indicator == "watsupply_ind")      return(.watsupply_ind())
    if (indicator == "watsupply_lvs")      return(.watsupply_lvs())
    if (indicator == "watsupply_irr")      return(.watsupply_irr())
}
