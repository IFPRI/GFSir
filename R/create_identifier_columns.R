#' create_identifier_columns
#'
#' @param df Dataframe where identifier columns need to be created
#' @param col Columns which are "ID" and need to be separated
#' @param into String vector with names of new columns
#' @param remove If "col" vector should be kicked out when making new columns.
#' @param sep Separator where "col" columns should be snipped for new columns.
#'
#' @return Cleaned scenario name
#' @export
#'
#' @importFrom tidyr separate
#' @examples
#' \dontrun{
#' create_identifier_columns()
#' }
#' @author Abhijeet Mishra
create_identifier_columns <- function(df, col = "scenario",
                                      into = c("SSP", "GCM", "RCP", "CO2"),
                                      sep = "-",
                                      remove = TRUE) {
    df <- separate(df, col, into = into, sep = sep, remove = remove)
    if ("RCP" %in% colnames(df)) df$RCP <- gsub(x = df$RCP, pattern = "RCP",
                                               replacement = "")
    colnames(df) <- tolower(colnames(df))
    return(df)
}
