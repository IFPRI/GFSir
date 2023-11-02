#' clean_filename
#'
#' @param gdx GDX file for IMPACT run
#'
#' @return Cleaned scenario name
#' @export
#'
#' @examples
#' \dontrun{
#' clean_filename()
#' }
#' @author Abhijeet Mishra
clean_filename <- function(gdx) {
    name <- gsub(pattern = "\\.gdx", replacement = "", x = basename(gdx))
    name <- gsub(pattern = "379", replacement = "NoCO2", x = name)
    # Use a regular expression to find and replace the third segment
    name <- sub("^(.*?)-(.*?)-(.*?)-", "\\1-\\2-RCP\\3-", name)
    # Use a regular expression to find and replace first digit after RCP
    name <- sub("(RCP\\d)", "RCP", name)
    # Use a regular expression to add dot to RCP
    name <- sub("RCP(\\d)", "RCP\\1.", name)
    # Remove any extra hyphens that may have been added
    name <- gsub("--", "-", name)

    return(name)
}
