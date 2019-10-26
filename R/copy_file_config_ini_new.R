#' Copy file configuration from outline in working directory
#'
#' Copy file configuration from outline in working directory
#' to search for additional terms in our R Markdown or LaTeX project.
#'
#' The name of the configuration file has to be "addinsOutline_ini.txt".
#'
#' Regular expressions are used to perform searches through
#' the functions of the "stringr" package.
#'
#' Make a copy of this file to see how new searches are defined.
#'
#' The following strings serve to distinguish which types of files will be used:
#'
#' - "rmd": R Markdown files
#'
#' - "rmddb": Bookdown files
#'
#' - "tex": LaTeX files
#'
#' @param path_to path where the configuration file will be copied
#' @param overwritefile logical variable;  if \code{TRUE} will overwrite the file if it already exists
#'
#' @seealso The function \code{\link{run_addinsOutline_Rmd}()},
#' \code{\link{run_addinsOutline_Rmd_bookdown}()} and
#' \code{\link{run_addinsOutline_tex}()}.
#'
#' @examples
#' \dontrun{
#' copy_file_config_ini_new()
#' }
#' @export
copy_file_config_ini_new <- function(path_to=NULL,overwritefile = TRUE) {

  if (is.null(path_to)) {
     file.copy(from=paste(system.file("templates",package="addinsOutline"),
                       "addinsOutline_ini.txt",sep="/"),
            to="./",
            overwrite = overwritefile)
  } else {
    file.copy(from=paste(system.file("templates",package="addinsOutline"),
                         "addinsOutline_ini.txt",sep="/"),
              to=path_to,
              overwrite = overwritefile)
  }

}
