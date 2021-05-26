#' Create R Project Skeleton
#'
#' Creates R project skeleton. This is not an R package skeleton.
#'
#' @param path The directory in which to create the skeletal directories.
#'
#' @export

proj_skeleton <- function(path = ".") {
  if (is.null(list.files(path = path, pattern = ".Rproj"))) {
    stop("This does not seem to be the root directory of Rproject")
  }
  dir.create(path = file.path(path, "data"))
  dir.create(path = file.path(path, "data", "raw"))
  dir.create(path = file.path(path, "data", "tidy"))
  dir.create(path = file.path(path, "doc"))
  dir.create(path = file.path(path, "R"))
  dir.create(path = file.path(path, "output"))
  dir.create(path = file.path(path, "fig"))
  dir.create(path = file.path(path, "tab"))
  dir.create(path = file.path(path, "report"))
}
