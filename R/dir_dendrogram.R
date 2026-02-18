#' Directory Tree Dendrogram
#'
#' This function takes a vector of directory structures and turns it into a
#' dendrogram. The code is a variation of Tyler Rinker's StackOverflow post
#' here: https://stackoverflow.com/questions/36094183/how-to-build-a-dendrogram-from-a-directory-tree
#' Note that you can print everything by such as following:
#' `print(dir_dendrogram(list.dirs()), limit = 300)`
#'
#' @importFrom stats na.omit
#' @importFrom dplyr bind_rows
#'
#' @param path List of directories to be converted to a dendrogram
#'
#' @export

dir_dendrogram <- function(path) {
  if (!requireNamespace("data.tree", quietly = TRUE))
    stop("Package 'data.tree' is required for this function. Install it with install.packages('data.tree').")
  x <- lapply(strsplit(path, "/"), function(z) as.data.frame(t(z)))
  x <- suppressWarnings(bind_rows(x))
  x$pathString <- apply(
    x, 1, function(x) paste(trimws(na.omit(x)), collapse = "/")
  )
  return(data.tree::as.Node(x))
}
