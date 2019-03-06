#' Create Census Geocoder Input Files and Curl Submission File
#'
#' This function takes a dataframe and creates a series of 10,000 line input
#' text files to be uploaded to the Census Geocoder for batch geocoding.
#' It also creates a text file in which curl commands are written.
#'
#' @importFrom stringr str_split
#' @importFrom utils tail
#' @importFrom utils write.table
#'
#' @param df Input dataframe.
#' @param input_path File path for input text files.
#' @param input_prefix File prefix for input text files.
#' @param output_path File path for output text files.
#' @param output_prefix File prefix for output text files.
#' @param curl_path File path for curl command file.
#' @param curl_file File name for curl command file, without the extension.
#' @param benchmark The benchmark selection for Census Geocoder.
#' @param vintage The vintage selection for Census Geocoder.
#'
#' @export

geocoder_curl_input <- function(df,
                                input_path = ".",
                                input_prefix = NULL,
                                output_path = ".",
                                output_prefix = NULL,
                                curl_path = ".",
                                curl_file = NULL,
                                benchmark = "Public_AR_Census2010",
                                vintage = "Census2010_Census2010") {
  if (is.null(input_prefix)) {
    input_prefix <- paste0(
      "geoinput_", tolower(tail(unlist(str_split(benchmark, "_")), 1)), "_"
    )
  }
  if (is.null(output_prefix)) {
    output_prefix <- paste0(
      "geobatch_", tolower(tail(unlist(str_split(benchmark, "_")), 1)), "_"
    )
  }
  if (is.null(curl_path)) {
    curl_path <- paste0(
      "curl_", tolower(tail(unlist(str_split(benchmark, "_")), 1))
    )
  }
  for (i in seq(ceiling(nrow(df) / 10000))) {
    temp <- df[seq((i - 1) * 10000, i * 10000 - 1), ]
    write.table(
      temp, file = file.path(input_path, paste0(input_prefix, i, ".txt")),
      quote = FALSE, sep = ",", na = "", row.names = FALSE, col.names = FALSE
    )
    write.table(
      data.frame(
        text = paste0(
          "curl --form addressFile=@", "./input/ocrov_", i, ".txt",
          " --form benchmark=", benchmark,
          " --form vintage=", vintage,
          " \"https://geocoding.geo.census.gov/",
          "geocoder/geographies/addressbatch?form\"",
          " --output ", file.path(output_path), "/",
          output_prefix, i, ".txt \n"
        )
      ),
      file = file.path(curl_path, paste0(curl_file, ".txt")),
      append = TRUE, quote = FALSE, sep = ",", na = "",
      row.names = FALSE, col.names = FALSE
    )
  }
  ### There is no return.
  print("Input/curl file generation complete.")
}
