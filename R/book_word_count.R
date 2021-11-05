#' Book Word Count
#'
#' Returns a dataframe with the word counts for index.Rmd and all input files
#' in the book input directory, along with totals, using rmdwc::rmdcount and
#' janitor::adorn_totals.
#'
#' @param update Boolean, FALSE by default. If TRUE, also takes the sum of the
#' word counts without code-chunks and updates the field `book_word_count`
#' in `_meta.yml`.
#'
#' @return A dataframe of word and characters counts from all book input files.
#' @export
#'
#' @examples
#' book_word_count()
book_word_count <- function(update = FALSE) {

  counts <- rmdwc::rmdcount(files = c("index.Rmd",
                                      fs::dir_ls("book/src",
                                                 recurse = TRUE)))

  totals <- janitor::adorn_totals(counts,
                                  where = "row",
                                  fill = "")

  if(update) {

    meta <- yaml::read_yaml("_meta.yml")

    total_word_count <- totals[[nrow(totals), "words_chunk"]]

    meta$book_word_count <- total_word_count

    yaml::write_yaml(meta, "_meta.yml")

  }

  return(totals)

}
