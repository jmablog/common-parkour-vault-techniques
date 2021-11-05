#' Build book
#'
#' @param format String, default "print". One of "all", "print", "web", "gitbook", "pdf",
#' or "word". "all" produces gitbook, pdf, and word outputs. "print" produces just
#' pdf and word outputs. "web" produces gitbook and pdf outputs. Individual output selections produces just that output.
#'
#' @param word_num Boolean, default FALSE. Set if Word output should have numbered sections.
#'
#' @export
#'
#' @examples
#'
build_book <- function(format = "print", word_num = FALSE){

  build_path <- glue::glue("book/builds/{Sys.Date()}")

  project_title <- glue::glue('{yaml::read_yaml("_meta.yml")$book_filename}.Rmd')

  bookdown_yml <-
      ymlthis::yml_bookdown_opts(.yml = ymlthis::yml_empty(),
        book_filename = project_title,
        rmd_subdir = c("book/src"),
        delete_merged_file = FALSE,
        output_dir = build_path,
        repo = "https://github.com/jmablog/common-parkour-vault-techniques"
      )

  ymlthis::use_bookdown_yml(bookdown_yml,
                            path = here::here())

  switch(format,
    "all" = formats <- c("bookdown::gitbook",
                         "bookdown::pdf_document2",
                         "bookdown::word_document2"),
    "print" = formats <- c("bookdown::pdf_document2",
                           "bookdown::word_document2"),
    "web" = formats <- c("bookdown::pdf_document2",
                           "bookdown::gitbook"),
    "gitbook" = formats <- c("bookdown::gitbook"),
    "pdf" = formats <- c("bookdown::pdf_document2"),
    "word" = formats <- c("bookdown::word_document2")
  )

  for(fmt in formats) {

    if(grepl("pdf", fmt)) {
      out_yml <- yaml::read_yaml("book/assets/output_yml/pdf_output.yml")
      ymlthis::use_yml_file(ymlthis::as_yml(out_yml), "_output.yml")
    }
    if(grepl("word", fmt)) {
      if(word_num) {
        out_yml <- yaml::read_yaml("book/assets/output_yml/word_output_num.yml")
      } else {
        out_yml <- yaml::read_yaml("book/assets/output_yml/word_output.yml")
      }
      ymlthis::use_yml_file(ymlthis::as_yml(out_yml), "_output.yml")
    }
    if(grepl("gitbook", fmt)) {
      out_yml <- yaml::read_yaml("book/assets/output_yml/gitbook_output.yml")
      ymlthis::use_yml_file(ymlthis::as_yml(out_yml), "_output.yml")
    }

    bookdown::render_book(here::here("index.Rmd"),
                          output_format = fmt)

    fs::file_delete("_output.yml")

  }

  fs::file_move(project_title, build_path)

  if(format %in% c("all", "print", "word")) {
    fs::file_delete(glue::glue("{build_path}/reference-keys.txt"))
  }

  fs::file_delete(here::here("_bookdown.yml"))

  if(fs::dir_exists(here::here("_bookdown_files"))) {
    fs::dir_copy(here::here("_bookdown_files"), build_path, overwrite = TRUE)

    fs::dir_delete(here::here("_bookdown_files"))
  }

  log_file <- glue::glue('{Sys.Date()}-{yaml::read_yaml("_meta.yml")$book_filename}.log')

  if(fs::file_exists(here::here(log_file))) {
    fs::file_move(log_file, build_path)
  }
}
