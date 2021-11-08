# Run the below in order to setup and reproduce the
# report. If using RStudio, you can use the keyboard
# shortcut `Cmd + Opt + R` to run this entire script.

# 1 ====================================================
# Activate renv and install the required packages
renv::activate()
renv::restore() # not required if using Docker

# 2 ====================================================
# Load custom functions for this research
devtools::load_all()

# 3 ====================================================
# Build the report into `book/builds/{date}`
# Replace "pdf" below with "word" or "gitbook" for those
# formats ,or use "print" for pdf & word, "web" for
# pdf & gitbooks, and "all" for all three formats.
# If building to word, set word_num true or false for
# numbered section headings.
build_book("pdf", word_num = FALSE)
