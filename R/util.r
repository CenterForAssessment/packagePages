#' @importFrom magrittr %>%
#' @importFrom roxygen2 roxygenise
#' @importFrom crayon magenta
#' @importFrom toOrdinal toOrdinalDate
#' @importFrom utils packageVersion
NULL

inst_path <- function() {
  if (is.null(pkgload::dev_meta("packagePages"))) {
    # packagePages is probably installed
    system.file(package = "packagePages")
  } else {
    # packagePages was probably loaded with devtools
    file.path(getNamespaceInfo("packagePages", "path"), "inst")
  }
}

"%||%" <- function(a, b) {
  if (length(a)) a else b
}

markdown_text <- function(text, ...) {
  if (is.null(text))
    return(text)

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  writeLines(text, tmp)
  markdown(tmp, ...)
}

markdown <- function(path = NULL, ..., depth = 0L, index = NULL) {
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)

  rmarkdown::pandoc_convert(
    input = path,
    output = tmp,
    from = "gfm",
    to = "html",
    options = list(
      "-t",
      "html4",
      "--indented-code-classes=R",
      "--section-divs",
      ...
    )
  )

  xml <- xml2::read_html(tmp, encoding = "UTF-8")
  autolink_html(xml, depth = depth, index = index)
  tweak_anchors(xml, only_contents = FALSE)

  # Extract body of html - as.character renders as xml which adds
  # significant whitespace in tags like pre
  xml %>%
    xml2::xml_find_first(".//body") %>%
    xml2::write_html(tmp, format = FALSE)

  lines <- readLines(tmp, warn = FALSE)
  lines <- sub("<body>", "", lines, fixed = TRUE)
  lines <- sub("</body>", "", lines, fixed = TRUE)
  paste(lines, collapse = "\n")
}

tweak_anchors <- function(html, only_contents = TRUE) {
  if (only_contents) {
    sections <- xml2::xml_find_all(html, ".//div[@class='contents']//div[@id]")
  } else {
    sections <- xml2::xml_find_all(html, "//div[@id]")
  }

  if (length(sections) == 0)
    return()

  # Update anchors: dot in the anchor breaks scrollspy
  anchor <- sections %>%
    xml2::xml_attr("id") %>%
    gsub(".", "-", ., fixed = TRUE)
  purrr::walk2(sections, anchor, ~ (xml2::xml_attr(.x, "id") <- .y))

  # Update href of toc anchors , use "-" instead "."
  toc_nav <- xml2::xml_find_all(html, ".//div[@id='tocnav']//a")
  hrefs <- toc_nav %>%
      xml2::xml_attr("href") %>%
      gsub(".", "-", ., fixed = TRUE)
  purrr::walk2(toc_nav, hrefs, ~ (xml2::xml_attr(.x, "href") <- .y))

  headings <- xml2::xml_find_first(sections, ".//h1|h2|h3|h4|h5")
  has_heading <- !is.na(xml2::xml_name(headings))

  for (i in seq_along(headings)[has_heading]) {
    # Insert anchor in first element of header
    heading <- headings[[i]]
    if (length(xml2::xml_contents(heading)) == 0) {
       # skip empty headings
       next
   }

    xml2::xml_attr(heading, "class") <- "hasAnchor"
    xml2::xml_add_sibling(
      xml2::xml_contents(heading)[[1]],
      "a", href = paste0("#", anchor[[i]]),
      class = "anchor",
      `aria-hidden`="true",
      .where = "before"
    )
  }
  invisible()
}

tweak_tables <- function(html) {
  # Ensure all tables have class="table"
  table <- xml2::xml_find_all(html, ".//table")
  xml2::xml_attr(table, "class") <- "table"

  invisible()
}

set_contains <- function(haystack, needles) {
  all(needles %in% haystack)
}

mkdir <- function(..., quiet = FALSE) {
  path <- file.path(...)

  if (!file.exists(path)) {
    if (!quiet)
      message("Creating '", path, "/'")
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

rule <- function(..., pad = "-") {
  if (nargs() == 0) {
    title <- ""
  } else {
    title <- paste0(..., " ")
  }
  width <- max(getOption("width") - nchar(title) - 1, 0)
  message(title, paste(rep(pad, width, collapse = "")))
}

out_path <- function(path, ...) {
  if (is.null(path)) {
    ""
  } else {
    file.path(path, ...)
  }

}

is_dir <- function(x) file.info(x)$isdir

split_at_linebreaks <- function(text) {
  if (length(text) < 1)
    return(character())
  trimws(strsplit(text, "\\n\\s*\\n")[[1]])
}

up_path <- function(depth) {
  paste(rep.int("../", depth), collapse = "")
}

print_yaml <- function(x) {
  structure(x, class = "print_yaml")
}
#' @export
print.print_yaml <- function(x, ...) {
  cat(yaml::as.yaml(x), "\n", sep = "")
}

copy_dir <- function(from, to) {

  from_dirs <- list.dirs(from, full.names = FALSE, recursive = TRUE)
  from_dirs <- from_dirs[from_dirs != '']

  to_dirs <- file.path(to, from_dirs)
  purrr::walk(to_dirs, mkdir)

  from_files <- list.files(from, recursive = TRUE, full.names = TRUE)
  from_files_rel <- list.files(from, recursive = TRUE)

  to_paths <- file.path(to, from_files_rel)
  file.copy(from_files, to_paths, overwrite = TRUE)
}


find_first_existing <- function(path, ...) {
  paths <- file.path(path, c(...))
  for (path in paths) {
    if (file.exists(path))
      return(path)
  }

  NULL
}

#' Compute relative path
#'
#' @param path Relative path
#' @param base Base path
#' @export
#' @examples
#' rel_path("a/b", base = "here")
#' rel_path("/a/b", base = "here")
rel_path <- function(path, base = ".") {
  if (is_absolute_path(path)) {
    path
  } else {
    if (base != ".") {
      path <- file.path(base, path)
    }
    normalizePath(path, mustWork = FALSE)
  }
}

is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:|\\\\|~)", path)
}

package_path <- function(package, path) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(package, " is not installed", call. = FALSE)
  }

  pkg_path <- system.file("packagePages", path, package = package)
  if (pkg_path == "") {
    stop(package, " does not contain 'inst/packagePages/", path, "'", call. = FALSE)
  }

  pkg_path

}

out_of_date <- function(source, target) {
  if (!file.exists(target))
    return(TRUE)

  if (!file.exists(source)) {
    stop("'", source, "' does not exist", call. = FALSE)
  }

  file.info(source)$mtime > file.info(target)$mtime
}

#' Determine if code is executed by pkgdown
#'
#' This is occassionally useful when you need different behaviour by
#' packagePages and regular documentation.
#'
#' @export
#' @examples
#' in_pkgdown()
in_pkgdown <- function() {
  identical(Sys.getenv("IN_PKGDOWN"), "true")
}

set_pkgdown_env <- function(x) {
  old <- Sys.getenv("IN_PKGDOWN")
  Sys.setenv("IN_PKGDOWN" = x)
  invisible(old)
}

###---   ...   ---###   CENTER FOR ASSESSMENT added UTILS   ###---   ...   ---###

trimWhiteSpace <- function(line) gsub("(^ +)|( +$)", "", line)

###  Get YAML from .Rmd file
getYAML <- function(input, element=NULL){
  con <- file(input) # input file
  rmd.text <- read_utf8(con)
  # Valid YAML could end in "---" or "..."  - test for both.
  rmd.yaml <- rmd.text[grep("---", rmd.text)[1]:ifelse(length(grep("---", rmd.text))>=2, grep("---", rmd.text)[2], grep("[.][.][.]", rmd.text)[1])]
  close(con)
  if (is.null(element)) {
    return(rmd.yaml)
  } else {
    tmp.element <- gsub("'", "", gsub("\"", "", trimWhiteSpace(gsub(element, "", rmd.yaml[grep(element, rmd.yaml)]))))
    if (length(tmp.element) == 0) tmp.element <- "vignette"
    return(tmp.element)
  }
}

searchYAML <- function(input, element="includes"){
  yml <- getYAML(input)
  yml <-yaml::yaml.load(paste(yml[-c(grep("---", yml), grep("[.][.][.]", yml))], collapse="\n"))
  if (!is.null(yml[[element]]))  return(yml[[element]])
}

read_utf8 <- function(file) {
  if (inherits(file, 'connection')) con <- file else {
    con <- base::file(file, encoding = 'UTF-8'); on.exit(close(con), add = TRUE)
  }
  enc2utf8(readLines(con, warn = FALSE))
}

getGitHubTopics <- function(github_url) {
  topic_url <- paste0(gsub("github.com", "api.github.com/repos", github_url), "/topics")
  con <- curl::curl(topic_url)
  keywords <- paste(jsonlite::fromJSON(readLines(con, warn=FALSE))[['names']], collapse=", ") 
  close(con) 
  return(keywords)
}
