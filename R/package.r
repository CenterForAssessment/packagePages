#' Generate pkgdown data structure
#'
#' You will generally not need to use this unless you need a custom site
#' design and you're writing your own equivalent of \code{\link{build_site}}.
#'
#' @param path Path to package
#' @param vignettes_directory Name of vignettes directory (defaults to vignettes)
#' @export
as_pkgdown <- function(path = ".", vignettes_directory="vignettes") {
  if (is_pkgdown(path)) {
    return(path)
  }

  if (!file.exists(path) || !is_dir(path)) {
    stop("`path` is not an existing directory", call. = FALSE)
  }

  structure(
    list(
      path = path,
      desc = read_desc(path),
      meta = read_meta(path),
      topics = topic_index(path),
      vignettes = vignette_index(path, vignettes_directory)
    ),
    class = "pkgdown"
  )
}

is_pkgdown <- function(x) inherits(x, "pkgdown")

str_person <- function(pers) {
  s <- paste0(c(pers$given, pers$family), collapse = ' ')

  if (length(pers$email)) {
    s <- paste0("<a href='mailto:", pers$email, "'>", s, "</a>")
  }
  if (length(pers$role)) {
    s <- paste0(s, " [", paste0(pers$role, collapse = ", "), "]")
  }
  s
}

read_desc <- function(path = ".") {
  path.DESCRIPTION <- file.path(path, "DESCRIPTION")
  if (!file.exists(path.DESCRIPTION)) {
     read_meta(path, return_what="DESCRIPTION")
  }
  desc::description$new(path.DESCRIPTION)
}

# Metadata ----------------------------------------------------------------

read_meta <- function(path, return_what="YAML") {
  path <- find_first_existing(path, c("_pkgdown.yml", "_pkgdown.yaml"))

  if (is.null(path)) {
    yaml <- list()
  } else {
    yaml <- yaml::yaml.load_file(path)
  }
  if ("DESCRIPTION" %in% names(yaml) && !file.exists("DESCRIPTION")) {
      for (des.iter in seq_along(yaml[['DESCRIPTION']])) {
        cat(paste(names(yaml[['DESCRIPTION']])[des.iter], yaml[['DESCRIPTION']][[des.iter]], sep=": "), file="DESCRIPTION", append=TRUE, sep="\n")
      }
 }
  if (return_what=="YAML") return(yaml) else return(NULL)
}

# Topics ------------------------------------------------------------------

topic_index <- function(path = ".") {
  rd <- package_rd(path)

  aliases <- purrr::map(rd, extract_tag, "tag_alias")
  names <- purrr::map_chr(rd, extract_tag, "tag_name")
  titles <- purrr::map_chr(rd, extract_title)
  concepts <- purrr::map(rd, extract_tag, "tag_concept")
  internal <- purrr::map_lgl(rd, is_internal)

  file_in <- names(rd)
  file_out <- gsub("\\.Rd$", ".html", file_in)

  usage <- purrr::map(rd, topic_usage)
  funs <- purrr::map(usage, usage_funs)


  tibble::tibble(
    name = names,
    file_in = file_in,
    file_out = file_out,
    alias = aliases,
    usage = usage,
    funs = funs,
    title = titles,
    rd = rd,
    concepts = concepts,
    internal = internal
  )
}

package_rd <- function(path) {
  man_path <- file.path(path, "man")
  rd <- dir(man_path, pattern = "\\.Rd$", full.names = TRUE)
  names(rd) <- basename(rd)
  lapply(rd, rd_file, pkg_path = path)
}

extract_tag <- function(x, tag) {
  x %>%
    purrr::keep(inherits, tag) %>%
    purrr::map_chr(c(1, 1))
}

extract_title <- function(x) {
  x %>%
    purrr::detect(inherits, "tag_title") %>%
    flatten_text() %>%
    trimws()
}

is_internal <- function(x) {
  any(extract_tag(x, "tag_keyword") %in% "internal")
}


# Vignettes ---------------------------------------------------------------

vignette_index <- function(path = ".", vignettes_directory="vignettes") {
  vig_path <- dir(
    file.path(path, vignettes_directory),
    pattern = "\\.Rmd$",
    recursive = TRUE
  )

  title <- file.path(path, vignettes_directory, vig_path) %>%
    purrr::map(rmarkdown::yaml_front_matter) %>%
    purrr::map_chr("title", .null = "UNKNOWN TITLE")

  tibble::tibble(
    file_in = vig_path,
    file_out = gsub("\\.Rmd$", "\\.html", vig_path),
    name = tools::file_path_sans_ext(basename(vig_path)),
    path = dirname(vig_path),
    vig_depth = dir_depth(vig_path),
    title = title
  )
}

dir_depth <- function(x) {
  x %>%
    strsplit("") %>%
    purrr::map_int(function(x) sum(x == "/"))
}
