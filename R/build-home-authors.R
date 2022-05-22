data_authors <- function(pkg = ".") {

  augment_name <- function(name_list, author_names) {
      if (grepl("href", name_list[['name']])) {
          name_list[['name']] <- gsub("/></", paste0("/>", " ", paste(author_names[['given']], collapse=" "), " ", author_names[['family']], "</"), name_list[['name']])
      }
      name_list
  }

  pkg <- as_pkgdown(pkg)
  author_info <- data_author_info(pkg)
  author_names_all <- pkg %>% pkg_authors()
  author_names_main <- pkg %>% pkg_authors(c("aut", "cre", "fnd"))

  all <- pkg %>%
    pkg_authors() %>%
    purrr::map(author_list, author_info)

  main <- pkg %>%
    pkg_authors(c("aut", "cre", "fnd")) %>%
    purrr::map(author_list, author_info)

  for (i in seq_along(all)) {
    all[[i]] <- augment_name(all[[i]], author_names_all[[i]])
  }

  for (i in seq_along(main)) {
    main[[i]] <- augment_name(main[[i]], author_names_main[[i]])
  }

  needs_page <- length(main) != length(all)

  print_yaml(list(
    all = all,
    main = main,
    needs_page = needs_page
  ))
}

pkg_authors <- function(pkg, role = NULL) {
  authors <- unclass(pkg$desc$get_authors())

  if (is.null(role)) {
    authors
  } else {
    purrr::keep(authors, ~ any(.$role %in% role))
  }
}


data_author_info <- function(pkg = ".") {
  pkg <- as_pkgdown(pkg)

  defaults <- list(
    "Damian W. Betebenner" = list(
      href = "https://github.com/dbetebenner",
      html = "<img class='img-author' src='https://centerforassessment.github.io/assets/dbetebenner.png' height='20' />"
    ),
    "Adam R. Van Iwaarden" = list(
      href = "https://github.com/adamvi",
      html = "<img class='img-author' src='https://centerforassessment.github.io/assets/adamvi.png' height='20' />"
    ),
    "Nathan Dadey" = list(
      href = "https://github.com/ndadey",
      html = "<img class='img-author' src='https://avatars0.githubusercontent.com/u/17909944' height='20' />"
    ),
    "Ben Domingue" = list(
      href = "https://github.com/ben-domingue",
      html = "<img class='img-author' src='https://avatars2.githubusercontent.com/u/1353244' height='20' />"
    ),
    "Yi Shang" = list(
      href = "https://github.com/shangy",
      html = "<img class='img-author' src='https://avatars0.githubusercontent.com/u/1530616' height='20' />"
    ),
    "Allie Cooperman" = list(
      href = "https://github.com/alliecooperman",
      html = "<img class='img-author' src='https://avatars.githubusercontent.com/u/82226809?v=4' height='20' />"
    )
  )

  utils::modifyList(defaults, pkg$meta$authors %||% list())
}


data_home_sidebar_authors <- function(pkg = ".") {
  pkg <- as_pkgdown(pkg)
  data <- data_authors(pkg)

  authors <- data$main %>% purrr::map_chr(author_desc)
  if (data$needs_page) {
    authors <- c(authors, "<a href='authors.html'>All authors...</li>")
  }

  list_with_heading(authors, "Developers")
}

build_authors <- function(pkg = ".", path = "docs", depth = 0L) {
  pkg <- as_pkgdown(pkg)

  data <- list(
    pagetitle = "Authors",
    authors = data_authors(pkg)$all,
    if (!is.null(pkg[['meta']][['DESCRIPTION']][['Description']])) {
      description = pkg[['meta']][['DESCRIPTION']][['Description']]
    } else {
      description = as.character(read_desc()$get("Description"))
    },
    keywords = getGitHubTopics(pkg[['meta']][['navbar']][['right']][[1]][['href']]),
    repo_name = tail(unlist(strsplit(pkg[['meta']][['navbar']][['right']][[1]][['href']], "/")), 1) 
  )

  render_page(pkg, "authors", data, file.path(path, "authors.html"), depth = depth)
}

author_name <- function(x, authors) {
  name <- format_author_name(x$given, x$family)

  if (!(name %in% names(authors)))
    return(name)

  author <- authors[[name]]

  if (!is.null(author$html)) {
    name <- author$html
  }

  if (is.null(author$href)) {
    name
  } else {
    paste0("<a href='", author$href, "'>", name, "</a>")
  }
}

format_author_name <- function(given, family) {
  given <- paste(given, collapse = " ")

  if (is.null(family)) {
    given
  } else {
    paste0(given, " ", family)
  }
}

author_list <- function(x, authors_info, comment = FALSE) {
  name <- author_name(x, authors_info)

  roles <- paste0(role_lookup[x$role], collapse = ", ")
  substr(roles, 1, 1) <- toupper(substr(roles, 1, 1))

  list(
    name = name,
    roles = roles,
    comment = x$comment
  )
}

author_desc <- function(x, comment = TRUE) {
  paste(
    x$name,
    "<br />\n<small class = 'roles'>", x$roles, "</small>",
    if (comment && !is.null(x$comment))
      paste0("<br/>\n<small>(", x$comment, ")</small>")
  )
}

role_lookup <- c(
  "aut" = "author",
  "com" = "compiler",
  "fnd" = "funder",
  "ctb" = "contributor",
  "cph" = "copyright&nbsp;holder",
  "cre" = "maintainer",
  "ctr" = "contractor",
  "dtc" = "data&nbsp;contributor",
  "ths" = "thesis&nbsp;advisor",
  "trl" = "translator"
)
