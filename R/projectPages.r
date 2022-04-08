#' Build packagePages project website
#'
#' \code{projectPages()} is a convenient wrapper around one function: \code{\link{build_home}()}
#'
#' See the documentation for \code{\link{build_home}()} to control
#' the site.
#'
#' @section YAML config:
#' There are five top-level YAML settings that affect the entire site:
#' \code{title}, \code{template}, and \code{navbar}.
#'
#' \code{title} overrides the default site title, which is the package name.
#' It's used in the page title and default navbar.
#'
#' You can also provided information to override the default display of
#' the authors. Provided a list named with the name of each author,
#' including \code{href} to add a link, or \code{html} to override the
#' text:
#'
#' \preformatted{
#' authors:
#'   Hadley Wickham:
#'     href: http://hadley.nz
#'   RStudio:
#'     href: https://www.rstudio.com
#'     html: <img src="http://tidyverse.org/rstudio-logo.svg" height="24" />
#' }
#'
#' @section YAML config - navbar:
#' \code{navbar} controls the navbar at the top of the page. It uses the same
#' syntax as \href{http://rmarkdown.rstudio.com/rmarkdown_websites.html#site_navigation}{RMarkdown}.
#' The following YAML snippet illustrates some of the most important features.
#'
#' \preformatted{
#' navbar:
#'   type: inverse
#'   left:
#'     - text: "Home"
#'       href: index.html
#'     - text: "Reference"
#'       href: reference/index.html
#'     - text: "Articles"
#'       menu:
#'         - text: "Heading 1"
#'         - text: "Article A"
#'           href: articles/page_a.html
#'         - text: "Article B"
#'           href: articles/page_b.html
#'         - text: "---------"
#'         - text: "Heading 2"
#'         - text: "Article C"
#'           href: articles/page_c.html
#'         - text: "Article D"
#'           href: articles/page_d.html
#'   right:
#'     - icon: fa-github fa-lg
#'       href: https://example.com
#' }
#'
#' Use \code{type} to choose between "default" and "inverse" themes.
#'
#' You position elements by placing under either \code{left} or \code{right}.
#' Components can contain sub-\code{menu}s with headings (indicated by missing
#' \code{href}) and separators. Currently pkgdown only supports fontawesome
#' icons. You can see a full list of options at
#' \url{http://fontawesome.io/icons/}.
#'
#' Any missing components (\code{type}, \code{left}, or \code{right})
#' will be automatically filled in from the default navbar: you can see
#' those values by running \code{\link{template_navbar}()}.
#'
#' @section YAML config - template:
#' You can get complete control over the appearance of the site using the
#' \code{template} component. There are two components to the template:
#' the HTML templates used to layout each page, and the css/js assets
#' used to render the page in the browser.
#'
#' The easiest way to tweak the default style is to use a bootswatch template,
#' by passing on the \code{bootswatch} template parameter to the built-in
#' template:
#'
#' \preformatted{
#' template:
#'   params:
#'     bootswatch: cerulean
#' }
#'
#' See a complete list of themes and preview how they look at
#' \url{https://gallery.shinyapps.io/117-shinythemes/}:
#'
#' Optionally provide the \code{ganalytics} template parameter to enable
#' \href{Google Analytics}{https://www.google.com/analytics/}. It should
#' correspond to your
#' \href{tracking id}{https://support.google.com/analytics/answer/1032385}.
#'
#' \preformatted{
#' template:
#'   params:
#'     ganalytics: UA-000000-01
#' }
#'
#' You can also override the default templates and provide additional
#' assets. You can do so by either storing in a \code{package} with
#' directories \code{inst/pkgdown/assets} and \code{inst/pkgdown/templates},
#' or by supplying \code{path} and \code{asset_path}. To suppress inclusion
#' of the default assets, set \code{default_assets} to false.
#'
#' \preformatted{
#' template:
#'   package: mycustompackage
#'
#' # OR:
#'
#' template:
#'   path: path/to/templates
#'   assets: path/to/assets
#'   default_assets: false
#' }
#'
#' These settings are currently recommended for advanced users only. There
#' is little documentation, and you'll need to read the existing source
#' for pkgdown templates to ensure that you use the correct components.
#'
#' @inheritParams build_articles
#' @inheritParams build_reference
#' @param path Location in which to save website, relative to package
#'   path.
#' @param preview If \code{TRUE}, will preview freshly generated site
#' @param encoding Defaults to UTF-8
#' @param vignettes_directory Defaults to Documents
#' @export
#' @examples
#' \dontrun{
#' projectPages()
#' }
projectPages <- function(pkg = ".",
                       path = "docs",
                       preview = interactive(),
                       encoding = "UTF-8",
                       vignettes_directory="Documents",
                       input_presentations_directory="Presentations"
                       ) {
  if (file.exists("DESCRIPTION")) stop("NOTE: projectPages should not be applied to directories with a pre-existing R package description. Use packagePages() instead.")
  old <- set_pkgdown_env("true")
  on.exit(set_pkgdown_env(old))

  pkg <- as_pkgdown(pkg, vignettes_directory)
  path <- rel_path(path, pkg$path)

  init_site(pkg, path)

  ### build_home, then build_presentations, then build_articles
  if (input_presentations_directory %in% list.dirs(recursive=FALSE, full.names=FALSE)) build_presentations(output_directory = "docs/presentations", depth = 1L, encoding = encoding, input_presentations_directory=input_presentations_directory)
  build_home(pkg, path = path, encoding = encoding)
  build_articles(pkg, path = file.path(path, "articles"), depth = 1L, encoding = encoding, vignettes_directory="Documents")

  if (file.exists("DESCRIPTION")) file.remove("DESCRIPTION")

  if (preview) {
    preview_site(path)
  }
  invisible(TRUE)
}
