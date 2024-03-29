% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/packagePages.r
\name{packagePages}
\alias{packagePages}
\title{Build packagePages package website}
\usage{
packagePages(
  pkg = ".",
  path = "docs",
  examples = TRUE,
  run_dont_run = FALSE,
  mathjax = TRUE,
  preview = interactive(),
  seed = 1014,
  encoding = "UTF-8",
  input_presentations_directory = "Presentations"
)
}
\arguments{
\item{pkg}{Path to source package. If R working directory is not
set to the source directory, then pkg must be a fully qualified
path to the source directory (not a relative path).}

\item{path}{Location in which to save website, relative to package
path.}

\item{examples}{Run examples?}

\item{run_dont_run}{Run examples that are surrounded in \\dontrun?}

\item{mathjax}{Use mathjax to render math symbols?}

\item{preview}{If \code{TRUE}, will preview freshly generated site}

\item{seed}{Seed used to initialize so that random examples are
reproducible.}

\item{encoding}{The encoding of the input files.}

\item{input_presentations_directory}{Defaults to Presentations}
}
\description{
\code{packagePages()} is a copy of \code{\link{build_site}} so that a parallel function exists against \code{projectPages()}.
\code{pagkagePages()} is a convenient wrapper around two functions:
\itemize{
  \item \code{\link{build_articles}()}
  \item \code{\link{build_home}()}
  \item \code{\link{build_reference}()}
  \item \code{\link{build_news}()}
}
See the documentation for the each function to learn how to control
that aspect of the site.
}
\section{YAML config}{

There are five top-level YAML settings that affect the entire site:
\code{title}, \code{template}, and \code{navbar}.

\code{title} overrides the default site title, which is the package name.
It's used in the page title and default navbar.

You can also provided information to override the default display of
the authors. Provided a list named with the name of each author,
including \code{href} to add a link, or \code{html} to override the
text:

\preformatted{
authors:
  Hadley Wickham:
    href: http://hadley.nz
  RStudio:
    href: https://www.rstudio.com
    html: <img src="http://tidyverse.org/rstudio-logo.svg" height="24" />
}
}

\section{YAML config - navbar}{

\code{navbar} controls the navbar at the top of the page. It uses the same
syntax as \href{http://rmarkdown.rstudio.com/rmarkdown_websites.html#site_navigation}{RMarkdown}.
The following YAML snippet illustrates some of the most important features.

\preformatted{
navbar:
  type: inverse
  left:
    - text: "Home"
      href: index.html
    - text: "Reference"
      href: reference/index.html
    - text: "Articles"
      menu:
        - text: "Heading 1"
        - text: "Article A"
          href: articles/page_a.html
        - text: "Article B"
          href: articles/page_b.html
        - text: "---------"
        - text: "Heading 2"
        - text: "Article C"
          href: articles/page_c.html
        - text: "Article D"
          href: articles/page_d.html
  right:
    - icon: fa-github fa-lg
      href: https://example.com
}

Use \code{type} to choose between "default" and "inverse" themes.

You position elements by placing under either \code{left} or \code{right}.
Components can contain sub-\code{menu}s with headings (indicated by missing
\code{href}) and separators. Currently pkgdown only supports fontawesome
icons. You can see a full list of options at
\url{http://fontawesome.io/icons/}.

Any missing components (\code{type}, \code{left}, or \code{right})
will be automatically filled in from the default navbar: you can see
those values by running \code{\link{template_navbar}()}.
}

\section{YAML config - template}{

You can get complete control over the appearance of the site using the
\code{template} component. There are two components to the template:
the HTML templates used to layout each page, and the css/js assets
used to render the page in the browser.

The easiest way to tweak the default style is to use a bootswatch template,
by passing on the \code{bootswatch} template parameter to the built-in
template:

\preformatted{
template:
  params:
    bootswatch: cerulean
}

See a complete list of themes and preview how they look at
\url{https://gallery.shinyapps.io/117-shinythemes/}:

Optionally provide the \code{ganalytics} template parameter to enable
\href{Google Analytics}{https://www.google.com/analytics/}. It should
correspond to your
\href{tracking id}{https://support.google.com/analytics/answer/1032385}.

\preformatted{
template:
  params:
    ganalytics: UA-000000-01
}

You can also override the default templates and provide additional
assets. You can do so by either storing in a \code{package} with
directories \code{inst/pkgdown/assets} and \code{inst/pkgdown/templates},
or by supplying \code{path} and \code{asset_path}. To suppress inclusion
of the default assets, set \code{default_assets} to false.

\preformatted{
template:
  package: mycustompackage

# OR:

template:
  path: path/to/templates
  assets: path/to/assets
  default_assets: false
}

These settings are currently recommended for advanced users only. There
is little documentation, and you'll need to read the existing source
for pkgdown templates to ensure that you use the correct components.
}

\examples{
\dontrun{
build_package_site()
}
}
