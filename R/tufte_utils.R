#' @details \code{newthought()} can be used in inline R expressions in R
#'   Markdown (e.g. \samp{`r newthought(Some text)`}), and it works for both
#'   HTML (\samp{<span class="newthought">text</span>}) and PDF
#'   (\samp{\\newthought{text}}) output.
#' @param text A character string to be presented as a \dQuote{new thought}
#'   (using small caps), or a margin note, or a footer of a quote
#' @rdname tufte_handout
#' @export
#' @examples newthought('In this section')
newthought = function(text) {
  sprintf('\\newthought<span class="newthought">%s</span>', text)
}

#' @details \code{margin_note()} can be used in inline R expressions to write a
#'   margin note (like a sidenote but not numbered).
#' @param icon A character string to indicate there is a hidden margin note when
#'   the page width is too narrow (by default it is a circled plus sign)
#' @rdname tufte_handout
#' @export
margin_note = function(text, icon = '&#8853;') {
  if (is_html_output()) {
    marginnote_html(sprintf('<span class="marginnote">%s</span>', text), icon)
  } else if (is_latex_output()) {
    sprintf('\\marginnote{%s}', text)
  } else {
    warning('marginnote() only works for HTML and LaTeX output', call. = FALSE)
    text
  }
}

#' @details \code{quote_footer()} formats text as the footer of a quote. It puts
#'   \code{text} in \samp{<footer></footer>} for HTML output, and
#'   after \samp{\\hfill} for LaTeX output (to right-align text).
#' @rdname tufte_handout
#' @export
quote_footer = function(text) {
  sprintf('\\hfill<span class="blockquote footer">%s</span>', text)
}

#' @details \code{sans_serif()} applies sans-serif fonts to \code{text}.
#' @rdname tufte_handout
#' @export
sans_serif = function(text) {
  sprintf('\\textsf<span class="sans">%s</span>', text)
}

template_resources = function(name, ...) {
  system.file('rmarkdown', 'templates', name, 'resources', ..., package = 'tufte')
}

# import two helper functions from knitr
is_html_output = function(...) knitr::is_html_output(...)
is_latex_output = function(...) knitr::is_latex_output(...)

gsub_fixed = function(...) gsub(..., fixed = TRUE)

readUTF8 = function(file, ...) readLines(file, encoding = 'UTF-8', warn = FALSE, ...)
writeUTF8 = function(text, ...) writeLines(enc2utf8(text), ..., useBytes = TRUE)



searchYAML <- function(input, element="includes"){
  yml <- getYAML(input)
  yml <- yaml::yaml.load(paste(yml[-c(grep("---", yml), grep("[.][.][.]", yml))], collapse="\n"))
  if (!is.null(yml[[element]]))  return(yml[[element]])
}


scrubPDF <- function(input) {
  md.text <- read_utf8(file(input))

  #  Equations
  # tmp.latex.eqn <- list()
  # for(j in grep("[{]55pt[}]", md.text)) {
  #   if (grepl(" [\\][\\] ", md.text[j])) tmp.latex.eqn[[j]] <- "multline" else tmp.latex.eqn[[j]] <- "equation"
  #   md.text[j] <- gsub(".*[{]55pt[}]", paste("\\\\begin{", tmp.latex.eqn[[j]], "}", sep=""), md.text[j])
  # }
  # for(j in grep("[$][$]", md.text)) md.text[j] <- gsub("[$][$]", paste("\\\\end{", tmp.latex.eqn[[j]], "}", sep=""), md.text[j])

  #  HTML Tag conversion
  for(j in grep("<sup>th</sup>", md.text)) md.text[j] <- gsub("<sup>th</sup>", "$^{th}$", md.text[j])
  for(j in grep("<sup>st</sup>", md.text)) md.text[j] <- gsub("<sup>st</sup>", "$^{st}$", md.text[j])
  for(j in grep("<sup>rd</sup>", md.text)) md.text[j] <- gsub("<sup>rd</sup>", "$^{rd}$", md.text[j])
  for(j in grep("<br></br>", md.text)) md.text[j] <- gsub("<br></br>", "\\\\", md.text[j])
  for(j in grep("<em>", md.text)) md.text[j] <- gsub("<em>", "\\\\emph{", md.text[j])
  for(j in grep("</em>", md.text)) md.text[j] <- gsub("</em>", "}", md.text[j])
  for(j in grep("<strong>", md.text)) md.text[j] <- gsub("<strong>", "\\\\bf{", md.text[j])
  for(j in grep("</strong>", md.text)) md.text[j] <- gsub("</strong>", "}", md.text[j])

  for(j in grep("\\label[{]my[}]", md.text)) {
    md.text[j] <- gsub("[{]my[}]", paste("{table", strsplit(strsplit(md.text[j], "[*][*]Table ")[[1]][2], ":[*][*]")[[1]][1], "}", sep=""), md.text[j])
  }
  for(j in grep("\\caption[{][*][*]Table", md.text)) {
    md.text[j] <- gsub(":[*][*]", ":}", gsub("\\caption[{][*][*]Table", "\\caption*{\\\\textbf{Table", md.text[j]))
  }

  ## ztable doesn't have \caption
  for(j in grep("[*][*]Table", md.text)) {
    md.text[j] <- gsub("\\\\end", "}\\\\end", gsub(":[*][*]", ":}", gsub("[*][*]Table", "\\\\caption*{\\\\textbf{Table", md.text[j])))
  }

  if (any(grepl("<!-- HTML_Start", md.text))) {
    if (length(grep("<!-- HTML_Start", md.text)) != length(grep("<!-- LaTeX_Start", md.text))){
      stop("There must be equal number of '<!-- HTML_Start' and '<!-- LaTeX_Start' elements in the .Rmd file.")
    }
  }
  while(any(grepl("<!-- HTML_Start", md.text))) {
    html.index <- grep("<!-- HTML_Start", md.text)[1]
    latex.index<- grep("<!-- LaTeX_Start", md.text)[1]
    if (grepl("%", md.text[latex.index+1])) latex.index <- latex.index+1
    md.text <- md.text[-(html.index:latex.index)]
  }

  md.text <- gsub("<!-- LaTeX_Start", "", md.text)
  md.text <- gsub("LaTeX_End -->", "", md.text)

  ## Get rid of random latex(...) comments
  for(j in grep("%latex", md.text)) md.text[j] <- ""

  writeLines(md.text, input)
}

marginnote_html = function(text = '', icon = '&#8853;') {
  sprintf(paste0(
    '<label for="tufte-mn-" class="margin-toggle">%s</label>',
    '<input type="checkbox" id="tufte-mn-" class="margin-toggle">%s'
  ), icon, text)
}

