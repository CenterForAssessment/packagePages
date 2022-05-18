`.onLoad` <-
function(libname, pkgname) {
}

`.onAttach` <-
function(libname, pkgname) {
	if (interactive()) {
		packageStartupMessage(magenta$bold('packagePages',paste(paste0(unlist(strsplit(as.character(packageVersion("packagePages")), "[.]")), c(".", "-", ".", "")), collapse=""),' (5-19-2022). For help: >help("packagePages") or visit https://centerforassessment.github.io/packagePages'))
	}
}
