`.onLoad` <-
function(libname, pkgname) {
}

`.onAttach` <-
function(libname, pkgname) {
	if (interactive()) {
		packageStartupMessage(magenta$bold('packagePages',paste(paste0(unlist(strsplit(as.character(packageVersion("packagePages")), "[.]")), c(".", "-", ".", "")), collapse=""),' (6-21-2023). For help: >help("packagePages") or visit https://centerforassessment.github.io/packagePages'))
	}
}
