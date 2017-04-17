## Mostly lifted from roxygen2 source code
## may be able to use roxygen framework entirely later?

##' @import roxygen2

##' @title parse_file
##' @description Parse a file with roxygen2-like S4 specifications into an
##' S4RecipeList
##' @param file The file containing the instruction comments to parse
##' @return an S4RecipeList contianing the recipes specified in the file.
##' @export
parse_file <- function(file) {
  parsed <- parse(file = file, keep.source = TRUE)
  if (length(parsed) == 0) return()

  refs <- getSrcref(parsed)
  comment_refs <- roxygen2:::comments(refs)
  as(mapply(makeRecipe, comment = comment_refs, ref = refs),
     "S4RecipeList")
}




