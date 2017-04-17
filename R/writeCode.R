##' @title writeCode
##' @description write generated code to a file
##' @param obj The Recipe or list of Recipes to write code for
##' @param outfile The filename or connection to write to
##' @param append if outfile is a connection, should code be appended rather
##' than replacing existing code. Defaults to FALSE
##' @return NULL (silently)
##' @export
writeCode = function(obj, outfile, append = FALSE) {

    splash = !append
    if(!is(outfile, "connection")) {
        outfile = file(outfile, open="w")
        on.exit(close(outfile))
        append = TRUE
    }
    if(splash)
        cat(.splashtext, file = outfile, append=append)

    if(is(obj, "list")) {
        cat(.prettify(genCode(obj[[1]])), "\n", sep="\n",file = outfile, append = append)
        lapply(obj[-1], function(x) {
            cat(.prettify(genCode(x)), "\n", sep="\n", file = outfile, append=TRUE)
        })
    } else
        cat(.prettify(genCode(obj)), "\n", sep="\n", file = outfile, append=append)
    invisible(NULL)
    

}
