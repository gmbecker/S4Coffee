##' Generate accessors for an existing class
##' @param cls The class definition to generate acessors for
##' @param funnames named list of accessor name, slot name pairs. Defaults to
##' acessor functions named identically to the slots they access
##' @param skip Character vector of slots to NOT create accessors for
##' @param libs character vector of packages to load (via library) before
##' generating acessors
##' @param ... Unused
##' @return a ClassAccessors object for use with write_accessors
##' @export
makeAccessors = function(cls,
    funnames = structure(names(cls@slots),
        names = names(cls@slots)),
    skip = character(),
    libs = character(), ...) {


    if(is.character(cls))
        cls = getClass(cls)

    orides = list(...)
    if(length(orides))
        funnames[names(orides)] = as.character(orides)

    if(length(skip))
        funnames = funnames[!names(funnames) %in% skip]

    lapply(libs, library)
     
    
    res = mapply(.do_slot, cls= cls@className, fname = funnames,
        slot = names(funnames))

    names(res) = names(funnames)
    

    new("ClassAccessors", res, class_name = cls@className)
}

setClassUnion("functionOrNULL", c("function", "NULL"))

.templates = list(get_method =  "setMethod('<%=fname%>', '<%=cls%>', function(x) x@<%=slot%>)",
    get_generic = "if(!exists('<%=fname%>') || !isGeneric('<%=fname%>')) \nsetGeneric('<%=fname%>', function(x) standardGeneric('<%=fname%>'))",
    set_generic = "if(!exists('<%=fname%><-') || !isGeneric('<%=fname%><-')) \nsetGeneric('<%=fname%><-', function(x, value) standardGeneric('<%=fname%><-'))",
    
    set_method = "setMethod('<%=fname%><-', '<%=cls%>', function(x, value) {\nx@<%=slot%> = value\nx})")



##' @import brew
.do_slot = function(cls, fname, slot, templates = .templates, doSetter = TRUE) {

    gmeth = character()
    smeth = character()
    ggen = character()
    sgen = character()
    brew(file = textConnection(templates$get_method) ,
         output = textConnection("gmeth", "w", local = TRUE))
    brew(file = textConnection(templates$get_generic),
         output = textConnection("ggen", "w", local = TRUE))
    if(doSetter) {
    brew(file = textConnection(templates$set_method),
         output = textConnection("smeth",  "w", local = TRUE))
    brew(file = textConnection(templates$set_generic),
         output = textConnection("sgen", "w", local = TRUE))
    }
         
    res = SlotMethods(get_generic = ggen, fname = fname,
        set_generic = sgen,
        get_method = gmeth,
        set_method = smeth,
        class_name= cls, slot = slot)
    res
}

##' write accessor functions to file
##' @param info the object containing information about the accessors
##' to generate
##' @param filename The file to write the generated accessors into.
##' @param append Should the accessors be appended to \code{filename} or
##' replace the file entirely.
##' @param splash The splashtext to place at the top of the file
##' @note In future versions of S4Coffee, this API will be deprecated
##' after it is unified with the genCode/writeCode workflow, but it
##' remains separate for now.
##' @export
setGeneric("write_accessors", function(info, filename = "./R/accessors.R", append,
                                       splash = .splashtext)
    standardGeneric("write_accessors"))

##' @rdname write_accessors
##' @aliases write_accessors,list-method
setMethod("write_accessors", "list", function(info, filename, append,
                                              splash = .splashtext) {
    if(!is(filename, "connection")) {
        filename = file(filename, open = if(append) "a" else "w")
        on.exit(close(filename))
    }

    cat(splash, file = filename, sep="")
    lapply(info, write_accessors, filename = filename, splash = character())

})

##' @rdname write_accessors
##' @aliases write_accessors,ClassAccessors-method
setMethod("write_accessors", "ClassAccessors", function(info, filename, append,
                                                        splash = .splashtext) {
    if(!is(filename, "connection")) {
        filename = file(filename, open = if(append) "a" else "w")
        on.exit(close(filename))
    }


    cat(splash, file = filename, sep="")

    cat(sprintf("## Generated slot accessors for class %s\n",
                info@class_name), file = filename)
    lapply(info, write_accessors, filename = filename, splash = character())

})


##' @rdname write_accessors
##' @aliases write_accessors,SlotMethods-method
setMethod("write_accessors", "SlotMethods", function(info, filename, append,
                                                     splash = .splashtext) {


    if(!is(filename, "connection")) {
        filename = file(filename, open = if(append) "a" else "w")
        on.exit(close(filename))
    }
    

    cat(splash, file = filename, sep="")

    cat(sprintf("\n## Generated accessors for slot %s\n\n", info@slot),
        file = filename)
    
    cat(.prettify(info@get_generic),
        .prettify(info@get_method), "", sep = "\n\n",
        file = filename)
    if(length(info@set_generic) > 0 && nzchar(info@set_generic)) {
        cat(
        .prettify(info@set_generic),
        .prettify(info@set_method), "",
        sep = "\n\n",
        file = filename)
    }
    
    
})



