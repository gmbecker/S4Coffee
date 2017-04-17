
##' genCode
##' @param obj The object (generally a class definition, *Recipe object
##' or part thereof, to generate code from
##' @param ... unused
##' @return A character vector of R code, or list of multiple code blocks
##' @docType methods
##' @export
setGeneric("genCode", function(obj, ...) standardGeneric("genCode"))

##' @rdname genCode
##' @aliases genCode,SlotDirectiveList-method

setMethod("genCode", "SlotDirectiveList", function(obj, ...) {
    if(length(obj) == 0)
        return("")
    slots = sapply(obj, genCode)
    
    sprintf(", representation(%s)", paste(slots, collapse = " , "))
})

##' @rdname genCode
##' @aliases genCode,SlotDirective-method

setMethod("genCode", "SlotDirective", function(obj, ...) {
    if(length(obj) == 0)
        return("")

    sprintf("%s = \"%s\"", name(obj), slotclass(obj))
})

##' @rdname genCode
##' @aliases genCode,ContainsDirective-method

setMethod("genCode", "ContainsDirective", function(obj, ...) {
    if(length(obj) == 0)
        return("")
    wquotes = paste0("\"", classnames(obj), "\"")
    sprintf(", contains = c( %s )", paste(wquotes, collapse = " , "))
})

##' @rdname genCode
##' @aliases genCode,SlotDefaultDirectiveListe-method

setMethod("genCode", "SlotDefaultDirectiveList", function(obj, ...) {
    if(length(obj) == 0)
        return("")

    slotdefs = sapply(obj, genCode)
    sprintf(", prototype(%s)", paste(slotdefs, collapse = " , "))
})


##' @rdname genCode
##' @aliases genCode,SlotDefaultDirective-method

setMethod("genCode", "SlotDefaultDirective", function(obj, ...) {
    if(length(obj) == 0)
        return("")

    sprintf("%s = %s", name(obj), value(obj))
})


##' @rdname genCode
##' @aliases genCode,list-method

setMethod("genCode", "list", function(obj, ...) {
    if(length(obj) == 0)
        return("")

    sapply(obj, genCode)
})


##' @rdname genCode
##' @aliases genCode,SigTypeDirectiveList-method

setMethod("genCode", "SigTypeDirectiveList", function(obj, ...){
    if(length(obj) == 0)
        return("")

    
    paste("signature = c(", paste(paste0("'", sapply(obj, argname), "' = '",  sapply(obj, argclass), "'"),
                      collapse = ", "), ")\n")
})


collapseRecipe = "<%=paste(c(recipe(obj), paste0('## ',as.character(expr(obj)))), collapse='\n')%>\n\n"

.setGenString = function(stubify = FALSE) {
    tmpstr = "<%=funsym%>"
    if(stubify)
        tmpstr = paste0("stubify(", tmpstr, ")")
    
    c("if(!exists('<%=funsym%>')) \n\t<%=as.character(expr(obj))%>\n",
        paste0("if(!exists('<%=name%>') || !isGeneric('<%=name%>')) \n\tsetGeneric('<%=name%>',",
           tmpstr,
           ")\n"))
    
}
genericTemplate = c(
                  .setGenString(FALSE))

methodTemplate= c(collapseRecipe,
                 "setMethod('<%=name%>', <%=sig_code%>, <%=funsym%>)\n")

methodFromRecipe = function(obj) {
    alldirs = directives(obj)
    disp= alldirs[sapply(alldirs, function(x) is(x, "SigTypeDirective"))]
    meth = alldirs[sapply(alldirs, function(x) is(x, "MethodDirective"))]

    sig_code = genCode(SigTypeDirectiveList(disp))
    name = name(meth[[1]])
    funpexpr = parse(text = as.character(expr(obj)), keep.source=FALSE)
    if(is(funpexpr, "expression"))
        funpexpr = funpexpr[[1]]
    if(class(funpexpr) %in% c("=", "<-"))
        funsym = deparse(funpexpr[[3]])
    else
        funsym = paste(deparse(funpexpr), collapse="\n")
    ##    funsym = as.character(funpexpr[[2]])

    mcode = character()
    brew(file = textConnection(methodTemplate),
         output = textConnection("mcode", "w", local=TRUE))
    mcode
                                                    
    
}

genericTemplate = c(collapseRecipe,
                    "setGeneric('<%=name%>', <%=funexpr%>)")



##' @rdname genCode
##' @aliases genCode,S4GenericRecipe-method

setMethod("genCode", "S4GenericRecipe", function(obj, ...) {
    alldirs = directives(obj)
    meth = alldirs[sapply(alldirs, function(x) is(x, "GenericDirective"))]

    name = name(meth[[1]])
    funpexpr = parse(text = as.character(expr(obj)))[[1]]
    ##funsym = as.character(funpexpr[[2]])
    if(class(funpexpr) %in% c("=", "<-"))
        funpexpr = funpexpr[[3]]
    
    if(!any(sapply(alldirs, is, "DefaultMethodDirective")))
        funexpr = gsub("NULL$", sprintf("standardGeneric('%s')", name), deparse(funpexpr[1:2]))
    else
        funexpr = deparse(funpexpr)
    mcode = character()
    
    brew(file = textConnection(genericTemplate),
         output = textConnection("mcode", "w", local=TRUE))
    mcode
                                                    
    
})



stubify = function(fun) {
    f = function(...) { stop("No default method")}

    formals(f) = formals(fun)
    environment(f) = environment(fun)
    f

}


##' @rdname genCode
##' @aliases genCode,S4MethodRecipe-method
setMethod("genCode", "S4MethodRecipe", definition=methodFromRecipe)


## necessary preceding commas are included in non-empty generated code
classTemplate = c(collapseRecipe,
                  "setClass('<%=name%>'<%=slotcode%><%=containscode%><%=prototype%>)")

classFromRecipe = function(obj) {
    slots = SlotDirectiveList(getDirectives(obj, "slot"))
    name = name(getDirectives(obj, "class")[[1]])
    slotDefaults = getDirectives(obj, "slotDefault")
    contains = getDirectives(obj, "contains")
    slotDefaults = SlotDefaultDirectiveList(getDirectives(obj, "slotDefault"))
    accessors = getDirectives(obj, "accessors")

  
    
    
    slotcode = genCode(slots)
    containscode = genCode(contains)
    prototype = genCode(slotDefaults)
    
    clCode = character()
    con = textConnection("clCode", "w", local=TRUE)
    on.exit(close(con))
    brew(file = textConnection(classTemplate), output = con)

    
    
    if(length(accessors) > 0) {
        ## setClass (currently) works with only warning when slot classes not defined
        env = new.env( parent = .GlobalEnv)
        eval(parse(text = clCode), envir = env)
        cls = getClass(name, where = env) 
        afuns = .slotfuns(accessors[[1]], cls)
        acsobj = new("ClassAccessors", mapply(.do_slot, cls = list(cls@className),
                                              slot = afuns$slot,
                                              fname = afuns$funname,
                                              doSetter = afuns$setter),
                     class_name = cls@className)
        write_accessors(acsobj, con, append=TRUE, splash = "")
        
    }
    clCode
    
}

.slotfuns = function(acsdir, cls) {
    sln = slotnames(acsdir)
    if(identical(sln, "ALL")) {
        data.frame(slot = slotNames(cls), funname = slotNames(cls), setter = FALSE,
                   stringsAsFactors = FALSE)
    } else if (identical(sln, "ALL<-")) {
        data.frame(slot = slotNames(cls), funname = slotNames(cls), setter = TRUE,
                   stringsAsFactors = FALSE)
    } else {
        slt = ifelse(grepl(":", sln),
                     gsub(".*:([^<]+).*", "\\1",sln),
                     gsub("<-", "", sln, fixed=TRUE))
        fn = ifelse(grepl(":", sln),
                    gsub("[[:space:]]*([^:]+):.*", "\\1",sln),
                    slt)
                
        st = grepl("<-", sln, fixed=TRUE)            
        data.frame(slot = slt, 
                   funname = fn,
                   setter = st,
                   stringsAsFactors = FALSE)
    }

}

getDirectives = function(recipe, types) {
    inds = types(recipe) %in% types
    directives(recipe)[inds]
}


##' @rdname genCode
##' @aliases genCode,S4ClassRecipe-method

setMethod("genCode", "S4ClassRecipe", definition = classFromRecipe)

##' @rdname genCode
##' @aliases gencode,NULL-method
setMethod("genCode", "NULL", definition = function(obj) "\n")
