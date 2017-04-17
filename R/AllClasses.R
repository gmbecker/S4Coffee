##' @import methods
##' @importFrom utils tail getSrcref

setClass("Directive", representation(type = "character"))

setClass("DirectiveList", contains = "list")

DirectiveList = function(...) new("DirectiveList", ...)

##' @title S4Recipe objects
##' @description S4Recipe objects represent the specifications of
##' classes and methods parsed from roxygen2-like S4Coffee comments
##' @rdname S4Recipe
##' @exportClass S4Recipe
setClass("S4Recipe", representation(recipe = "character", expr="ANY",
                                    directives = "DirectiveList"))
##' @exportClass S4ClassRecipe
##' @rdname S4Recipe
##' @aliases S4ClassRecipe-class
setClass("S4ClassRecipe", contains="S4Recipe")
##' @export
##' @param recipe The recipe (original S4Cofee comments)
##' @param expr The expression associated with the comments (currently a
##' srcRef but this is likely to change).
##' @param directives a DirectiveList containing the directives which make up
##' the recipe.
##' @rdname S4Recipe
##' @aliases S4ClassRecipe
S4ClassRecipe = function(recipe, expr, directives)
    new("S4ClassRecipe", recipe = recipe, expr = expr,
        directives = directives)


##' @rdname S4Recipe
##' @aliases S4MethodRecipe-class
##' @exportClass S4MethodRecipe
setClass("S4MethodRecipe", contains="S4Recipe")

##' @export
##' @rdname S4Recipe
##' @aliases S4MethodRecipe
S4MethodRecipe = function(recipe, expr, directives)
    new("S4MethodRecipe", recipe = recipe, expr = expr,
        directives = directives)


##' @rdname S4Recipe
##' @aliases S4GenericRecipe-class
##' @exportClass S4GenericRecipe
setClass("S4GenericRecipe", contains="S4Recipe")

##' @rdname S4Recipe
##' @aliases S4GenericRecipe
##' @export
S4GenericRecipe = function(recipe, expr, directives)
    new("S4GenericRecipe", recipe = recipe, expr = expr,
        directives = directives)



##' @rdname S4Recipe
##' @aliases S4RecipeList-class
##' @exportClass S4RecipeList
setClass("S4RecipeList", contains = "list")

##' @export
##' @rdname S4Recipe
##' @param ... Elements of the new S4RecipeList.
##' @aliases S4RecipeList
S4RecipeList = function(...) new("S4RecipeList",...)


setClass("EntryPattern", representation(single_words = "integer",
                                        freeform_end = "logical"))

EntryPattern = function(single_words = 1L, freeform_end = TRUE)  {
    if(!is(single_words, "integer"))
        single_words = as.integer(single_words)
    if(is.na(single_words) && freeform_end)
        stop("Cannot have variable word entries (single_words==NA) with free form end")
    new("EntryPattern", single_words = single_words, freeform_end = freeform_end)
}

entryPatterns = list(class = EntryPattern(1L, FALSE),
    slot = EntryPattern(2L, FALSE),
    accessors = EntryPattern(NA, FALSE),
   ## accessors = EntryPattern(1L, TRUE),
    method = EntryPattern(1L, FALSE),
    sigType = EntryPattern(2L, FALSE),
    contains = EntryPattern(NA, FALSE),
    slotDefault = EntryPattern(1L, TRUE),
    generic  = EntryPattern(1L, FALSE),
    defaultMethod= EntryPattern(0L, FALSE)
    )


setGeneric("types", function(obj) standardGeneric("types"))
setMethod("types", "DirectiveList", function(obj) sapply(obj, type))
setMethod("types", "S4Recipe", function(obj) types(directives(obj)))



setClass("ClassDirective", representation(name = "character"),
         contains = "Directive")
ClassDirective = function(name) new("ClassDirective", name = name,
    type = "class")

setClass("SlotDirective", representation(name = "character",
                                         slotclass = "character"
                                         ),
         contains = "Directive")

SlotDirective = function(name, slotclass)
    new("SlotDirective", name = name, slotclass = slotclass, 
        type = "slot")

setClass("AccessorsDirective", representation(slotnames = "character"),
         contains = "Directive")

AccessorsDirective = function(nms) {
    new("AccessorsDirective", slotnames = nms,
        type = "accessors")
}

setClass("MethodDirective", representation(name = "character"), contains = "Directive")

MethodDirective = function(name) new("MethodDirective", name = name, type = "method")

setClass("GenericDirective", representation(name = "character"), contains = "Directive")

GenericDirective = function(name) new("GenericDirective", name = name, type = "generic")

setClass("DefaultMethodDirective", contains = "Directive")

DefaultMethodDirective = function() new("DefaultMethodDirective", type = "defaultMethod")



setClass("SigTypeDirective", representation(argname = "character",
                                              argclass = "character"),
         contains = "Directive")

SigTypeDirective = function(argname, argclass) new("SigTypeDirective",
    argname = argname, argclass = argclass, type = "sigtype")

setClass("ContainsDirective", representation(classnames = "character"),
         contains = "Directive")

ContainsDirective = function(classnames) new("ContainsDirective",
    classnames = classnames, type = "contains")

setClass("SlotDefaultDirective", representation(name = "character",
                                         value = "character"),
         contains = "Directive")

SlotDefaultDirective = function(name, value)
    new("SlotDefaultDirective", name = name, value = value,
        type = "slotDefault")



setClass("SlotDirectiveList", contains = "list")

SlotDirectiveList = function(...) new("SlotDirectiveList", ...)

setClass("SlotDefaultDirectiveList", contains = "list")

SlotDefaultDirectiveList = function(...) new("SlotDefaultDirectiveList", ...)


setClass("SlotMethods", representation(class_name= "character",
                                       slot = "character",
                                       get_generic = "character",
                                       get_method = "character",
                                       getter_name = "character",
                                       set_generic = "character",
                                       setter_name = "character",
                                       set_method = "character"))


SlotMethods = function(class_name, fname, slot, get_generic, get_method,
    set_generic, set_method) {
    new("SlotMethods", class_name= class_name, slot = slot, get_generic = get_generic,
        getter_name = fname,
        get_method = get_method,
        setter_name = paste0(fname, "<-"),
        set_generic = set_generic, set_method = set_method)
}

setClass("ClassAccessors", contains = "list",
         representation(class_name = "character"))

setClass("SigTypeDirectiveList", contains = "DirectiveList")
SigTypeDirectiveList = function(x, ...) {
    if(!missing(x) && is(x, "list"))
        return( do.call(SigTypeDirectiveList, unname(x)))
    if(!missing(x))
        new("SigTypeDirectiveList", list(x, ...))
}
    
