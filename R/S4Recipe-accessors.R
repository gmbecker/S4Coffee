#######################################################################
## This code was automatically generated by S4Coffee version X.X.XX  ##
## S4Coffee is available at XXX                                      ##
##                                                                   ##
## S4Coffee is copyright Gabriel Becker. The code in this file is    ##
## copyright the author(s) of the package in which it originally     ##
## appears.                                                          ##
#######################################################################

## Generated slot accessors for class S4Recipe

## Generated accessors for slot recipe

if (!exists("recipe") || !isGeneric("recipe")) setGeneric("recipe", function(x) standardGeneric("recipe"))

setMethod("recipe", "S4Recipe", function(x) x@recipe)

if (!exists("recipe<-") || !isGeneric("recipe<-")) setGeneric("recipe<-", function(x, value) standardGeneric("recipe<-"))

setMethod("recipe<-", "S4Recipe", function(x, value) {
    x@recipe = value
    x
})





## Generated accessors for slot expr

if (!exists("expr") || !isGeneric("expr")) setGeneric("expr", function(x) standardGeneric("expr"))

setMethod("expr", "S4Recipe", function(x) x@expr)

if (!exists("expr<-") || !isGeneric("expr<-")) setGeneric("expr<-", function(x, value) standardGeneric("expr<-"))

setMethod("expr<-", "S4Recipe", function(x, value) {
    x@expr = value
    x
})





## Generated accessors for slot directives

if (!exists("directives") || !isGeneric("directives")) setGeneric("directives", function(x) standardGeneric("directives"))

setMethod("directives", "S4Recipe", function(x) x@directives)

if (!exists("directives<-") || !isGeneric("directives<-")) setGeneric("directives<-", function(x, value) standardGeneric("directives<-"))

setMethod("directives<-", "S4Recipe", function(x, value) {
    x@directives = value
    x
})




