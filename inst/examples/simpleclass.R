
## This file acts as a "Spec by example" of the directives S4Coffee supports.


##^ @class myclass
##^ @slot slot1 logical 
##^ @slot slot2 character 
##^ @slot slot3 numeric
##^ @slotDefault slot1 TRUE
##^ @slotDefault slot2 "what?"
##^ @accessors ALL
NULL 


##^ @class myclass2
##^ @slot slot1 logical 
##^ @slot slot2 character 
##^ @slot slot3 numeric 
##^ @accessors ALL<-
NULL


##^ @class myclass3
##^ @slot slot1 logical 
##^ @slot slot2 character 
##^ @slot slot3 numeric 
##^ @accessors fancyfun:slot2 fancyfun2:slot3<-
NULL 

##^ @class myclass4
##^ @contains myclass3
##^ @slot slot4 ANY
NULL



##^ @generic mymethod
function(x,y = TRUE, z, ...) NULL


##^ @method mymethod
##^ @sigType x myclass

myfancyfun = function(x, y, z, ...) sum(slot3(x))



##^ @generic mygeneric
##^ @defaultMethod
function(x,y,z = TRUE,...) NULL

##^ @method mygeneric
##^ @sigType x  numeric
myfancyfun


##^ @method mygeneric
##^ @sigType x integer
function(x,y,z = TRUE,...) { sum(x)}


##^ @method mygeneric
##^ @sigType x character
function(x,y,z = TRUE, ...) {
    w = 5
    w
}
