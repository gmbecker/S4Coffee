library(S4Coffee)

fil = system.file("examples/simpleclass.R", package="S4Coffee")
## needed to get myfancyfun
source(fil)
## do the comments parse

thing = parse_file(fil)

## does the code generate

code = genCode(thing)

## does the code evaluate
parseEval = function(x) eval(parse(text =x), envir = .GlobalEnv)
lapply(code, parseEval)


obj = new("myclass")

## did accessors get generated and slotDefaults work

stopifnot(slot1(obj))

obj2 = new("myclass", slot3=1:5)

## is it hitting the right method?
stopifnot(mymethod(obj2) == 15)
