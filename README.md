---
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{S4Coffee}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---

# S4Coffee: generating S4 classes and methods automatically from code and comments

## Introduction

The S4Coffee package allows users to declare S4 classes, S4 generics (with
default methods) and S4 methods via roxygen-style comments (Danenberg and Eugster,
Wickham et al.)

It also provides functionality for automatically generated so-called "accessor"
functions, i.e., getter and setter functions, for slots on S4 classes from
the class definition.


## Installation

S4Coffee is currently only available on Github in a development version.

It can be installed via `devtools` or `switchr`


```r
library(devtools)
install_github("gmbecker/S4Coffee")
```

or


```r
library(switchr)
man = GithubManifest("gmbecker/S4Coffee")
install_packages("S4Coffee", man)
```


## Declaring classes, generics, and methods

S4Coffee directives look exactly like roxygen2 directives, except that they
begin with `##^` rather than the traditional `##'.`

### classes

Classes are declared via the "class" roxygen-like directive, with the
slot, slotDefault, and contains methods being largely self-explanatory.

The one important detail about the slot directive is that after the slot name,
it expects the slot class before the description, which is arguably advisable but
not required when using roxygen comments to document a slot.

We can define a simple class, then, like so


```r
##^ @class myclass
##^ @slot a numeric
##^ @slot b character
##^ @slotDefault a 5L
##^ @slotDefault b "whaaaat"
##^ @accessors ALL
NULL
```

```
## NULL
```

Note we indicate the end of the comment with NULL.

The accessors directive indicates which slots should have accessors (getter and
setter functions) created for them. `ALL` indicates all slots should receive
getters, `ALL<-` indicates all slots should receive getters and setters.
Otherwise, accessors are declared slot-wise, via the pattern `funname:slotname`
to generate only getters with the name `funname` for the slot `slotname`, or
`funname:slotname<-` to do the same but also generate a `funname<-` setter
which sets the slot. 

S4Coffee also supports the `@contains` directive, which declares inheritence. 

### generics

S4 Generics are declared via the `@generic` directive, followed by ither
what we are calling a function declaration (the function signature with `NULL` as
the body, or by the desired default method in cases where a `@defaultMethod`
directive is present.:


```r
##^ @generic mygeneric
function(x, y = 5L, z, ...) NULL
```

or


```r
##^ @generic diffgeneric
##^ @defaultMethod
function(x, y = 5L, z, ...) sum(y)
```

When there is no `@defaultMethod` directive, the `NULL` in the generic declaration
is automatically replaced with a `standardGeneric` call in the generated generic
declaration.


### methods

Methods are declared with the `@method` directive, with `sigType` directives
specifying the type-signature of the method (one directive per parameter
in the type signature), like so


```r
##^ @method mygeneric
##^ @sigType x integer
##^ @sigType y numeric
function(x,y, z, ...) sum(y*x)


## Parsing files

The parse_file function parses a file (or character string, in fact) containing
these comments and generates an S4RecipeList containing S4Recipe objects for
each S4Coffee comment block.
```

```r
library(S4Coffee)
recips = parse_file(system.file("examples/simpleclass.R", package="S4Coffee"))
recips[[1]]
```

```
## An object of class "S4ClassRecipe"
## Slot "recipe":
## [1] "##^ @class myclass"               "##^ @slot slot1 logical "        
## [3] "##^ @slot slot2 character "       "##^ @slot slot3 numeric"         
## [5] "##^ @slotDefault slot1 TRUE"      "##^ @slotDefault slot2 \"what?\""
## [7] "##^ @accessors ALL"              
## 
## Slot "expr":
## NULL
## 
## Slot "directives":
## An object of class "DirectiveList"
## [[1]]
## An object of class "ClassDirective"
## Slot "name":
## [1] "myclass"
## 
## Slot "type":
## [1] "class"
## 
## 
## [[2]]
## An object of class "SlotDirective"
## Slot "name":
## [1] "slot1"
## 
## Slot "slotclass":
## [1] "logical"
## 
## Slot "type":
## [1] "slot"
## 
## 
## [[3]]
## An object of class "SlotDirective"
## Slot "name":
## [1] "slot2"
## 
## Slot "slotclass":
## [1] "character"
## 
## Slot "type":
## [1] "slot"
## 
## 
## [[4]]
## An object of class "SlotDirective"
## Slot "name":
## [1] "slot3"
## 
## Slot "slotclass":
## [1] "numeric"
## 
## Slot "type":
## [1] "slot"
## 
## 
## [[5]]
## An object of class "SlotDefaultDirective"
## Slot "name":
## [1] "slot1"
## 
## Slot "value":
## [1] "TRUE"
## 
## Slot "type":
## [1] "slotDefault"
## 
## 
## [[6]]
## An object of class "SlotDefaultDirective"
## Slot "name":
## [1] "slot2"
## 
## Slot "value":
## [1] "\"what?\""
## 
## Slot "type":
## [1] "slotDefault"
## 
## 
## [[7]]
## An object of class "AccessorsDirective"
## Slot "slotnames":
## [1] "ALL"
## 
## Slot "type":
## [1] "accessors"
```

## Generating code
 We generate code from recipes via the `genCode` function:


```r
code = genCode(recips)
code[[1]]
```

```
##  [1] "##^ @class myclass"                                                                                                                                 
##  [2] "##^ @slot slot1 logical "                                                                                                                           
##  [3] "##^ @slot slot2 character "                                                                                                                         
##  [4] "##^ @slot slot3 numeric"                                                                                                                            
##  [5] "##^ @slotDefault slot1 TRUE"                                                                                                                        
##  [6] "##^ @slotDefault slot2 \"what?\""                                                                                                                   
##  [7] "##^ @accessors ALL"                                                                                                                                 
##  [8] "## NULL"                                                                                                                                            
##  [9] ""                                                                                                                                                   
## [10] ""                                                                                                                                                   
## [11] "setClass('myclass', representation(slot1 = \"logical\" , slot2 = \"character\" , slot3 = \"numeric\"), prototype(slot1 = TRUE , slot2 = \"what?\"))"
## [12] "## Generated slot accessors for class myclass"                                                                                                      
## [13] ""                                                                                                                                                   
## [14] "## Generated accessors for slot slot1"                                                                                                              
## [15] ""                                                                                                                                                   
## [16] "if (!exists(\"slot1\") || !isGeneric(\"slot1\")) setGeneric(\"slot1\", function(x) standardGeneric(\"slot1\"))"                                     
## [17] ""                                                                                                                                                   
## [18] "setMethod(\"slot1\", \"myclass\", function(x) x@slot1)"                                                                                             
## [19] ""                                                                                                                                                   
## [20] ""                                                                                                                                                   
## [21] ""                                                                                                                                                   
## [22] "## Generated accessors for slot slot2"                                                                                                              
## [23] ""                                                                                                                                                   
## [24] "if (!exists(\"slot2\") || !isGeneric(\"slot2\")) setGeneric(\"slot2\", function(x) standardGeneric(\"slot2\"))"                                     
## [25] ""                                                                                                                                                   
## [26] "setMethod(\"slot2\", \"myclass\", function(x) x@slot2)"                                                                                             
## [27] ""                                                                                                                                                   
## [28] ""                                                                                                                                                   
## [29] ""                                                                                                                                                   
## [30] "## Generated accessors for slot slot3"                                                                                                              
## [31] ""                                                                                                                                                   
## [32] "if (!exists(\"slot3\") || !isGeneric(\"slot3\")) setGeneric(\"slot3\", function(x) standardGeneric(\"slot3\"))"                                     
## [33] ""                                                                                                                                                   
## [34] "setMethod(\"slot3\", \"myclass\", function(x) x@slot3)"                                                                                             
## [35] ""                                                                                                                                                   
## [36] ""
```


# Legacy accessor generation API for existing classes

S4Coffee also offers a legacy API for generating accessor code for classes
which already exist, via the makeAccessors and write_accessors functions.

makeAccessors accepts the name or class definition object of an existing
class, and generates a ClassAccessors object, while write_accessors accepts
ClassAccessors objects, along with the file to write to, and writes the
generated code.

Note: this legacy code is used under-the-hood when handling the `@accessors`
directive described above.




```r
setClass("a", representation(a = "integer", b="numeric", c = "ANY"))
acs = makeAccessors("a")
thing = character()
con = textConnection("thing", "w", local=TRUE)
write_accessors(acs, con, splash="", append=TRUE)
```

```
## $a
## NULL
## 
## $b
## NULL
## 
## $c
## NULL
```

```r
close(con)
thing
```

```
##  [1] "## Generated slot accessors for class a"                                                                      
##  [2] ""                                                                                                             
##  [3] "## Generated accessors for slot a"                                                                            
##  [4] ""                                                                                                             
##  [5] "if (!exists(\"a\") || !isGeneric(\"a\")) setGeneric(\"a\", function(x) standardGeneric(\"a\"))"               
##  [6] ""                                                                                                             
##  [7] "setMethod(\"a\", \"a\", function(x) x@a)"                                                                     
##  [8] ""                                                                                                             
##  [9] ""                                                                                                             
## [10] "if (!exists(\"a<-\") || !isGeneric(\"a<-\")) setGeneric(\"a<-\", function(x, value) standardGeneric(\"a<-\"))"
## [11] ""                                                                                                             
## [12] "setMethod(\"a<-\", \"a\", function(x, value) {"                                                               
## [13] "    x@a = value"                                                                                              
## [14] "    x"                                                                                                        
## [15] "})"                                                                                                           
## [16] ""                                                                                                             
## [17] ""                                                                                                             
## [18] ""                                                                                                             
## [19] "## Generated accessors for slot b"                                                                            
## [20] ""                                                                                                             
## [21] "if (!exists(\"b\") || !isGeneric(\"b\")) setGeneric(\"b\", function(x) standardGeneric(\"b\"))"               
## [22] ""                                                                                                             
## [23] "setMethod(\"b\", \"a\", function(x) x@b)"                                                                     
## [24] ""                                                                                                             
## [25] ""                                                                                                             
## [26] "if (!exists(\"b<-\") || !isGeneric(\"b<-\")) setGeneric(\"b<-\", function(x, value) standardGeneric(\"b<-\"))"
## [27] ""                                                                                                             
## [28] "setMethod(\"b<-\", \"a\", function(x, value) {"                                                               
## [29] "    x@b = value"                                                                                              
## [30] "    x"                                                                                                        
## [31] "})"                                                                                                           
## [32] ""                                                                                                             
## [33] ""                                                                                                             
## [34] ""                                                                                                             
## [35] "## Generated accessors for slot c"                                                                            
## [36] ""                                                                                                             
## [37] "if (!exists(\"c\") || !isGeneric(\"c\")) setGeneric(\"c\", function(x) standardGeneric(\"c\"))"               
## [38] ""                                                                                                             
## [39] "setMethod(\"c\", \"a\", function(x) x@c)"                                                                     
## [40] ""                                                                                                             
## [41] ""                                                                                                             
## [42] "if (!exists(\"c<-\") || !isGeneric(\"c<-\")) setGeneric(\"c<-\", function(x, value) standardGeneric(\"c<-\"))"
## [43] ""                                                                                                             
## [44] "setMethod(\"c<-\", \"a\", function(x, value) {"                                                               
## [45] "    x@c = value"                                                                                              
## [46] "    x"                                                                                                        
## [47] "})"                                                                                                           
## [48] ""                                                                                                             
## [49] ""
```

# A worked example

Here we will define a simple class and methods which operate on it. We will define
a csvdf class, which will extend data.frame with slots for storing the
file it was read from and an in-meory representation of any header information
in the CSV file. We note that this is a toy example and more care would
need to go into the design of a production version of this class. 


Note we only ask for header and csvfile getters and setters currently, rather
than ALL, because the S4 class representing data.frame has a "names" slot which
causes problems with the generated code because it is implicit. Future versions
will handle this issue more automatically.


```r
##^ @class csvdf
##^ @contains data.frame
##^ @slot header ANY 
##^ @slot csvfile character
##^ @accessors header<- csvfile<-
NULL
```

```
## NULL
```

```r
make_csvdf = function(file) {
    df = read.csv(file, comment.char = "#")
    lines = readLines(file)
    hstop = min(grep("^#", lines, invert=TRUE) )-1
    if(hstop>0)
	header = lines[1:hstop]
    else
	header = character()
    new("csvdf", df, header = header, csvfile = file)
}

##^ @method show
##^ @sigType object csvdf
function(object) {
    cat("A CSV-derived data.frame with ", length(header(object)),
        " lines of header information\n",
	"\tfile: ", csvfile(object), "\n",
	"head:\n")
    print.data.frame(head(object))
}
```

```
## function(object) {
##     cat("A CSV-derived data.frame with ", length(header(object)),
##         " lines of header information\n",
## 	"\tfile: ", csvfile(object), "\n",
## 	"head:\n")
##     print.data.frame(head(object))
## }
```

Here we do some trickery to have our generated classes and methods
available while knitting this vignette.  You don't need to worry
about this, but I will leave it visible for completeness.


```r
library(S4Coffee)
library(knitr)
opts =options(knitr.duplicate.label="allow")
tangled = tempfile(fileext = ".R")
purl("S4Coffee.Rmd", output = tangled)
```

```
## 
## 
## processing file: S4Coffee.Rmd
```

```
##   |                                                                         |                                                                 |   0%  |                                                                         |..                                                               |   4%  |                                                                         |.....                                                            |   8%  |                                                                         |........                                                         |  12%  |                                                                         |..........                                                       |  15%  |                                                                         |............                                                     |  19%  |                                                                         |...............                                                  |  23%  |                                                                         |..................                                               |  27%  |                                                                         |....................                                             |  31%  |                                                                         |......................                                           |  35%  |                                                                         |.........................                                        |  38%  |                                                                         |............................                                     |  42%  |                                                                         |..............................                                   |  46%  |                                                                         |................................                                 |  50%  |                                                                         |...................................                              |  54%  |                                                                         |......................................                           |  58%  |                                                                         |........................................                         |  62%  |                                                                         |..........................................                       |  65%  |                                                                         |.............................................                    |  69%  |                                                                         |................................................                 |  73%  |                                                                         |..................................................               |  77%  |                                                                         |....................................................             |  81%  |                                                                         |.......................................................          |  85%  |                                                                         |..........................................................       |  88%  |                                                                         |............................................................     |  92%  |                                                                         |..............................................................   |  96%  |                                                                         |.................................................................| 100%
```

```
## output file: /var/folders/79/l_n_5qr152d2d9d9xs0591lh0000gn/T//Rtmp6eqicM/filecdf83eb3aa0f.R
```

```
## [1] "/var/folders/79/l_n_5qr152d2d9d9xs0591lh0000gn/T//Rtmp6eqicM/filecdf83eb3aa0f.R"
```

```r
options(opts)
parsed = parse_file(tangled)
gened = tempfile(fileext=".R")
writeCode(parsed, outfile = gened, append=FALSE)
eval(parse(gened), envir = knit_global())
```

```
## [1] "show"
```





```r
tmpfile  = tempfile(fileext=".csv")
cat("# dataset:iris\n# source: R\n", file = tmpfile)
write.table(iris, file = tmpfile, append=TRUE, sep=",")
```

```
## Warning in write.table(iris, file = tmpfile, append = TRUE, sep = ","):
## appending column names to file
```

```r
thing = make_csvdf(tmpfile)
print(class(thing))
```

```
## [1] "csvdf"
## attr(,"package")
## [1] ".GlobalEnv"
```

```r
## we need the explicit show call here because apparently kntir (or evaluate?)
## doesn't respect show methods.
show(thing)
```

```
## A CSV-derived data.frame with  2  lines of header information
##  	file:  /var/folders/79/l_n_5qr152d2d9d9xs0591lh0000gn/T//Rtmp6eqicM/filecdf83ca6f3cb.csv 
##  head:
##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 1          5.1         3.5          1.4         0.2  setosa
## 2          4.9         3.0          1.4         0.2  setosa
## 3          4.7         3.2          1.3         0.2  setosa
## 4          4.6         3.1          1.5         0.2  setosa
## 5          5.0         3.6          1.4         0.2  setosa
## 6          5.4         3.9          1.7         0.4  setosa
```



# Licensing and ownership of generated code

Code generated via S4Coffee is owned and copyright the user of our package,
and can be licensed in the manner of their choosing.

We request that our users not remove the notice at the top of files generated
by S4Coffee when distributing those files publicly.

# Acknowledgement


We further ask that S4Coffee and its authors be acknowledged, in whatever manner
is appropriate for the setting, when users publish about R pacakges for which
S4Coffee played a substantial role in development.

A citation for S4Coffee can be obtained by calling


```r
citation(package="S4Coffee")
```

```
## 
## To cite package 'S4Coffee' in publications use:
## 
##   Gabriel Becker (2017). S4Coffee: Tools for generating S4 classes
##   and methods. R package version 0.2.4.
## 
## A BibTeX entry for LaTeX users is
## 
##   @Manual{,
##     title = {S4Coffee: Tools for generating S4 classes and methods},
##     author = {Gabriel Becker},
##     year = {2017},
##     note = {R package version 0.2.4},
##   }
## 
## ATTENTION: This citation information has been auto-generated from
## the package DESCRIPTION file and may need manual editing, see
## 'help("citation")'.
```

