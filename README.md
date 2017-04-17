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
## output file: /var/folders/79/l_n_5qr152d2d9d9xs0591lh0000gn/T//RtmpVnrSqN/filec71817b484e8.R
```

```
## [1] "/var/folders/79/l_n_5qr152d2d9d9xs0591lh0000gn/T//RtmpVnrSqN/filec71817b484e8.R"
```

```r
options(opts)
parsed = parse_file(tangled)
gened = tempfile(fileext=".R")
writeCode(parsed, outfile = gened, append=FALSE)
eval(parse(gened), envir = knit_global())
```

```
## Error in parse(gened): /var/folders/79/l_n_5qr152d2d9d9xs0591lh0000gn/T//RtmpVnrSqN/filec7184c81a780.R:13:102: unexpected 'if'
## 12: 
## 13: setClass("myclass", representation(a = "numeric", b = "character"), prototype(a = 5, b = "whaaaat")) if
##                                                                                                          ^
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
## Object of class "csvdf"
##     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
## 1            5.1         3.5          1.4         0.2     setosa
## 2            4.9         3.0          1.4         0.2     setosa
## 3            4.7         3.2          1.3         0.2     setosa
## 4            4.6         3.1          1.5         0.2     setosa
## 5            5.0         3.6          1.4         0.2     setosa
## 6            5.4         3.9          1.7         0.4     setosa
## 7            4.6         3.4          1.4         0.3     setosa
## 8            5.0         3.4          1.5         0.2     setosa
## 9            4.4         2.9          1.4         0.2     setosa
## 10           4.9         3.1          1.5         0.1     setosa
## 11           5.4         3.7          1.5         0.2     setosa
## 12           4.8         3.4          1.6         0.2     setosa
## 13           4.8         3.0          1.4         0.1     setosa
## 14           4.3         3.0          1.1         0.1     setosa
## 15           5.8         4.0          1.2         0.2     setosa
## 16           5.7         4.4          1.5         0.4     setosa
## 17           5.4         3.9          1.3         0.4     setosa
## 18           5.1         3.5          1.4         0.3     setosa
## 19           5.7         3.8          1.7         0.3     setosa
## 20           5.1         3.8          1.5         0.3     setosa
## 21           5.4         3.4          1.7         0.2     setosa
## 22           5.1         3.7          1.5         0.4     setosa
## 23           4.6         3.6          1.0         0.2     setosa
## 24           5.1         3.3          1.7         0.5     setosa
## 25           4.8         3.4          1.9         0.2     setosa
## 26           5.0         3.0          1.6         0.2     setosa
## 27           5.0         3.4          1.6         0.4     setosa
## 28           5.2         3.5          1.5         0.2     setosa
## 29           5.2         3.4          1.4         0.2     setosa
## 30           4.7         3.2          1.6         0.2     setosa
## 31           4.8         3.1          1.6         0.2     setosa
## 32           5.4         3.4          1.5         0.4     setosa
## 33           5.2         4.1          1.5         0.1     setosa
## 34           5.5         4.2          1.4         0.2     setosa
## 35           4.9         3.1          1.5         0.2     setosa
## 36           5.0         3.2          1.2         0.2     setosa
## 37           5.5         3.5          1.3         0.2     setosa
## 38           4.9         3.6          1.4         0.1     setosa
## 39           4.4         3.0          1.3         0.2     setosa
## 40           5.1         3.4          1.5         0.2     setosa
## 41           5.0         3.5          1.3         0.3     setosa
## 42           4.5         2.3          1.3         0.3     setosa
## 43           4.4         3.2          1.3         0.2     setosa
## 44           5.0         3.5          1.6         0.6     setosa
## 45           5.1         3.8          1.9         0.4     setosa
## 46           4.8         3.0          1.4         0.3     setosa
## 47           5.1         3.8          1.6         0.2     setosa
## 48           4.6         3.2          1.4         0.2     setosa
## 49           5.3         3.7          1.5         0.2     setosa
## 50           5.0         3.3          1.4         0.2     setosa
## 51           7.0         3.2          4.7         1.4 versicolor
## 52           6.4         3.2          4.5         1.5 versicolor
## 53           6.9         3.1          4.9         1.5 versicolor
## 54           5.5         2.3          4.0         1.3 versicolor
## 55           6.5         2.8          4.6         1.5 versicolor
## 56           5.7         2.8          4.5         1.3 versicolor
## 57           6.3         3.3          4.7         1.6 versicolor
## 58           4.9         2.4          3.3         1.0 versicolor
## 59           6.6         2.9          4.6         1.3 versicolor
## 60           5.2         2.7          3.9         1.4 versicolor
## 61           5.0         2.0          3.5         1.0 versicolor
## 62           5.9         3.0          4.2         1.5 versicolor
## 63           6.0         2.2          4.0         1.0 versicolor
## 64           6.1         2.9          4.7         1.4 versicolor
## 65           5.6         2.9          3.6         1.3 versicolor
## 66           6.7         3.1          4.4         1.4 versicolor
## 67           5.6         3.0          4.5         1.5 versicolor
## 68           5.8         2.7          4.1         1.0 versicolor
## 69           6.2         2.2          4.5         1.5 versicolor
## 70           5.6         2.5          3.9         1.1 versicolor
## 71           5.9         3.2          4.8         1.8 versicolor
## 72           6.1         2.8          4.0         1.3 versicolor
## 73           6.3         2.5          4.9         1.5 versicolor
## 74           6.1         2.8          4.7         1.2 versicolor
## 75           6.4         2.9          4.3         1.3 versicolor
## 76           6.6         3.0          4.4         1.4 versicolor
## 77           6.8         2.8          4.8         1.4 versicolor
## 78           6.7         3.0          5.0         1.7 versicolor
## 79           6.0         2.9          4.5         1.5 versicolor
## 80           5.7         2.6          3.5         1.0 versicolor
## 81           5.5         2.4          3.8         1.1 versicolor
## 82           5.5         2.4          3.7         1.0 versicolor
## 83           5.8         2.7          3.9         1.2 versicolor
## 84           6.0         2.7          5.1         1.6 versicolor
## 85           5.4         3.0          4.5         1.5 versicolor
## 86           6.0         3.4          4.5         1.6 versicolor
## 87           6.7         3.1          4.7         1.5 versicolor
## 88           6.3         2.3          4.4         1.3 versicolor
## 89           5.6         3.0          4.1         1.3 versicolor
## 90           5.5         2.5          4.0         1.3 versicolor
## 91           5.5         2.6          4.4         1.2 versicolor
## 92           6.1         3.0          4.6         1.4 versicolor
## 93           5.8         2.6          4.0         1.2 versicolor
## 94           5.0         2.3          3.3         1.0 versicolor
## 95           5.6         2.7          4.2         1.3 versicolor
## 96           5.7         3.0          4.2         1.2 versicolor
## 97           5.7         2.9          4.2         1.3 versicolor
## 98           6.2         2.9          4.3         1.3 versicolor
## 99           5.1         2.5          3.0         1.1 versicolor
## 100          5.7         2.8          4.1         1.3 versicolor
## 101          6.3         3.3          6.0         2.5  virginica
## 102          5.8         2.7          5.1         1.9  virginica
## 103          7.1         3.0          5.9         2.1  virginica
## 104          6.3         2.9          5.6         1.8  virginica
## 105          6.5         3.0          5.8         2.2  virginica
## 106          7.6         3.0          6.6         2.1  virginica
## 107          4.9         2.5          4.5         1.7  virginica
## 108          7.3         2.9          6.3         1.8  virginica
## 109          6.7         2.5          5.8         1.8  virginica
## 110          7.2         3.6          6.1         2.5  virginica
## 111          6.5         3.2          5.1         2.0  virginica
## 112          6.4         2.7          5.3         1.9  virginica
## 113          6.8         3.0          5.5         2.1  virginica
## 114          5.7         2.5          5.0         2.0  virginica
## 115          5.8         2.8          5.1         2.4  virginica
## 116          6.4         3.2          5.3         2.3  virginica
## 117          6.5         3.0          5.5         1.8  virginica
## 118          7.7         3.8          6.7         2.2  virginica
## 119          7.7         2.6          6.9         2.3  virginica
## 120          6.0         2.2          5.0         1.5  virginica
## 121          6.9         3.2          5.7         2.3  virginica
## 122          5.6         2.8          4.9         2.0  virginica
## 123          7.7         2.8          6.7         2.0  virginica
## 124          6.3         2.7          4.9         1.8  virginica
## 125          6.7         3.3          5.7         2.1  virginica
## 126          7.2         3.2          6.0         1.8  virginica
## 127          6.2         2.8          4.8         1.8  virginica
## 128          6.1         3.0          4.9         1.8  virginica
## 129          6.4         2.8          5.6         2.1  virginica
## 130          7.2         3.0          5.8         1.6  virginica
## 131          7.4         2.8          6.1         1.9  virginica
## 132          7.9         3.8          6.4         2.0  virginica
## 133          6.4         2.8          5.6         2.2  virginica
## 134          6.3         2.8          5.1         1.5  virginica
## 135          6.1         2.6          5.6         1.4  virginica
## 136          7.7         3.0          6.1         2.3  virginica
## 137          6.3         3.4          5.6         2.4  virginica
## 138          6.4         3.1          5.5         1.8  virginica
## 139          6.0         3.0          4.8         1.8  virginica
## 140          6.9         3.1          5.4         2.1  virginica
## 141          6.7         3.1          5.6         2.4  virginica
## 142          6.9         3.1          5.1         2.3  virginica
## 143          5.8         2.7          5.1         1.9  virginica
## 144          6.8         3.2          5.9         2.3  virginica
## 145          6.7         3.3          5.7         2.5  virginica
## 146          6.7         3.0          5.2         2.3  virginica
## 147          6.3         2.5          5.0         1.9  virginica
## 148          6.5         3.0          5.2         2.0  virginica
## 149          6.2         3.4          5.4         2.3  virginica
## 150          5.9         3.0          5.1         1.8  virginica
## Slot "header":
## [1] "# dataset:iris" "# source: R"   
## 
## Slot "csvfile":
## [1] "/var/folders/79/l_n_5qr152d2d9d9xs0591lh0000gn/T//RtmpVnrSqN/filec7187511f06e.csv"
```

```
## NULL
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
##   and methods. R package version 0.2.3.1.
## 
## A BibTeX entry for LaTeX users is
## 
##   @Manual{,
##     title = {S4Coffee: Tools for generating S4 classes and methods},
##     author = {Gabriel Becker},
##     year = {2017},
##     note = {R package version 0.2.3.1},
##   }
## 
## ATTENTION: This citation information has been auto-generated from
## the package DESCRIPTION file and may need manual editing, see
## 'help("citation")'.
```

