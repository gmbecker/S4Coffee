##dirComment = "^##'[[:space:]]*"
dirComment = "^##\\^[[:space:]]*"
dirPat = paste0(dirComment, "@")
dirNamePat = paste0(dirPat, "([[:alpha:]]+)")
dirNamePatGreedy = paste0(dirNamePat, ".*")

makeRecipe = function(comment, ref) {
    comment = as.character(comment)
    comment = comment[nchar(comment) > 0]
    ##    comment = comment[grepl("^#{1,2}'", comment)]
    comment = comment[grepl("^#{1,2}\\^", comment)]
    
    if(length(comment) == 1)
        comment = strsplit(comment, "\n")[[1]]
    dirspots = grepl(dirPat, comment)

    directives = split(comment, cumsum(dirspots))
    names(directives) = sapply(directives, function(x)
             gsub(dirNamePatGreedy, "\\1", x[1]))

    directives = DirectiveList(mapply(makeDirective, directives, names(directives)))
    if("class" %in% names(directives))
        S4ClassRecipe(recipe = comment, expr = ref,
                      directives = directives)
    else if("method" %in% names(directives))
        S4MethodRecipe(recipe = comment, expr = ref,
                       directives = directives)
    else if("generic" %in% names(directives))
        S4GenericRecipe( recipe = comment, expr = ref,
                        directives = directives)
}

## Given the comment lines and an EntryPattern object, generate a list of directive contents

readDirective = function(txt, pat)
{
    ## txt = gsub("^##'[[:space:]]*(@[[:alpha:]]+[[:space:]]+){0,1}(.*)", "\\2", txt)
    txt = gsub("^##\\^[[:space:]]*(@[[:alpha:]]+[[:space:]]+){0,1}(.*)", "\\2", txt)
    txt2 = paste(txt, collapse = " ")
    spl = strsplit(txt2, "[[:space:]]+")[[1]]
    numsingle = single_words(pat)
    if(is.na(numsingle))
        ret = list(spl) ## list with 1 element containing vec of single words
    else  if (numsingle >=1){
        ret  = list()
        for(i in 1:single_words(pat)) 
            ret[[i]] = spl[i]
        if(freeform_end(pat))
            ret[[numsingle + 1]] = paste(tail(spl, -1*numsingle),
                   collapse = " ")
    } else {
        ret = list()
    }
    ret
}

makeDirective = function(txt, dirtype) {
    pat = entryPatterns[[dirtype]]
    rawdir = readDirective(txt, pat)
    dirconst = switch(dirtype,
        slot = SlotDirective,
        class = ClassDirective,
        accessors = AccessorsDirective,
        method = MethodDirective,
        contains = ContainsDirective,
        sigType = SigTypeDirective,
        slotDefault = SlotDefaultDirective,
        generic = GenericDirective,
        defaultMethod = DefaultMethodDirective)
    do.call(dirconst, rawdir)
}
