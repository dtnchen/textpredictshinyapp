#library(stringi)
stringmatch <- function(ngram,string) {
        string <- paste(string,'')
        foo <- stri_startswith_fixed(ngram$String,string)
        if (sum(foo) > 0) ngram[foo,]
        #foo <- gregexpr(string, ngram$String,fixed=TRUE)
        #bar <- lapply(foo, function(x) attributes(x)$match.length == nchar(string) & x == 1) 
        #if (sum(as.numeric(bar)) > 0) ngram[which(as.numeric(bar) == 1),]
}