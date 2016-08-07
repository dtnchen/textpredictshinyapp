textpredict <- function(string) {

foo <- tolower(string)

#check if ngrams have been loaded, if not load the ngram datasets

if (!exists("onegrams")) onegrams <- readRDS("onegrams_twit0p03.rds")
if (!exists("twograms")) twograms <- readRDS("twograms_twit0p03.rds")
if (!exists("threegrams")) threegrams <- readRDS("threegrams_twit0p03.rds")
if (!exists("fourgrams")) fourgrams <- readRDS("fourgrams_twit0p03.rds")

#filter out multiple and trailing spaces from input text
trim <- function(x) return(gsub("^ *|(?<= ) | *$", "", x, perl=T))
foo <- trim(foo)
#count number of words in input text; this is the (n-1)gram
if (length(unlist(strsplit(foo," "))) >= 3) {
        #grab the last three words and check for fourgram matches
        foo4 <- word(foo,start=-3L, end=-1L)
        res4 <- stringmatch(fourgrams, foo4)
        if (is.null(res4)) {
                foo3 <- word(foo,start=-2L, end=-1L)    #backoff look at threegram matches
                res3 <- stringmatch(threegrams, foo3)
                if (is.null(res3)) {
                        foo2 <- word(foo,start=-1L, end=-1L)    #backoff look at twogram matches
                        res2 <- stringmatch(twograms, foo2)
                        if (is.null(res2)) {
                                res1 <- onegrams %>% mutate(Count = alpha*Count*disc/sum(Count)) %>% arrange(desc(Count))
                                res1[1:min(10,dim(res1)[1]),]
                                #res1$String[1:min(10,dim(res1)[1])]
                        } else {
                                #show the top 5 twogram matches sorted by probability
                                res2 <- res2 %>% mutate(Count = alpha*Count*disc/sum(Count)) %>% arrange(desc(Count))
                                res2[1:min(10,dim(res2)[1]),]
                                #word(res2$String[1:min(10,dim(res2)[1])],start=-1L, end=-1L)
                        }
                } else {
                        #show the top 5 threegram matches sorted by probability
                        res3 <- res3 %>% mutate(Count = alpha*Count*disc/sum(Count)) %>% arrange(desc(Count))
                        res3[1:min(10,dim(res3)[1]),]
                        #word(res3$String[1:min(10,dim(res3)[1])],start=-1L, end=-1L)
                }
        } else {
                #show the top 5 fourgram matches sorted by probability
                res4 <- res4 %>% mutate(Count = Count*disc/sum(Count)) %>% arrange(desc(Count))
                res4[1:min(10,dim(res4)[1]),]
                #word(res4$String[1:min(10,dim(res4)[1])],start=-1L, end=-1L)
        }
} else if (length(unlist(strsplit(foo," "))) == 2) {
        #check for threegram matches
        res3 <- stringmatch(threegrams, foo)
        if (is.null(res3)) {
                foo2 <- word(foo,start=-1L, end=-1L)    #backoff look at twogram matches
                res2 <- stringmatch(twograms, foo2)
                if (is.null(res2)) {
                        #show the top 5 onegram matches
                        res1 <- onegrams %>% mutate(Count = alpha*Count*disc/sum(Count)) %>% arrange(desc(Count))
                        res1[1:min(10,dim(res1)[1]),]
                        #res1$String[1:min(10,dim(res1)[1])]
                } else {
                        #show the top 5 twogram matches sorted by probability
                        res2 <- res2 %>% mutate(Count = alpha*Count*disc/sum(Count)) %>% arrange(desc(Count))
                        res2[1:min(10,dim(res2)[1]),]
                        #word(res2$String[1:min(10,dim(res2)[1])],start=-1L, end=-1L)
                } 
        } else {
                #show the top 5 threegram matches sorted by probability
                res3 <- res3 %>% mutate(Count = Count*disc/sum(Count)) %>% arrange(desc(Count))
                res3[1:min(10,dim(res3)[1]),]
                #word(res3$String[1:min(10,dim(res3)[1])],start=-1L, end=-1L)
        }
} else if (length(unlist(strsplit(foo," "))) == 1) {
        #check for twogram matches
        res2 <- stringmatch(twograms, foo) 
                if (is.null(res2)) {
                        #show the top 5 onegram matches
                        res1 <- onegrams %>% mutate(Count = alpha*Count*disc/sum(Count)) %>% arrange(desc(Count))
                        res1[1:min(10,dim(res1)[1]),]
                        #res1$String[1:min(10,dim(res1)[1])]
                } else { 
                        #show the top 5 twogram matches sorted by probability
                        res2 <- res2 %>% mutate(Count = Count*disc/sum(Count)) %>% arrange(desc(Count))
                        res2[1:min(10,dim(res2)[1]),]
                        #word(res2$String[1:min(10,dim(res2)[1])],start=-1L, end=-1L)
                }
} else {
        #show the top 5 highest prob onegrams
        res1 <- onegrams %>% mutate(Count = Count*disc/sum(Count)) %>% arrange(desc(Count))
        res1[1:min(10,dim(res1)[1]),]
        #res1$String[1:min(10,dim(res1)[1])]
}
}
