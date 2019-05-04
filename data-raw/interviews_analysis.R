# analysis of interviews

# set up ---------

pacman::p_load(Soysambu, data.table, tmap, sf, raster, rnaturalearth, maditr, knitr, foreach, quanteda)

data(soysambu_boundaries, gps_points, gps_tracks, open_questions, closed_questions,
     snare_estimates, years_experience_rangers, clue_questions, census_data)

overwrite=T

# closed questions ====================

# calculate stats for closed questions --------

tab=closed_questions[, .N, .(Question, QuestionExplain, Score)]
dtab=dcast(tab, Question+QuestionExplain~Score, value.var="N")
mycols=names(dtab)[3:ncol(dtab)]
dtab[, (mycols) := lapply(.SD, function(x) ifelse(is.na(x),0,x)), .SDcols=mycols]
setnames(dtab, "NA","No opinion")

meds=closed_questions[, .(median=median(Score, na.rm=T)), .(Question, QuestionExplain)]

dtab=dtab[meds, on=c("Question","QuestionExplain")]
setnames(dtab, c("Question","QuestionExplain"),c("Nr","Question"))

# new sorting order for questions
sortvec = c(1,16,17,2,4,5,6,7,8,9,10,11,12,14,15,3,13,18,19,20,21,22,23)
setorder(dtab[, r := order(sortvec)], r)[, r := NULL]
closed_question_scores=dtab

# store
usethis::use_data(closed_question_scores, overwrite=overwrite)

# text analysis via text corpora ====================================================================

# prepare -----------------------------------------------------------------------------------------

# prepare open questions
open_questions[, Score := NA]
open_questions[Nr==1, Respondent := paste("ranger",1:.N)]
open_questions[, Respondent := zoo::na.locf(Respondent)]
open_questions[, Label := "Open questions"]

# prepare closed questions
closed_questions[, ID := NULL]
setnames(closed_questions, c("Nr","Question","Score","Answer", "ranger.id"))
closed_questions[Nr==1, Respondent := paste("ranger",1:.N)]
closed_questions[, Respondent := zoo::na.locf(Respondent)]
closed_questions[, Label := "Closed questions"]
setcolorder(closed_questions, names(open_questions))

# bind and make corpora -------------------------------------------------------------------------------
questions=rbind(open_questions, closed_questions)
corp=corpus(questions$Answer, docvars=questions)
mydfm=dfm(corp,
          remove=stopwords("english"),
          stem=F, remove_punct=T, ngrams=1)
topfeatures(mydfm)
set.seed(1964)
textplot_wordcloud(mydfm, min_count=6)

dict=dictionary(list(
  attack=c("attack","threat","home" ),
  ignore=c("ignore","run","other")
))

dfm_dict=dfm(corp, dictionary=dict)
dfm_lookup(mydfm, dict) # same
dft=convert(dfm_dict, "data.frame") %>% setDT
textstat_collocations(corp,size=4)

# https://stackoverflow.com/questions/45327556/quanteda-how-to-remove-my-own-list-of-words
tocs=tokens(corp, remove_punct=T)
toks=tokens_remove(tocs, stopwords("english"), padding=T)
tocs=tokens_ngrams(toks, n=3)
dfm(tocs) %>% topfeatures(n=50)

kwic(corp, "per")

corpus_subset(corp, Label=="Closed questions") %>%
  kwic("per", window=20)

corpus_subset(corp, Label=="Closed questions") %>%
  kwic("supervisor*", window=20, valuetype="regex")

subset(questions, Nr==10 & Label == "Open questions")

kwic(corp, "management", window=20, valuetype="regex")


# https://stackoverflow.com/questions/37445449/how-do-i-transform-a-kwic-from-quanteda-package-to-a-corpus
mykwic=kwic(corp, "per", window=20)
kwics=corpus(mykwic) %>% texts %>% as.data.frame %>% setDT(keep.rownames=T)
setnames(kwics, c("id","sentence"))
col1=kwics[id %like% ".pre"]
col2=kwics[id %like% ".post"]
kwix=cbind(col1, "per", col2)
setnames(kwix, c("id", "pre", "keyword", "idx", "post"))
kwix[, c("id", "idx"):= NULL]

# wordcloud: what needs to happen?
oq=corpus(open_questions[Nr==5, Answer])

mydfm=dfm(oq,
          remove=c(stopwords("english"),
                   c("rangers", "increase", "get", "stay","better","sure", "build", "good","add","make","solved","number","enough","problem","info",
                                          "low","also","people", "us","meet","small","many", "now","work","put","hot","exchange","improve","can","ends","moment","must","set",
                                          "develop","can","closer","already","need")),
          stem=F, remove_punct=T, ngrams=1)
textplot_wordcloud(mydfm, min_count=3)

# ditto: what is the biggest problem?
bp=corpus(open_questions[Nr==10,Answer])
mydfm=dfm(bp,
          remove=c(stopwords("english"),
                   c("can","time","therefore","know","get","sometimes", "poachers", "rangers")),
          stem=F, remove_punct=T, ngrams=1)
textplot_wordcloud(mydfm, min_count=3)


# save
interview_corpus=corp
usethis::use_data(interview_corpus, overwrite=overwrite)


# crosstables

library(gmodels)
f=function(x,df=closed_questions){
  dx=df[Nr==x, Score]
}

they.know=f(11)
transp=f(13)
able.stop=f(18)
pred=f(22)
ranger=f(3)

chisq.test(past,future, simulate.p.value=T)
library(coin)
mydata=data.table(past, future)
oneway_test(past~future, data=mydata, distribution="exact")

with(freq_poachers, chisq.test(No.report, Freqf, simulate.p.value=T))
