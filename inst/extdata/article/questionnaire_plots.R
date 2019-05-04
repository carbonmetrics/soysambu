pacman::p_load(data.table, forcats, maditr, viridis, ggplot2, Soysambu)
data(closed_questions)
d=closed_questions

a0=unique(d, by=c("Question", "QuestionExplain"))[, .(Question, QuestionExplain)]
occurrence=c(1,2,16,17)
where=c(4,5:10)
who_knows_who=c(11,12,14,15)
deterrence=c(3,13,18,21)
patrolling=c(19,20,22,23)
a0[Question %in% occurrence, Category := "Perspective"]
a0[Question %in% where, Category := "Places"]
a0[Question %in% who_knows_who, Category := "Relations"]
a0[Question %in% deterrence, Category := "Deterrence"]
a0[Question %in% patrolling, Category := "Patrolling"]

a1=d[, .N, .(Question, QuestionExplain, Score)]
a2=d[, .N, .(Question, QuestionExplain)]
a3=a2[a1,on=c("Question", "QuestionExplain")]
a3[, perc := {T
  t1=i.N/N
  t2=t1*100
}]
a4=a3[, .(Question, QuestionExplain, Score, perc, i.N)]
a5=closed_questions[, .(agree=mean(Score, na.rm=T)), Question]
a6=a5[a4,on="Question"]
setorder(a6, -agree,QuestionExplain)

a6[, QuestionExplain := {
  t1=fct_inorder(QuestionExplain)
  t2=fct_rev(t1)
}]

a7=a0[a6, on=c("Question","QuestionExplain")]

# clean up score
scores=data.table(
  Score=c(1:5, NA),
  Scores=c("Strongly disagree","Disagree", "Neutral","Agree","Strongly agree", "No opinion")
)

a8=scores[a7, on="Score"]
setorder(a8, -agree, Category)

a8[, `:=` (
  QuestionExplain = fct_inorder(QuestionExplain),
  Category = fct_inorder(Category),
  Scores=factor(Scores, levels=c("Strongly disagree","Disagree", "Neutral","Agree","Strongly agree", "No opinion"))
)]

a8[, Scores := fct_rev(Scores)]
a8[, QuestionExplain := fct_rev(QuestionExplain)]

mycats=c("Relations","Deterrence")
a9=a8[Category %in% mycats,]

a9[, QuestionExplain := stringr::str_wrap(QuestionExplain, width=15)]


interviews = ggplot(a9) +
  geom_col(aes(QuestionExplain, i.N, fill=Scores), width=0.5) +
  coord_flip() +
  scale_fill_viridis(discrete=T, alpha=0.8, direction=-1) +
  facet_grid(Category~.,scales="free",space="free") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0, colour="grey50")) +
  theme(strip.background = element_rect(fill="white", colour="grey70")) +
  labs(x="",y="Number of rangers",
       title="Interviews with 30 rangers in Soysambu conservancy",
       subtitle="Results on Likert scale, 1-5")



# overnieuw ======

d=closed_questions

# join with categories --------------------

b1=a0[d, on=c("Question","QuestionExplain")]

# calculate average score (median! this is a Likert scale)
b2=d[, .(median = median(Score, na.rm=T)), Question]

b3=b1[b2, on="Question"]

setorder(b3, Category, -median)
b3[, QuestionExplain := {
  t1=fct_inorder(QuestionExplain)
  t2=fct_rev(t1)
}]

# ggplot(b3, aes(x=QuestionExplain, y=median, fill=Category)) +
#   geom_segment(aes(x=QuestionExplain, xend=QuestionExplain, y=0, yend=median), col="grey40") +
#   geom_point(size=3, aes(col=Category)) +
#   coord_flip() +
#   theme_light() +
#   facet_grid(Category~.,scales="free",space="free") +
#   theme(strip.text.y = element_text(angle = 0, colour="black")) +
#   theme(strip.background = element_rect(fill="white", colour="grey60"))


