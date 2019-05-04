#' Interview data - closed questions
#'
#' Results of interviews with rangers in the period from 2019-01-17 to 2019-01-30.
#' Interviews were carried out on the basis of anonymity.
#' Closed questions were scored on a Likert 1-5 scale.
#' Additional comments are included.
#'
#' @name: closed_questions
#' @format: data.table with 552 records and 5 fields.
#' @source ./data-raw/interviews_read_clean.R.
#' \describe{
#'     \item{ID}{unique serial number}
#'     \item{Question}{Question Statement}
#'     \item{QuestionExplain}{Explanation of the Question}
#'     \item{Score}{Agreement with with Statement on 1-5 Likert scale range}
#'     \item{Explain_Notes}{Ranger's explanation for the given score}
#' }
#' @keywords interviews, rangers, Likert, closed
"closed_questions"

# -------------------------------------------------------------------------------

#' No observations and violence
#'
#' Derived from closed questions and open questions.
#' @name: freq_poachers
#' @source interviews_read_clean.R
#' @keyword interviews, rangers, violence, frequencies, observations
"freq_poachers"


# -------------------------------------------------------------------------------

#' Interview data - closed question scores
#'
#' Results of interviews with rangers in the period from 2019-01-17 to 2019-01-30.
#' Interviews were carried out on the basis of anonymity.
#' Closed questions were scored on a Likert 1-5 scale.
#' The frequency of each Likert score per question was calculated.
#' The Tukey 5 number summary was also calculated per question (minimum, 25% quantile, 50% quantile (median), 75% quantile, maximum).
#'
#' @name: closed_question_scores
#' @format: data.table with 23 records and 13 fields.
#' \describe{
#'     \item{Question}{Question}
#'     \item{Nr}{Question Nr}
#'     \item{Score 1:5}{Likert score}
#'     \item{No opinion}{No Opinion}
#'     \item{Min, Q25, Q50, Q75, Q75, Max}{Tukey's five number summary}
#' }
#' @keywords interviews, rangers, Likert, closed
"closed_question_scores"


# --------------------------------------------------------------

#' Interview data - open questions
#'
#' Results of interviews with rangers in the period from 2019-01-17 to 2019-01-30.
#' Interviews were carried out on the basis of anonymity.
#' Open questions.
#'
#' @name: open_questions
#' @format data.table with 240 records and 3 fields.
#' \describe{
#'     \item{Nr}{Question number}
#'     \item{Question}{Interview open question}
#'     \item{Answer}{Ranger's answer}
#' }
"open_questions"


# -------------------------------------------------------------

#' Interview data - clue questions
#'
#' Results of interviews with rangers in the period from 2019-01-17 to 2019-01-30.
#' Interviews were carried out on the basis of anonymity.
#' Clue questions: what is the most important clue for finding poachers?
#'
#' @name: clue_questions
#' @format vector of length 14
#' \describe{
#'     \item{clue_question}{What is the most important variable to look out for if you want to find poachers?}
#' }
"clue_questions"


# ------------------------------------------------------------

#' Interview data - snare_estimates
#' Before desnaring, each ranger could submit an estimate for the number of snares we would find during a particular transit.
#' The ranger that submitted the estimate closest the the actual number of snares found received 500 KSh (5 USD).
#'
#' @name: snare_estimates_raw
#' @format data.table with 162 records and 9 fields.
#' \describe{
#'     \item{area}{The name of the area where the transect took place.}
#'     \item{transect_date}{Date of desnaring of the particular transect.}
#'     \item{snares_found}{The number of snares found (both dead and live).}
#'     \item{snares_estimated}{The number of snares estimated by the rangers.}
#'     \item{n}{the number of rangers participating in the estimate.}
#'     \item{min}{the minimum of the estimate range.}
#'     \item{max}{the maximum of the estimate range.}
#'     \item{group_avg}{group average of the estimate.}
#'     \item{in_range}{whether min estimate > snares found & max estimate < snares found.}
#' }
"snare_estimates_raw"


# ---------------------------------------------------------------

#' Interview data - snare_estimates
#' Before desnaring, each ranger could submit an estimate for the number of snares we would find during a particular transit.
#' The ranger that submitted the estimate closest the the actual number of snares found received 500 KSh (5 USD).
#' The raw data were processed in order to assess whether (1) the snares found was in range of the estimates of the rangers;
#' (2) indicators for calculating group scores, namely PD, IE, CE. See Hong, 2004 for details.
#'
#' @name: snare_estimates
#' @format data.table with 24 records and 13 fields.
#' \describe{
#'    \item{area}{subarea of the Soysambu conservation site.}
#'    \item{n}{number of estimates made (= number of participating rangers).}
#'    \item{min}{minimum of the estimate range.}
#'    \item{max}{maximum of the estimate range.}
#'    \item{snares_found}{number of snares found during the transect.}
#'    \item{PD}{Prediction Diversity.}
#'    \item{IE}{Average Individual Error.}
#'    \item{CE}{Collective Error = IE-PD}
#'    \item{PDsq}{squared PD, which brings the units back to snare count.}
#'    \item{IEsq}{squared IE}
#'    \item{CEsq}{CEsq=IEsq-PDsq}
#'    \item{r}{ratio of CE and  IE: r=CE/IE.}
#' }
"snare_estimates"

# --------------------------------------------------------

#' Interview data - years of experience of rangers
#'
#' During interviews, each ranger was asked to give the number of years of experience on the Soysambu ranch in the function of ranger.
#'
#' @name: years_experience_rangers
#' @format numeric vector
"years_experience_rangers"


# -------------------------------------------------

#' Interview data - corpus of open and closed interview answers
#'
#' The answers of open and closed interview questions were combined and put into a corpus.
#' This is accessible through the quanteda library.
#'
#' @name: interview_corpus
#' @format quanteda corpus
#' @examples
#' require(quanteda)
#' quanteda::kwic(interview_corpus, "supervisor*", window=20, valuetype="regex")
"interview_corpus"
