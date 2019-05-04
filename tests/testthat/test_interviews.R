context("Test interviews")

data(closed_questions)
crangers=closed_questions[, unique(ranger.id)]
cquestions=closed_questions[, unique(Question)]

data(open_questions)
orangers=open_questions[, unique(ranger.id)]
oquestions=open_questions[, unique(Nr)]

# closed questions
test_that("interview questions are set up correctly", {
  expect_is(closed_questions, "data.table")
  expect_true(all(diff(crangers) == 1))
  expect_equal(max(crangers), 30)
  expect_true(all(diff(cquestions) == 1))
  expect_equal(max(cquestions), 23)
  expect_is(closed_questions$Score, "integer")
})

# years experience
test_that("years experience is set up correctly", {
  expect_true(length(years_experience_rangers) == max(crangers))
})

# open questions
test_that("open questions are set up correctly", {
  expect_is(open_questions, "data.table")
  expect_true(all(diff(orangers) == 1))
  expect_equal(max(orangers), max(crangers))
  expect_equal(max(oquestions), 10)
})

# snare_estimates
n.transects=snare_estimates$area %>% unique %>% length

test_that("snare estimates are set up correctly", {
  expect_equal(n.transects, 27)
  expect_true(snare_estimates[area=="Congreve", snares_found] == 7)
  expect_true(snare_estimates[area=="Melia plot", snares_found] == 9)
  expect_true(snare_estimates[area=="Serena B", snares_found] == 35)
})

# frequency of poacher sightings and violence
test_that("Frequency of poacher sightings and violence events are set up correctly", {
  expect_equal(nrow(freq_poachers), 30)
  expect_is(freq_poachers$Freqd, "numeric")
})




