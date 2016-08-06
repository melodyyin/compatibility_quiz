test_that("scores do not contain NAs", {
  expect_false(anyNA(mc_scores$score))
  expect_false(anyNA(raw2$answer)) 
  expect_false(anyNA(mc_choices$choice)) 
})

test_that("to_binary_vector is a binary vector", {
  test_qa = list(question="q5", choice="choice1")  
  test_possibs = data.frame(question=rep("q5", 5),
                            choice=c("choice1", "choice2", "choice3", "choice4", "choice5"))
  
  expect_false(anyNA(to_binary_vector(test_qa$question, test_qa$choice, test_possibs))) 
  expect_is(to_binary_vector(test_qa$question, test_qa$choice, test_possibs), "numeric")
  expect_equal(to_binary_vector(test_qa$question, test_qa$choice, test_possibs), c(1,0,0,0,0))
  expect_equal(to_binary_vector(test_qa$question, "choice2", test_possibs), c(0,1,0,0,0))
  expect_error(to_binary_vector(test_qa$question, "choice10", test_possibs))
})

test_that("to_sentiment_score returns the correct word count", {
  expect_is(to_sentiment_score("something"), "integer")
  expect_equal(to_sentiment_score("something or another"), 3)
  expect_equal(to_sentiment_score(NA), -1)
})

test_that("get_matrix has colSum of 1", {
  expect_is(get_matrix("q5"), "matrix")
  expect_equal(unique(colSums(get_matrix("q5"))), 1)
})

test_that("everyone has matches", {
  expect_true(!anyNA(results_df)) 
  expect_equal(length(unique(raw$user_id)), length(unique(results_df$user_name)))
})