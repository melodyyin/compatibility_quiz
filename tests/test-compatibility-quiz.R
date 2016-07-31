test_that("scores do not contain NAs", {
  expect_false(anyNA(mc_scores$score))
  expect_false(anyNA(raw2$answer)) 
  expect_false(anyNA(mc_choices$choice)) 
})

test_that("to_binary_vector is a binary vector", {
  rand_qa = sample_n(mc_choices, 1) 
  
  expect_false(anyNA(to_binary_vector(rand_qa$question, rand_qa$choice)))
  expect_is(to_binary_vector(rand_qa$question, rand_qa$choice), "numeric")
  expect_true(all(c(0,1) %in% to_binary_vector(rand_qa$question, rand_qa$choice)))
})

test_that("to_sentiment_score returns the correct word count", {
  expect_is(to_sentiment_score("something"), "integer")
  expect_equal(to_sentiment_score("something or another"), 3)
  expect_equal(to_sentiment_score(NA), -1)
})

test_that("get_matrix has colSum of 1", {
  rand_q = sample(unique(raw$question), 1)
  
  expect_is(get_matrix(rand_q), "matrix")
  expect_equal(unique(colSums(get_matrix(rand_q))), 1)
})

test_that("everyone has matches", {
  expect_true(!anyNA(results_df)) 
  expect_equal(length(unique(raw$user_id)), length(unique(results_df$user_name)))
})