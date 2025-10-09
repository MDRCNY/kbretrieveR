# tests/testthat/test-basic.R
test_that("kbretrieveR loads and exposes expected exports", {
  # Namespace should be loaded by tests/testthat.R
  expect_true("kbretrieveR" %in% loadedNamespaces())
  
  exps <- getNamespaceExports("kbretrieveR")
  expect_true(length(exps) >= 1)
  
  # Your current public API (as reported) — allow future growth
  expected <- c("kb_retrieve_httr", "KBClient", "kb_chat_with_ellmer")
  
  # At least one of these must be present
  expect_true(any(expected %in% exps),
              info = paste("Exports were:", paste(exps, collapse = ", ")))
  
  # Optional light sanity checks that don’t touch the network:
  # KBClient should be a callable object (e.g., an R6 generator or function)
  if ("KBClient" %in% exps) {
    obj <- get("KBClient", asNamespace("kbretrieveR"))
    expect_true(is.function(obj) || is.environment(obj))
  }
})
