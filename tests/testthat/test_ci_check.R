context('ci_check')

test_that('ci_check output validity', {
    expect_equal(coreSim:::ci_check(95), 0.95)
    expect_equal(coreSim:::ci_check(1), 1)
    expect_equal(coreSim:::ci_check(0.5), 0.5)
    expect_error(coreSim:::ci_check(500))
})
