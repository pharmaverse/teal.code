describe("qenv inherits from environment: ", {
  it("is an environment", {
    expect_true(is.environment(qenv()))
  })

  it("names() shows nothing on empty environment", {
    expect_identical(names(qenv()), character(0))
  })

  it("names() shows available objets", {
    q <- within(qenv(), iris <- iris)
    expect_setequal(names(q), "iris")
  })

  it("names() shows hidden objects", {
    q <- within(qenv(), {
      iris <- iris
      .hidden <- 2
    })
    expect_setequal(names(q), c("iris", ".hidden"))
  })

  it("ls() does not show hidden objects", {
    q <- within(qenv(), {
      iris <- iris
      .hidden <- 2
    })
    expect_setequal(ls(q), c("iris"))
  })

  it("ls(all.names = TRUE) show all objects", {
    q <- eval_code(qenv(), "
      iris <- iris
      .hidden <- 2
    ")
    expect_setequal(ls(q, all.names = TRUE), c("iris", ".hidden"))
  })

  it("does not allow binding to be added", {
    q <- qenv()
    expect_error(q$x <- 1, "cannot add bindings to a locked environment")
  })

  it("does not allow binding to be modified", {
    q <- within(qenv(), obj <- 1)
    expect_error(q$obj <- 2, "cannot change value of locked binding for 'obj'")
  })
})

test_that("constructor returns qenv", {
  q <- qenv()
  expect_s4_class(q, "qenv")
  expect_identical(names(q), character(0))
  expect_identical(q@code, list())
})

describe("parent of qenv environment is the parent of .GlobalEnv", {
  it("via slot", {
    q <- qenv()
    expect_identical(parent.env(q@.xData), parent.env(.GlobalEnv))
  })

  it("via qenv directly", {
    q <- qenv()
    expect_identical(parent.env(q), parent.env(.GlobalEnv))
  })

  it("but doesn't mess up with library calls inside qenv", {
    skip_if_not_installed("knitr")
    expect_s4_class(q <- within(qenv(), {
      library(knitr)
      labels <- knitr::all_labels()
    }), "qenv")
  })

  it("cannot get variable after subsetting", {
    q <- qenv() |> within({
      a <- 2
      b <- 3
    })
    q2 <- q["a"]
    expect_error(get("b", envir = q2), "not found")
  })

  it("does not change the original environment when used to create", {
    e <- list2env(list(a = 2))
    q <- new("qenv", .xData = e)
    expect_no_error(e$b <- 3)
    expect_no_error(e$a <- 3)
    expect_equal(q$a, 2)
    expect_equal(e$a, 3)
  })


  it("with the right environment", {
    c <- 5
    q <- qenv() |> within(aa <- 2)
    q2 <- q |> within(1 + 1)

    expect_identical(get("c", envir = q), base::c)

    parent.env(q2@.xData) <- parent.env(.GlobalEnv)
    expect_identical(get("c", envir = q2), base::c)
  })

  it("does not get variables from global environment", {
    .GlobalEnv$a_variable <- 99
    q <- qenv() |> within({
      some_variable <- 42
    })
    expect_error(get("a_variable", envir = q), "not found")
    rm("a_variable", envir = .GlobalEnv)
  })

  it("does not get variables from current environment", {
    a_variable <- 99
    q <- qenv() |> within({
      some_variable <- 42
    })
    expect_error(get("a_variable", envir = q), "not found")
    rm("a_variable")
  })

})
