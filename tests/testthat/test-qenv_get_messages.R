test_that("get_messages accepts a qenv object and returns character", {
  q <- eval_code(qenv(), quote(message("This is a message!")))
  expect_identical(
    get_messages(q),
    paste0(
      "~~~ Messages ~~~\n\n> This is a message!\nwhen running code:\nmessage(\"This is a message!\")\n\n",
      "~~~ Trace ~~~\n\nmessage(\"This is a message!\")"
    )
  )
})

test_that("get_messages accepts a qenv.error object and returns NULL", {
  q <- eval_code(qenv(), quote(error("This is a error!")))
  expect_null(get_messages(q))
})

test_that("get_messages accepts a NULL object and returns NULL", {
  expect_null(get_messages(NULL))
})

test_that("get_messages accepts a qenv object with no message and returns NULL", {
  q <- eval_code(qenv(), quote("x <- 1"))
  expect_null(get_messages(q))
})

test_that("get_messages accepts a qenv object with 2 messages", {
  q <- qenv()
  q <- eval_code(q, quote(message("This is a message 1!")))
  q <- eval_code(q, quote(message("This is a message 2!")))
  expect_identical(
    get_messages(q),
    paste0(
      "~~~ Messages ~~~\n\n> This is a message 1!\nwhen running code:\nmessage(\"This is a message 1!\")",
      "\n\n> This is a message 2!\nwhen running code:\nmessage(\"This is a message 2!\")\n\n",
      "~~~ Trace ~~~\n\nmessage(\"This is a message 1!\")\nmessage(\"This is a message 2!\")"
    )
  )
})

test_that("get_messages accepts a qenv object with a single eval_code returning 2 messages", {
  q <- eval_code(qenv(), quote({
    message("This is a message 1!")
    message("This is a message 2!")
  }))
  expect_identical(
    get_messages(q),
    paste(
      c(
        "~~~ Messages ~~~\n",
        "> This is a message 1!",
        "when running code:",
        "message(\"This is a message 1!\")\n",
        "> This is a message 2!",
        "when running code:",
        "message(\"This is a message 2!\")\n",
        "~~~ Trace ~~~\n",
        "message(\"This is a message 1!\")",
        "message(\"This is a message 2!\")"
      ),
      collapse = "\n"
    )
  )
})

test_that("get_messages accepts a qenv object with 1 message eval_code and 1 no message eval_code", {
  q <- qenv()
  q <- eval_code(q, quote("x <- 1"))
  q <- eval_code(q, quote(message("This is a message 2!")))
  expect_identical(
    get_messages(q),
    paste0(
      "~~~ Messages ~~~\n\n> This is a message 2!\nwhen running code:\nmessage(\"This is a message 2!\")\n\n",
      "~~~ Trace ~~~\n\nx <- 1\nmessage(\"This is a message 2!\")"
    )
  )
})

test_that("get_messages formats calls with unequal numbers of messages without deparsing list elements", {
  q <- eval_code(qenv(), c(
    "f <- function() { message('m1'); message('m2') }",
    "f()",
    "message('m3')"
  ))
  expect_identical(
    get_messages(q),
    paste(
      c(
        "~~~ Messages ~~~\n",
        "> m1",
        "when running code:",
        "f()\n",
        "> m2",
        "when running code:",
        "f()\n",
        "> m3",
        "when running code:",
        "message('m3')\n",
        "~~~ Trace ~~~\n",
        "f <- function() { message('m1'); message('m2') }",
        "f()",
        "message('m3')"
      ),
      collapse = "\n"
    )
  )
})
