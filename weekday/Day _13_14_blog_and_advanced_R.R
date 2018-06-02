f <- function(x) substitute(x)
g <- function(x) deparse(f(x))
g(1:10)
g(x)
g(x + y ^ 2 / z + exp(a * sin(b)))

subset(mtcars, cyl > 7)

subset2 <- function(df, condition){
  if(missing(condition)){
    cond <- seq_along(1:nrow(df))
  } else {
    cond <- eval(substitute(condition), envir = df)
  }
  df[cond,,drop = FALSE]
}
subset2(mtcars, cyl>7)

sample_df2 <- data.frame(x = 1:10)
subset2(sample_df2)

subset2(data.frame(x=c(1, 2, NA, 44)), x < 4)
subset3 <- function(x, condition) {
  r <- eval(substitute(condition), x)
  x[!is.na(r) & r, , drop=FALSE]
}
subset3(data.frame(x=c(1, 2, NA, 44)), x < 4)

setNames(as.list(seq_along(mtcars)), names(mtcars))

select <- function(df, vars) {
  vars <- substitute(vars)
  var_pos <- setNames(as.list(seq_along(df)), names(df))
  pos <- eval(vars, var_pos)
  df[, pos, drop = FALSE]
}
select(mtcars, -cyl)

eval(quote(eval(quote(eval(quote(2 + 2))))))
evalq(evalq(evalq(2 + 2)))

y <- 4
x <- 4
condition <- 4
condition_call <- 4

subset2 <- function(x, condition) {
  condition_call <- substitute(condition)
  r <- eval(condition_call, x)
  x[r, ]
}
sample_df <- data.frame(a = 1:5, b = 5:1, c = c(5, 3, 1, 4, 1))
subset2(sample_df, a == 4)
subset2(sample_df, a == y)
subset2(sample_df, a == x)
subset2(sample_df, a == condition)
subset2(sample_df, a == condition_call)

subset2 <- function(x, condition) {
  condition_call <- substitute(condition)
  r <- eval(condition_call, x, parent.frame())
  x[r, ]
}

x <- 4
subset2(sample_df, a == x)

subset2a <- function(x, condition) {
  condition_call <- substitute(condition)
  env <- list2env(x, parent = parent.frame())
  r <- eval(condition_call, env)
  x[r, ]
}

x <- 5
subset2a(sample_df, a == x)
library(plyr)
arrange(mtcars, cyl)
substitute(order(mtcars$cyl))

arrange2 <- function(x, condition){
  stopifnot(is.data.frame(x))
  cond <- substitute(order(condition))
  y <- eval(cond, x, enclos = parent.frame())
  x[y, , drop = FALSE]
}
arrange2(mtcars, cyl)

eval(substitute(list(cyl = -cyl), env = mtcars))

df <- data.frame(x = 1:5)
transform(df, x2 = x * x, x3 = x2 * x)
plyr::mutate(df, x2 = x * x, x3 = x2 * x) #this work because function have cycle for inside
plyr::mutate(df, x2 = -x)

with.default
with(mtcars, cyl * 2)     
within.data.frame

subset2 <- function(x, condition) {
  condition_call <- substitute(condition)
  r <- eval(condition_call, x, parent.frame())
  x[r, ]
}

scramble <- function(x) x[sample(nrow(x)), ]

subscramble <- function(x, condition) {
  scramble(subset2(x, condition))
}
subscramble(sample_df, a >= 4)
# Error in eval(expr, envir, enclos) : object 'a' not found
traceback()
debugonce(subset2)
subscramble(sample_df, a >= 4)
#> debugging in: subset2(x, condition)
#> debug at #1: {
#>     condition_call <- substitute(condition)
#>     r <- eval(condition_call, x, parent.frame())
#>     x[r, ]
#> }
n
#> debug at #2: condition_call <- substitute(condition)
n
#> debug at #3: r <- eval(condition_call, x, parent.frame())
r <- eval(condition_call, x, parent.frame())
#> Error in eval(expr, envir, enclos) : object 'a' not found
condition_call
#> condition
eval(condition_call, x)
#> Error in eval(expr, envir, enclos) : object 'a' not found
Q

a <- 4
subscramble(sample_df, a == 4)

subset2_q <- function(x, condition) {
  r <- eval(condition, x, parent.frame())
  x[r, ]
}
subset2 <- function(x, condition) {
  subset2_q(x, substitute(condition))
}
subscramble <- function(x, condition) {
  condition <- substitute(condition)
  scramble(subset2_q(x, condition))
}

subscramble(sample_df, a >= 3)