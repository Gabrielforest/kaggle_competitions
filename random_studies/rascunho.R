install.packages("equatiomatic")

library(equatiomatic)

equatiomatic::extract_eq(age_model)

x <- 10:1

any(x > 10)
all(x > 0)

example(seq)


function(x,c) {
  if (length(c) != 1) stop("vector c not allowed")
  return((x+c)^2)
}


z <- c(TRUE, FALSE, TRUE)
# sign dummie to the logicals:
sign(z)

g <- c( "M", "F", "F", "I", "M", "M", "F" )
# get positions of each factor:
grps <- list()
for (gen in c("M","F","I")) grps[[gen]] <- which(g == gen)
grps


# : produces integers while c() produces floating-point numbers.
1:2
c(1,2)

plot(1:10)

h <- c(0.5, 1, 0.6, 0.3)
ifelse(h > 0.5, 1, ifelse(h == 0.5, 0.5, 0))

s <- "abcdef"
strsplit(s, "ef")
substr("abcdef", 2, 4)

# creating factor for each number of Petal.width
cut(iris$Petal.Width, breaks = seq(from = 0, to = 3, by = 0.5), labels = FALSE)

# recursive functions
qs <- function(x) {
  if (length(x) <= 1) return(x)
  pivot <- x[1]
  therest <- x[-1]
  sv1 <- therest[therest < pivot]
  sv2 <- therest[therest >= pivot]
  sv1 <- qs(sv1)
  sv2 <- qs(sv2)
  return(c(sv1,pivot,sv2))
}


# + 1 until reach 15, printing how many times it was required:
p <- c(4, 2, 1)
y <- list()
soma_15 <- function(x) {if(sum(x) < 15)  y <- Recall(sum(x + 1)) else x}

soma_15(p)

# log to get numbers lower than 1:
logme <- function(x) ifelse(x > 1, Recall(log(x)), x)
logme(10)