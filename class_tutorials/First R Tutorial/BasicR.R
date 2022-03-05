#Code snippets from 
#An Introduction to R
#Notes on R: A Programming Environment for Data Analysis and Graphics Version 4.0.3 (2020-10-10)

?setwd
  
#Set working directory
setwd("~/Documents/teaching/SEN1231 Mixed research methods/Demo")

#Assignment
x <- 5
5 -> x
x = 5

#Expression
x

#Vector
x <- c(10.4, 5.6, 3.1, 6.4, 21.7)
x

#Vectorization
1/x

#Artihmetic
y <- 2*x + 1

#Arithmetic function
log(x)
mean(y)

#Sequences
seq(-5, 5, by = 1) -> z
rep(x, 2)
rep(x, each=2)

#Logical comparison
temp <- x > 7.5

#Missing value
z <- c(1:3,NA);  ind <- is.na(z)

#Character vector
labs <- paste(c("X","Y"), 1:10, sep="")

?paste
help(paste)
  
#Conditional execution
a <- 4
b <- if (a < 5) 0 else a

#Vectorized if
y <- ifelse(x > 7.5, x, 0)

#Index vector
y <- x[x > 7.5]

#Subsetting
x[1:3]
x[-(1:3)]

#Factors
state <- c("tas", "sa",  "qld", "nsw", "nsw", "nt",  "wa",  "wa",
           "qld", "vic", "nsw", "vic", "qld", "qld", "sa",  "tas",
           "sa",  "nt",  "wa",  "vic", "qld", "nsw", "nsw", "wa",
           "sa",  "act", "nsw", "vic", "vic", "act")

statef <- factor(state)
statef

#Frequency tables
statefr <- table(statef)


#List: an ordered collection of objects
Lst <- list(name="Fred", wife="Mary", no.children=3, child.ages=c(4,7,9))
Lst$name
Lst$wife
Lst$child.ages[1]

#Data frames
incomes <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69, 70, 42, 56,
             61, 61, 61, 58, 51, 48, 65, 49, 49, 41, 48, 52, 46,
             59, 46, 58, 43)

incomef <- factor(cut(incomes, breaks = 35+10*(0:7)))

accountants <- data.frame(home=statef, loot=incomes, shot=incomef)

#Subsetting dataframes
accountants[accountants$home %in% c("tas"),]

