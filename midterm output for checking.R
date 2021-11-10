library(tidyverse)
library(kableExtra)

options(knitr.kable.NA = '')
k <- 4
N <- 13+13+14+14
df_b <- k-1
df_w <- N-k
df_tot <- N-1

mean11 <- 341.69
mean12 <- 312.92
mean13 <- 321.43
mean14 <- 302.00
grandmean <- (13*mean11 + 13*mean12 + 14*mean13 + 14*mean14)/N

ssb <- 13*(mean11-grandmean)^2 + 13*(mean12-grandmean)^2 + 14*(mean13-grandmean)^2 + 14*(mean14-grandmean)^2
ssw <- 12*(18.12)^2 + 12*(23.20)^2 + 13*(21.72)^2 + 13*(17.02)^2
msb <- ssb/df_b
msw <- ssw/df_w
Fstat <- msb/msw
p <- 1 - pf(Fstat, k-1, N-k)

q4 <- data.frame(Source = c("Between groups (Treatment)", 
                             "Within groups (Error)", 
                             "Total"),
                  df = c(k-1, N-k, N-1),
                  SS = c(ssb, ssw, ssb+ssw),
                  MS = c(msb, msw, (ssb+ssw)/(N-1)),
                  F = c(Fstat, NA, NA),
                  p = c(p, NA, NA))

kbl(q4, booktabs = T, 
    caption = "ANOVA Table Question 4") %>% 
  kable_styling(latex_options = c("hold_position"))


q5data <- data.frame(x=c(1,2,4,5,10),
                     y=c(4,3,2,1,10))

cov(q5data$x, q5data$y, use="pairwise.complete.obs")


q5lm <-lm(y~x,data=q5data)
summary(q5lm)x
mse <- mean(q5lm$residuals^2)
s_e <- summary(q5lm)$sigma
