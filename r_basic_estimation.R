# This Script is a fast analysis (calling this as analysis is a crime) to understand what kind
# of values these packages give us, and how do they compute those values.
#

library(incidence)
library(earlyR)

incidences <- c()
incidences <- c(incidences, rep("2020-3-2", 2))
incidences <- c(incidences, rep("2020-3-3", 0))
incidences <- c(incidences, rep("2020-3-4", 3))
incidences <- c(incidences, rep("2020-3-5", 3))
incidences <- c(incidences, rep("2020-3-6", 5))
incidences <- c(incidences, rep("2020-3-7", 7))
incidences <- c(incidences, rep("2020-3-8", 10))
incidences <- c(incidences, rep("2020-3-9", 0))
incidences <- c(incidences, rep("2020-3-10", 11))
incidences <- c(incidences, rep("2020-3-11", 18))
incidences <- c(incidences, rep("2020-3-12", 0))
incidences <- c(incidences, rep("2020-3-13", 53))
incidences <- c(incidences, rep("2020-3-14", 57))
incidences <- c(incidences, rep("2020-3-15", 76))
incidences <- c(incidences, rep("2020-3-16", 86))
incidences <- c(incidences, rep("2020-3-17", 117))

today <- as.Date("2020-03-18")
i <- incidence(as.Date(incidences), last_date = today)

mu <- 8 # mean in days days
sigma <- 3.4 # standard deviation in days
base_R_stats <- get_R(i, si_mean = mu, si_sd = sigma)

plot(base_R_stats)

R_val <- sample_R(base_R_stats, 1000)
summary(R_val) # basic stats

hist(R_val, border = "grey", col = "navy",
     xlab = "Values of R",
     main = "Sample of likely R values")

plot(base_R_stats, "lambdas", scale = length(incidences) + 1)
abline(v = incidences, lwd = 3, col = "grey")
abline(v = today, col = "blue", lty = 2, lwd = 2)
points(incidences, seq_along(incidences), pch = 20, cex = 3)

incidence_time_series <- c(2,0,3,3,5,7,10,0,11,18,0,53,57,76,86,117)

EpiEstim::overall_infectivity(incidence_time_series)

pt_incidence_fit <- incidence::fit(i, split = NULL)
plot(pt_incidence_fit)
