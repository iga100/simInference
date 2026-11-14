# Run from the folder containing simInference_data_interface_fixed.R
library(simInference)
d <- data.frame(Year = rep(c("FirstYear", "Junior", "Senior", "Sophomore"), each = 20),
                Sex = rep(c("F", "M"), 40), Score = seq_len(80),
                Pass = rep(c("Yes", "No"), 40))
compare <- function(a, b) {
  set.seed(123); first <- a()
  set.seed(123); second <- b()
  stopifnot(isTRUE(all.equal(first, second)))
}
compare(function() chisq_ind(Year, Sex, data=d, reps=100, plot=FALSE),
        function() chisq_ind(d$Year, d$Sex, reps=100, plot=FALSE))
compare(function() bootstrap_one_mean_ci(Score, data=d, reps=100),
        function() bootstrap_one_mean_ci(d$Score, reps=100))
compare(function() bootstrap_two_mean_ci(Score, Sex, data=d, reps=100),
        function() bootstrap_two_mean_ci(d$Score, d$Sex, reps=100))
compare(function() bootstrap_one_prop_ci(Pass, data=d, reps=100),
        function() bootstrap_one_prop_ci(d$Pass, reps=100))
compare(function() bootstrap_two_prop_ci(Pass, Sex, data=d, reps=100),
        function() bootstrap_two_prop_ci(d$Pass, d$Sex, reps=100))
compare(function() randomization_one_mean_test(Score, mu0=40, data=d, reps=100),
        function() randomization_one_mean_test(d$Score, mu0=40, reps=100))
compare(function() randomization_one_prop_test(Pass, success="Yes", p0=.5, data=d, reps=100),
        function() randomization_one_prop_test(d$Pass, success="Yes", p0=.5, reps=100))
compare(function() randomization_two_mean_test(Score, Sex, data=d, reps=100),
        function() randomization_two_mean_test(d$Score, d$Sex, reps=100))
compare(function() randomization_two_prop_test(Pass, Sex, success="Yes", data=d, reps=100),
        function() randomization_two_prop_test(d$Pass, d$Sex, success="Yes", reps=100))
counts <- data.frame(Observed = c(38L, 13L, 39L))
compare(function() chisq_gof(Observed, data=counts, reps=100, plot=FALSE),
        function() chisq_gof(counts$Observed, reps=100, plot=FALSE))
cat("All 10 data-interface comparisons passed.\n")
