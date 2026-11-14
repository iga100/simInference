# Shared validation and numerical comparison helpers.
.check_reps <- function(reps) {
  if (!is.numeric(reps) || length(reps) != 1L || is.na(reps) ||
      !is.finite(reps) || reps < 2 || reps != floor(reps) ||
      reps > .Machine$integer.max) {
    stop("reps must be an integer between 2 and .Machine$integer.max.")
  }
  as.integer(reps)
}

.check_alternative <- function(alternative) {
  if (!is.character(alternative) || length(alternative) != 1L ||
      is.na(alternative) || !alternative %in% c("less", "greater", "two.sided")) {
    stop("alternative must be 'less', 'greater', or 'two.sided'.")
  }
  alternative
}

.check_scalar_label <- function(value, name) {
  if (length(value) != 1L || is.na(value) || !is.atomic(value)) {
    stop(name, " must be a single non-missing value.")
  }
  as.character(value)
}

# Floating-point tolerance is scaled to the magnitude of the compared values.
.is_extreme <- function(simulated, observed, null_value, alternative) {
  tolerance <- 64 * .Machine$double.eps *
    pmax(1, abs(simulated), abs(observed), abs(null_value))
  switch(alternative,
         less = simulated <= observed + tolerance,
         greater = simulated >= observed - tolerance,
         two.sided = abs(simulated - null_value) >=
           abs(observed - null_value) - tolerance)
}

# Plot a histogram whose bin boundaries include the test cutoffs.
# Boundary observations are assigned to the extreme-side bin, so
# red bars never extend into the non-extreme region.
.plot_randomization_hist <- function(simulated, extreme, null_value,
                                     observed, alternative, xlab,
                                     breaks = 30) {
  base_hist <- hist(simulated, breaks = breaks, plot = FALSE)
  distance <- abs(observed - null_value)
  cutoffs <- switch(alternative,
                    less = observed,
                    greater = observed,
                    two.sided = c(null_value - distance,
                                  null_value + distance))
  # Ensure every cutoff inside the data range is a bin boundary.
  inside <- cutoffs[cutoffs > min(base_hist$breaks) &
                      cutoffs < max(base_hist$breaks)]
  edges <- sort(unique(c(base_hist$breaks, inside)))
  n_bins <- length(edges) - 1L

  # Left-closed bins by default. Move exact left-cutoff observations
  # into the bin on the left for a two-sided or left-sided test.
  bin <- findInterval(simulated, edges, rightmost.closed = TRUE)
  left_cutoff <- if (alternative == "two.sided") min(cutoffs) else observed
  if (alternative != "greater") {
    on_left <- extreme & simulated == left_cutoff &
      left_cutoff %in% edges
    bin[on_left] <- pmax(1L, bin[on_left] - 1L)
  }
  counts <- tabulate(bin, nbins = n_bins)
  red_counts <- tabulate(bin[extreme], nbins = n_bins)

  # Use equal-width count bars with their true widths; the underlying
  # histogram object retains the correct counts and bin boundaries.
  ymax <- max(1, counts) * 1.12
  plot(range(edges), c(0, ymax), type = "n", xlab = xlab,
       ylab = "Number of simulations",
       bty = "l",
       main = paste0(switch(alternative,
                            less = "Left-Sided",
                            greater = "Right-Sided",
                            two.sided = "Two-Sided"),
                     " Randomization Test"))
  for (i in seq_len(n_bins)) {
    if (counts[i] > 0) {
      rect(edges[i], 0, edges[i + 1L], counts[i],
           col = "lightgray", border = "white")
    }
    if (red_counts[i] > 0) {
      # All bins lie wholly on one side of the cutoff(s).
      rect(edges[i], 0, edges[i + 1L], red_counts[i],
           col = "tomato", border = "white")
    }
  }
  abline(v = null_value, col = "black", lwd = 1.5, lty = 2)
  if (alternative == "two.sided") {
    abline(v = cutoffs, col = "blue", lwd = 1.2)
  } else {
    abline(v = observed, col = "blue", lwd = 1.2)
  }
  legend("topright", c("Extreme outcomes", "Non-extreme outcomes",
                       "Cutoff(s)", "Null value"),
         fill = c("tomato", "lightgray", NA, NA),
         border = c("white", "white", NA, NA),
         lty = c(NA, NA, 1, 2),
         col = c(NA, NA, "blue", "black"), bty = "n", cex = 0.8)
  invisible(list(breaks = edges, counts = counts,
                 extreme_counts = red_counts))
}

# Optional bootstrap confidence intervals are calculated from
# the observed data, independently of the null simulation used for p-values.
.check_ci_options <- function(ci, conf.level, ci.method) {
  if (!is.logical(ci) || length(ci) != 1L || is.na(ci))
    stop("ci must be TRUE or FALSE.")
  if (!is.numeric(conf.level) || length(conf.level) != 1L ||
      !is.finite(conf.level) || conf.level <= 0 || conf.level >= 1)
    stop("conf.level must be a number strictly between 0 and 1.")
  if (!is.character(ci.method) || length(ci.method) < 1L ||
      anyNA(ci.method) || any(!ci.method %in% c("percentile", "basic", "normal")))
    stop("ci.method must contain one or more of: percentile, basic, normal.")
  invisible(NULL)
}

.bootstrap_interval <- function(boot_statistics, conf.level, observed,
                                method = c("percentile", "basic", "normal")) {
  methods <- unique(method)
  alpha <- 1 - conf.level
  q <- as.numeric(stats::quantile(boot_statistics,
       probs = c(alpha / 2, 1 - alpha / 2), names = FALSE))
  se <- stats::sd(boot_statistics)
  intervals <- list(
    percentile = q,
    basic = 2 * observed - rev(q),
    normal = observed + c(-1, 1) * stats::qnorm(1 - alpha / 2) * se
  )
  out <- do.call(rbind, lapply(methods, function(m) {
    data.frame(Method = m, Lower = unname(intervals[[m]][1]),
               Upper = unname(intervals[[m]][2]))
  }))
  rownames(out) <- NULL
  out
}

.print_ci <- function(ci_result, conf.level, description, method) {
  cat(sprintf("\n%.1f%% bootstrap confidence intervals for %s:\n",
              100 * conf.level, description))
  print(ci_result, row.names = FALSE)
}

# Simulation-Based Hypothesis Tests for MA 205
# Load with: source("randomization_tests_all_CI_methods.R")
# For reproducible simulations, call set.seed(123) before a test.


# One-sample proportion randomization test

randomization_one_prop_test <- function(
    x,
    success = 1,
    p0,
    alternative = "two.sided",
    reps = 10000,
    ci = FALSE,
    conf.level = 0.95,
    ci.method = c("percentile", "basic", "normal")
) {
  
  alternative <- .check_alternative(alternative)
  reps <- .check_reps(reps)
  .check_ci_options(ci, conf.level, ci.method)
  success <- .check_scalar_label(success, "success")

  # ============================================================
  # Remove missing values
  # ============================================================
  
  if (!is.atomic(x) || is.list(x) || is.complex(x)) {
    stop("x must be an atomic categorical or numeric vector.")
  }
  x <- x[!is.na(x)]
  
  
  # ============================================================
  # Check inputs
  # ============================================================
  
  if (length(x) < 2) {
    stop(
      "x must contain at least two non-missing observations."
    )
  }
  
  if (length(success) != 1) {
    stop(
      "Specify exactly one value as the success category."
    )
  }
  
  if (!is.numeric(p0) ||
      length(p0) != 1 ||
      !is.finite(p0) ||
      p0 <= 0 ||
      p0 >= 1) {
    
    stop(
      "p0 must be one number strictly between 0 and 1."
    )
  }
  
  if (!alternative %in%
      c("less", "greater", "two.sided")) {
    
    stop(
      "alternative must be 'less', 'greater', or 'two.sided'."
    )
  }
  
  if (!is.numeric(reps) ||
      length(reps) != 1 ||
      !is.finite(reps) ||
      reps < 2 ||
       reps != floor(reps)) {
    
    stop(
      "reps must be an integer greater than 1."
    )
  }
  
  reps <- as.integer(reps)
  
  
  # ============================================================
  # Convert response to 0 and 1
  # ============================================================
  
  # The value specified in "success" is coded as 1.
  # All other values are coded as 0.
  
  x_binary <- as.integer(
    as.character(x) ==
      as.character(success)
  )
  
  
  # ============================================================
  # Require an actual binary variable (one category is also possible
  # when the observed sample contains all successes or all failures).
  if (any(!is.finite(match(as.character(x), unique(as.character(x)))))) {
    stop("Invalid response values.")
  }
  if (length(unique(as.character(x))) > 2L) {
    stop("x must contain at most two observed categories.")
  }

  # Observed sample statistics
  # ============================================================
  
  n <- length(x_binary)
  
  successes <- sum(x_binary)
  
  failures <- n - successes
  
  p_hat <- mean(x_binary)
  
  
  # ============================================================
  # Generate randomization distribution under H0
  # ============================================================
  
  # Under H0:
  #
  # p = p0
  #
  # Generate n Bernoulli outcomes with probability p0
  # and calculate the sample proportion.
  
  p_random <- replicate(
    reps,
    {
      
      random_sample <- rbinom(
        n = n,
        size = 1,
        prob = p0
      )
      
      mean(random_sample)
    }
  )
  
  
  # ============================================================
  # Randomization p-value
  # ============================================================
  
  if (alternative == "less") {
    
    # ----------------------------------------------------------
    # H0: p = p0
    # Ha: p < p0
    #
    # Left tail:
    # randomized proportions <= observed p-hat
    # ----------------------------------------------------------
    
    cutoff <- p_hat
    
    extreme_count <- sum(
      p_random <= cutoff
    )
    
    p_value <- (
      extreme_count + 1
    ) / (
      reps + 1
    )
    
    
  } else if (alternative == "greater") {
    
    # ----------------------------------------------------------
    # H0: p = p0
    # Ha: p > p0
    #
    # Right tail:
    # randomized proportions >= observed p-hat
    # ----------------------------------------------------------
    
    cutoff <- p_hat
    
    extreme_count <- sum(
      p_random >= cutoff
    )
    
    p_value <- (
      extreme_count + 1
    ) / (
      reps + 1
    )
    
    
  } else {
    
    # ----------------------------------------------------------
    # H0: p = p0
    # Ha: p != p0
    #
    # Both tails equally far from p0 as observed p-hat.
    # ----------------------------------------------------------
    
    distance <- abs(
      p_hat - p0
    )
    
    lower_cutoff <- p0 - distance
    
    upper_cutoff <- p0 + distance
    
    extreme_count <- sum(
      p_random <= lower_cutoff |
        p_random >= upper_cutoff
    )
    
    p_value <- (
      extreme_count + 1
    ) / (
      reps + 1
    )
  }
  
  
  # ============================================================
  # Use identical comparisons for p-values and histogram coloring.
  extreme <- .is_extreme(p_random, p_hat, p0, alternative)
  extreme_count <- sum(extreme)
  p_value <- (extreme_count + 1) / (reps + 1)

  # Display results
  # ============================================================
  
  cat("\n")
  cat("Randomization Test for One Proportion\n")
  cat("========================================\n")
  
  cat(
    "Sample size:",
    n,
    "\n"
  )
  
  cat(
    "Success category:",
    success,
    "\n"
  )
  
  cat(
    "Number of successes:",
    successes,
    "\n"
  )
  
  cat(
    "Number of failures:",
    failures,
    "\n"
  )
  
  cat(
    "Observed sample proportion:",
    round(
      p_hat,
      4
    ),
    "\n"
  )
  
  cat(
    "Hypothesized proportion:",
    p0,
    "\n"
  )
  
  
  # ============================================================
  # Display hypotheses
  # ============================================================
  
  if (alternative == "less") {
    
    cat(
      "H0: p =",
      p0,
      "\n"
    )
    
    cat(
      "Ha: p <",
      p0,
      "\n"
    )
    
    
  } else if (alternative == "greater") {
    
    cat(
      "H0: p =",
      p0,
      "\n"
    )
    
    cat(
      "Ha: p >",
      p0,
      "\n"
    )
    
    
  } else {
    
    cat(
      "H0: p =",
      p0,
      "\n"
    )
    
    cat(
      "Ha: p !=",
      p0,
      "\n"
    )
  }
  
  
  cat(
    "Number of randomizations:",
    reps,
    "\n"
  )
  
  cat(
    "Randomization p-value:",
    round(
      p_value,
      4
    ),
    "\n"
  )
  
  
  # ============================================================
  # Optional confidence interval: bootstrap the observed data, not H0.
  # This does not alter the null simulation or its p-value.
  # ============================================================
  ci_result <- NULL
  ci_bootstrap <- NULL
  if (ci) {
  ci_bootstrap <- stats::rbinom(reps, size = n, prob = p_hat) / n
    ci_result <- .bootstrap_interval(ci_bootstrap, conf.level, p_hat, ci.method)
    .print_ci(ci_result, conf.level, "proportion", ci.method)
  }

  # ============================================================
  # Create histogram information
  # ============================================================
  
  # Flag individual simulated outcomes, not histogram midpoints.
  .plot_randomization_hist(p_random, extreme, p0, p_hat,
                           alternative, "Randomized Sample Proportions", breaks = 30)


  # ============================================================
  # Return results
  # ============================================================
  
  invisible(
    list(
      
      # Sample information
      n = n,
      
      success = success,
      
      successes = successes,
      
      failures = failures,
      
      # Observed proportion
      p_hat = p_hat,
      
      # Null proportion
      p0 = p0,
      
      # Alternative
      alternative = alternative,
      
      # P-value
      p_value = p_value,
      ci = ci_result,
      conf.level = if (ci) conf.level else NULL,
      ci.method = if (ci) ci.method else NULL,
      ci_bootstrap = ci_bootstrap,
      extreme_count = extreme_count,
      reps = reps,
      
      # Randomization distribution
      p_random = p_random,
      
      randomization_distribution = p_random
      
    )
  )
}



# One-sample mean: null-centered bootstrap test


randomization_one_mean_test <- function(
    x,
    mu0,
    alternative = "two.sided",
    reps = 10000,
    ci = FALSE,
    conf.level = 0.95,
    ci.method = c("percentile", "basic", "normal")
) {
  
  alternative <- .check_alternative(alternative)
  reps <- .check_reps(reps)
  .check_ci_options(ci, conf.level, ci.method)

  # ============================================================
  # Remove missing values
  # ============================================================
  
  x <- na.omit(x)
  
  
  # ============================================================
  # Check inputs
  # ============================================================
  
  if (!is.numeric(x)) {
    stop("x must be a numeric variable.")
  }
  
  if (length(x) < 2) {
    stop(
      "x must contain at least two non-missing observations."
    )
  }
  
  if (!is.numeric(mu0) ||
      length(mu0) != 1 ||
      !is.finite(mu0)) {
    
    stop("mu0 must be one finite numeric value.")
  }
  
  if (!alternative %in%
      c("less", "greater", "two.sided")) {
    
    stop(
      "alternative must be 'less', 'greater', or 'two.sided'."
    )
  }
  
  if (!is.numeric(reps) ||
      length(reps) != 1 ||
      !is.finite(reps) ||
      reps < 2 ||
       reps != floor(reps)) {
    
    stop("reps must be an integer greater than 1.")
  }
  
  reps <- as.integer(reps)
  
  
  # ============================================================
  if (any(!is.finite(x))) {
    stop("x must contain only finite numeric values after removing missing values.")
  }
  if (sd(x) == 0) {
    warning("The observations have zero sample variance; the test is degenerate.")
  }

  # Observed sample statistics
  # ============================================================
  
  n <- length(x)
  
  xbar <- mean(x)
  
  
  # ============================================================
  # Shift the sample to satisfy H0
  # ============================================================
  
  # Shift every observation by the same amount so that
  # the shifted sample has mean exactly equal to mu0.
  
  x_null <- x - xbar + mu0
  
  
  # ============================================================
  # Generate the randomization distribution
  # ============================================================
  
  # Following the StatKey-style approach:
  #
  # 1. Start with the shifted sample under H0.
  # 2. Resample n observations WITH replacement.
  # 3. Calculate the sample mean.
  # 4. Repeat many times.
  
  mean_random <- replicate(
    reps,
    {
      
      x_random <- sample(
        x_null,
        size = n,
        replace = TRUE
      )
      
      mean(x_random)
    }
  )
  
  
  # ============================================================
  # Randomization p-value
  # ============================================================
  
  # ============================================================
  # Randomization p-value
  # ============================================================
  
  extreme <- .is_extreme(mean_random, xbar, mu0, alternative)
  
  extreme_count <- sum(extreme)
  
  p_value <- (extreme_count + 1) / (reps + 1)
  
  
  # ============================================================
  # Use identical comparisons for p-values and histogram coloring.
  extreme <- .is_extreme(mean_random, xbar, mu0, alternative)
  extreme_count <- sum(extreme)
  p_value <- (extreme_count + 1) / (reps + 1)

  # Display results
  # ============================================================
  
  cat("\n")
  cat("Randomization Test for One Mean\n")
  cat("========================================\n")
  
  cat(
    "Sample size:",
    n,
    "\n"
  )
  
  cat(
    "Observed sample mean:",
    round(xbar, 4),
    "\n"
  )
  
  cat(
    "Hypothesized mean:",
    mu0,
    "\n"
  )
  
  
  # Display hypotheses
  
  if (alternative == "less") {
    
    cat(
      "H0: mu =",
      mu0,
      "\n"
    )
    
    cat(
      "Ha: mu <",
      mu0,
      "\n"
    )
    
  } else if (alternative == "greater") {
    
    cat(
      "H0: mu =",
      mu0,
      "\n"
    )
    
    cat(
      "Ha: mu >",
      mu0,
      "\n"
    )
    
  } else {
    
    cat(
      "H0: mu =",
      mu0,
      "\n"
    )
    
    cat(
      "Ha: mu !=",
      mu0,
      "\n"
    )
  }
  
  
  cat(
    "Number of randomizations:",
    reps,
    "\n"
  )
  
  cat(
    "Randomization p-value:",
    round(p_value, 4),
    "\n"
  )
  
  
  # ============================================================
  # Optional confidence interval: bootstrap the observed data, not H0.
  # This does not alter the null simulation or its p-value.
  # ============================================================
  ci_result <- NULL
  ci_bootstrap <- NULL
  if (ci) {
  ci_bootstrap <- replicate(reps, mean(sample(x, size = n, replace = TRUE)))
    ci_result <- .bootstrap_interval(ci_bootstrap, conf.level, xbar, ci.method)
    .print_ci(ci_result, conf.level, "mean", ci.method)
  }

  # ============================================================
  # Create histogram information
  # ============================================================
  
  # Flag individual simulated outcomes, not histogram midpoints.
  .plot_randomization_hist(mean_random, extreme, mu0, xbar,
                           alternative, "Randomized Sample Means", breaks = 40)


  # ============================================================
  # Return results
  # ============================================================
  
  invisible(
    list(
      
      # Sample information
      n = n,
      
      sample_mean = xbar,
      
      # Null value
      mu0 = mu0,
      
      # Alternative
      alternative = alternative,
      
      # P-value
      p_value = p_value,
      ci = ci_result,
      conf.level = if (ci) conf.level else NULL,
      ci.method = if (ci) ci.method else NULL,
      ci_bootstrap = ci_bootstrap,
      extreme_count = extreme_count,
      reps = reps,
      
      # Data shifted under H0
      x_null = x_null,
      
      # Randomization distribution
      mean_random = mean_random,
      
      randomization_distribution = mean_random
      
    )
  )
}




# Two-sample proportion permutation test

# ============================================================
# Randomization test for two population proportions
# ============================================================

randomization_two_prop_test <- function(
    x,
    group,
    success = 1,
    group1 = NULL,
    group2 = NULL,
    alternative = "two.sided",
    reps = 10000,
    ci = FALSE,
    conf.level = 0.95,
    ci.method = c("percentile", "basic", "normal")
) {
  
  alternative <- .check_alternative(alternative)
  reps <- .check_reps(reps)
  .check_ci_options(ci, conf.level, ci.method)
  success <- .check_scalar_label(success, "success")
  if (!is.null(group1)) group1 <- .check_scalar_label(group1, "group1")
  if (!is.null(group2)) group2 <- .check_scalar_label(group2, "group2")

  # ============================================================
  # Check inputs
  # ============================================================
  
  if (missing(x)) {
    stop("You must provide the response variable using x.")
  }
  
  if (missing(group)) {
    stop("You must provide the grouping variable using group.")
  }
  
  if (length(x) != length(group)) {
    stop("x and group must have the same length.")
  }
  
  if (length(success) != 1) {
    stop("Specify exactly one success category.")
  }
  
  if (!alternative %in% c(
    "less",
    "greater",
    "two.sided"
  )) {
    
    stop(
      "alternative must be 'less', 'greater', or 'two.sided'."
    )
  }
  
  if (
    !is.numeric(reps) ||
    length(reps) != 1 ||
    !is.finite(reps) ||
    reps < 2 ||
    reps != floor(reps)
  ) {
    
    stop(
      "reps must be an integer greater than 1."
    )
  }
  
  reps <- as.integer(reps)
  
  
  # ============================================================
  # Remove missing values
  # ============================================================
  
  complete_cases <- complete.cases(
    x,
    group
  )
  
  x <- x[complete_cases]
  group <- group[complete_cases]
  
  if (length(x) == 0) {
    stop("No complete observations remain after removing missing values.")
  }
  
  
  # ============================================================
  # Check response variable
  # ============================================================
  
  response_values <- unique(
    as.character(x)
  )
  
  if (length(response_values) != 2) {
    
    stop(
      paste0(
        "The response variable must contain exactly two categories. ",
        "The available values are: ",
        paste(
          response_values,
          collapse = ", "
        )
      )
    )
  }
  
  if (!as.character(success) %in% response_values) {
    
    stop(
      paste0(
        "The success category '",
        success,
        "' was not found. Available values are: ",
        paste(
          response_values,
          collapse = ", "
        )
      )
    )
  }
  
  
  # ============================================================
  # Convert response to 0/1
  # ============================================================
  
  # Success category becomes 1
  # Other category becomes 0
  
  response <- as.integer(
    as.character(x) == as.character(success)
  )
  
  
  # ============================================================
  # Check grouping variable
  # ============================================================
  
  group <- droplevels(
    factor(group)
  )
  
  available_groups <- levels(group)
  
  if (length(available_groups) != 2) {
    
    stop(
      paste0(
        "The grouping variable must contain exactly two groups. ",
        "The available groups are: ",
        paste(
          available_groups,
          collapse = ", "
        )
      )
    )
  }
  
  
  # ============================================================
  # Determine group order
  # ============================================================
  
  if (
    is.null(group1) &&
    is.null(group2)
  ) {
    
    group1 <- available_groups[1]
    group2 <- available_groups[2]
    
  } else if (
    is.null(group1) ||
    is.null(group2)
  ) {
    
    stop(
      "Specify both group1 and group2, or leave both as NULL."
    )
    
  } else {
    
    group1 <- as.character(group1)
    group2 <- as.character(group2)
    
    if (group1 == group2) {
      stop("group1 and group2 must be different.")
    }
    
    if (!group1 %in% available_groups) {
      
      stop(
        paste0(
          "The group '",
          group1,
          "' was not found."
        )
      )
    }
    
    if (!group2 %in% available_groups) {
      
      stop(
        paste0(
          "The group '",
          group2,
          "' was not found."
        )
      )
    }
  }
  
  
  # Reorder the grouping variable so the statistic is:
  #
  # p-hat group1 - p-hat group2
  
  group <- factor(
    group,
    levels = c(
      group1,
      group2
    )
  )
  
  
  # ============================================================
  # Separate the two groups
  # ============================================================
  
  response1 <- response[
    group == group1
  ]
  
  response2 <- response[
    group == group2
  ]
  
  n1 <- length(response1)
  n2 <- length(response2)
  
  if (n1 < 2 || n2 < 2) {
    
    stop(
      "Each group must contain at least two observations."
    )
  }
  
  
  # ============================================================
  # Observed sample statistics
  # ============================================================
  
  successes1 <- sum(response1)
  successes2 <- sum(response2)
  
  failures1 <- n1 - successes1
  failures2 <- n2 - successes2
  
  p_hat1 <- mean(response1)
  p_hat2 <- mean(response2)
  
  observed_difference <- p_hat1 - p_hat2
  
  
  # ============================================================
  # Pool observed outcomes
  # ============================================================
  
  # Under H0:
  #
  # p1 = p2
  #
  # The observed successes and failures are randomly
  # reallocated between the two groups.
  
  pooled_response <- c(
    response1,
    response2
  )
  
  total_n <- n1 + n2
  
  
  # ============================================================
  # Generate randomization distribution
  # ============================================================
  
  difference_random <- replicate(
    reps,
    {
      
      shuffled_response <- sample(
        pooled_response,
        size = total_n,
        replace = FALSE
      )
      
      random_group1 <- shuffled_response[
        seq_len(n1)
      ]
      
      random_group2 <- shuffled_response[
        (n1 + 1):total_n
      ]
      
      mean(random_group1) -
        mean(random_group2)
    }
  )
  
  
  # ============================================================
  # Calculate randomization p-value
  # ============================================================
  
  if (alternative == "less") {
    
    # H0: p1 - p2 = 0
    # Ha: p1 - p2 < 0
    
    lower_cutoff <- observed_difference
    upper_cutoff <- NA_real_
    
    extreme_count <- sum(
      difference_random <= observed_difference
    )
    
  } else if (alternative == "greater") {
    
    # H0: p1 - p2 = 0
    # Ha: p1 - p2 > 0
    
    lower_cutoff <- NA_real_
    upper_cutoff <- observed_difference
    
    extreme_count <- sum(
      difference_random >= observed_difference
    )
    
  } else {
    
    # H0: p1 - p2 = 0
    # Ha: p1 - p2 != 0
    
    distance <- abs(
      observed_difference
    )
    
    lower_cutoff <- -distance
    upper_cutoff <- distance
    
    extreme_count <- sum(
      difference_random <= lower_cutoff |
        difference_random >= upper_cutoff
    )
  }
  
  p_value <- (
    extreme_count + 1
  ) / (
    reps + 1
  )
  
  
  # ============================================================
  # Use identical comparisons for p-values and histogram coloring.
  extreme <- .is_extreme(difference_random, observed_difference, 0, alternative)
  extreme_count <- sum(extreme)
  p_value <- (extreme_count + 1) / (reps + 1)

  # Display results
  # ============================================================
  
  cat("\n")
  cat("Randomization Test for Two Proportions\n")
  cat("============================================\n")
  
  cat(
    "Success category:",
    as.character(success),
    "\n"
  )
  
  
  # ------------------------------------------------------------
  # Group 1
  # ------------------------------------------------------------
  
  cat(
    "\nGroup 1:",
    group1,
    "\n"
  )
  
  cat(
    "Sample size:",
    n1,
    "\n"
  )
  
  cat(
    "Number of successes:",
    successes1,
    "\n"
  )
  
  cat(
    "Number of failures:",
    failures1,
    "\n"
  )
  
  cat(
    "Sample proportion:",
    round(
      p_hat1,
      4
    ),
    "\n"
  )
  
  
  # ------------------------------------------------------------
  # Group 2
  # ------------------------------------------------------------
  
  cat(
    "\nGroup 2:",
    group2,
    "\n"
  )
  
  cat(
    "Sample size:",
    n2,
    "\n"
  )
  
  cat(
    "Number of successes:",
    successes2,
    "\n"
  )
  
  cat(
    "Number of failures:",
    failures2,
    "\n"
  )
  
  cat(
    "Sample proportion:",
    round(
      p_hat2,
      4
    ),
    "\n"
  )
  
  
  # ------------------------------------------------------------
  # Observed difference
  # ------------------------------------------------------------
  
  cat(
    "\nObserved difference (",
    group1,
    " - ",
    group2,
    "): ",
    round(
      observed_difference,
      4
    ),
    "\n",
    sep = ""
  )
  
  
  # ============================================================
  # Display hypotheses
  # ============================================================
  
  cat("\nHypotheses\n")
  cat("--------------------------------------------\n")
  
  cat(
    "H0: p_",
    group1,
    " - p_",
    group2,
    " = 0\n",
    sep = ""
  )
  
  if (alternative == "less") {
    
    cat(
      "Ha: p_",
      group1,
      " - p_",
      group2,
      " < 0\n",
      sep = ""
    )
    
  } else if (alternative == "greater") {
    
    cat(
      "Ha: p_",
      group1,
      " - p_",
      group2,
      " > 0\n",
      sep = ""
    )
    
  } else {
    
    cat(
      "Ha: p_",
      group1,
      " - p_",
      group2,
      " != 0\n",
      sep = ""
    )
  }
  
  cat(
    "\nNumber of randomizations:",
    reps,
    "\n"
  )
  
  cat(
    "Randomization p-value:",
    round(
      p_value,
      4
    ),
    "\n"
  )
  
  
  # ============================================================
  # Optional confidence interval: bootstrap the observed data, not H0.
  # This does not alter the null simulation or its p-value.
  # ============================================================
  ci_result <- NULL
  ci_bootstrap <- NULL
  if (ci) {
  ci_bootstrap <- stats::rbinom(reps, n1, p_hat1) / n1 -
    stats::rbinom(reps, n2, p_hat2) / n2
    ci_result <- .bootstrap_interval(ci_bootstrap, conf.level, observed_difference, ci.method)
    .print_ci(ci_result, conf.level, "difference in proportions (group1 - group2)", ci.method)
  }

  # ============================================================
  # Create histogram information
  # ============================================================
  
  # Flag individual simulated outcomes, not histogram midpoints.
  .plot_randomization_hist(difference_random, extreme, 0, observed_difference,
                           alternative, "Randomized Difference in Proportions", breaks = 30)


  # ============================================================
  # Return results
  # ============================================================
  
  invisible(
    list(
      
      success = success,
      
      group1 = group1,
      group2 = group2,
      
      n1 = n1,
      successes1 = successes1,
      failures1 = failures1,
      p_hat1 = p_hat1,
      
      n2 = n2,
      successes2 = successes2,
      failures2 = failures2,
      p_hat2 = p_hat2,
      
      observed_difference = observed_difference,
      difference = observed_difference,
      
      alternative = alternative,
      
      lower_cutoff = lower_cutoff,
      upper_cutoff = upper_cutoff,
      
      extreme_count = extreme_count,
      reps = reps,
      p_value = p_value,
      ci = ci_result,
      conf.level = if (ci) conf.level else NULL,
      ci.method = if (ci) ci.method else NULL,
      ci_bootstrap = ci_bootstrap,
      
      difference_random = difference_random,
      randomization_distribution = difference_random
    )
  )
}

# ============================================================
# Randomization test for two population means
# ============================================================

randomization_two_mean_test <- function(
    x,
    group,
    group1 = NULL,
    group2 = NULL,
    alternative = "two.sided",
    reps = 10000,
    ci = FALSE,
    conf.level = 0.95,
    ci.method = c("percentile", "basic", "normal")
) {
  
  alternative <- .check_alternative(alternative)
  reps <- .check_reps(reps)
  .check_ci_options(ci, conf.level, ci.method)
  if (!is.null(group1)) group1 <- .check_scalar_label(group1, "group1")
  if (!is.null(group2)) group2 <- .check_scalar_label(group2, "group2")

  # ============================================================
  # Check inputs
  # ============================================================
  
  if (missing(x)) {
    stop("You must provide the numeric response variable using x.")
  }
  
  if (missing(group)) {
    stop("You must provide the grouping variable using group.")
  }
  
  if (!is.numeric(x)) {
    stop("x must be a numeric variable.")
  }
  
  if (length(x) != length(group)) {
    stop("x and group must have the same length.")
  }
  
  if (!alternative %in% c(
    "less",
    "greater",
    "two.sided"
  )) {
    
    stop(
      "alternative must be 'less', 'greater', or 'two.sided'."
    )
  }
  
  if (
    !is.numeric(reps) ||
    length(reps) != 1 ||
    !is.finite(reps) ||
    reps < 2 ||
    reps != floor(reps)
  ) {
    
    stop("reps must be an integer greater than 1.")
  }
  
  reps <- as.integer(reps)
  
  
  # ============================================================
  # Remove missing values
  # ============================================================
  
  complete_cases <- complete.cases(
    x,
    group
  )
  
  x <- x[complete_cases]
  group <- group[complete_cases]
  
  if (length(x) == 0) {
    
    stop(
      "No complete observations remain after removing missing values."
    )
  }
  
  
  # ============================================================
  if (any(!is.finite(x))) {
    stop("x must contain only finite numeric values after removing missing values.")
  }
  if (sd(x) == 0) {
    warning("The observations have zero sample variance; the test is degenerate.")
  }

  # Check grouping variable
  # ============================================================
  
  group <- droplevels(
    factor(group)
  )
  
  available_groups <- levels(group)
  
  if (length(available_groups) != 2) {
    
    stop(
      paste0(
        "The grouping variable must contain exactly two groups. ",
        "The available groups are: ",
        paste(
          available_groups,
          collapse = ", "
        )
      )
    )
  }
  
  
  # ============================================================
  # Determine group order
  # ============================================================
  
  if (
    is.null(group1) &&
    is.null(group2)
  ) {
    
    group1 <- available_groups[1]
    group2 <- available_groups[2]
    
  } else if (
    is.null(group1) ||
    is.null(group2)
  ) {
    
    stop(
      "Specify both group1 and group2, or leave both as NULL."
    )
    
  } else {
    
    group1 <- as.character(group1)
    group2 <- as.character(group2)
    
    if (group1 == group2) {
      stop("group1 and group2 must be different.")
    }
    
    if (!group1 %in% available_groups) {
      
      stop(
        paste0(
          "The group '",
          group1,
          "' was not found. Available groups are: ",
          paste(
            available_groups,
            collapse = ", "
          )
        )
      )
    }
    
    if (!group2 %in% available_groups) {
      
      stop(
        paste0(
          "The group '",
          group2,
          "' was not found. Available groups are: ",
          paste(
            available_groups,
            collapse = ", "
          )
        )
      )
    }
  }
  
  
  # Reorder the grouping variable so that the statistic is:
  #
  # mean of group1 - mean of group2
  
  group <- factor(
    group,
    levels = c(
      group1,
      group2
    )
  )
  
  
  # ============================================================
  # Separate the two groups
  # ============================================================
  
  x1 <- x[
    group == group1
  ]
  
  x2 <- x[
    group == group2
  ]
  
  n1 <- length(x1)
  n2 <- length(x2)
  
  if (n1 < 2 || n2 < 2) {
    
    stop(
      "Each group must contain at least two observations."
    )
  }
  
  
  # ============================================================
  # Observed sample statistics
  # ============================================================
  
  mean1 <- mean(x1)
  mean2 <- mean(x2)
  
  sd1 <- sd(x1)
  sd2 <- sd(x2)
  
  observed_difference <- mean1 - mean2
  
  
  # ============================================================
  # Pool all observations
  # ============================================================
  
  # Under H0:
  #
  # mu1 - mu2 = 0
  #
  # The group labels are exchangeable. Therefore, the observed
  # values can be randomly reallocated between the two groups.
  
  pooled_data <- c(
    x1,
    x2
  )
  
  total_n <- n1 + n2
  
  
  # ============================================================
  # Generate randomization distribution
  # ============================================================
  
  # Randomly reallocate the observations between the groups
  # WITHOUT replacement while preserving n1 and n2.
  
  difference_random <- replicate(
    reps,
    {
      
      shuffled_data <- sample(
        pooled_data,
        size = total_n,
        replace = FALSE
      )
      
      random_group1 <- shuffled_data[
        seq_len(n1)
      ]
      
      random_group2 <- shuffled_data[
        (n1 + 1):total_n
      ]
      
      mean(random_group1) -
        mean(random_group2)
    }
  )
  
  
  # ============================================================
  # Calculate randomization p-value
  # ============================================================
  
  if (alternative == "less") {
    
    # H0: mu1 - mu2 = 0
    # Ha: mu1 - mu2 < 0
    
    lower_cutoff <- observed_difference
    upper_cutoff <- NA_real_
    
    extreme_count <- sum(
      difference_random <= observed_difference
    )
    
  } else if (alternative == "greater") {
    
    # H0: mu1 - mu2 = 0
    # Ha: mu1 - mu2 > 0
    
    lower_cutoff <- NA_real_
    upper_cutoff <- observed_difference
    
    extreme_count <- sum(
      difference_random >= observed_difference
    )
    
  } else {
    
    # H0: mu1 - mu2 = 0
    # Ha: mu1 - mu2 != 0
    
    distance <- abs(
      observed_difference
    )
    
    lower_cutoff <- -distance
    upper_cutoff <- distance
    
    extreme_count <- sum(
      difference_random <= lower_cutoff |
        difference_random >= upper_cutoff
    )
  }
  
  # Add one to the numerator and denominator to avoid
  # obtaining a p-value of exactly zero.
  
  p_value <- (
    extreme_count + 1
  ) / (
    reps + 1
  )
  
  
  # ============================================================
  # Use identical comparisons for p-values and histogram coloring.
  extreme <- .is_extreme(difference_random, observed_difference, 0, alternative)
  extreme_count <- sum(extreme)
  p_value <- (extreme_count + 1) / (reps + 1)

  # Display results
  # ============================================================
  
  cat("\n")
  cat("Randomization Test for Two Means\n")
  cat("============================================\n")
  
  
  # ------------------------------------------------------------
  # Group 1
  # ------------------------------------------------------------
  
  cat(
    "\nGroup 1:",
    group1,
    "\n"
  )
  
  cat(
    "Sample size:",
    n1,
    "\n"
  )
  
  cat(
    "Sample mean:",
    round(
      mean1,
      4
    ),
    "\n"
  )
  
  cat(
    "Sample standard deviation:",
    round(
      sd1,
      4
    ),
    "\n"
  )
  
  
  # ------------------------------------------------------------
  # Group 2
  # ------------------------------------------------------------
  
  cat(
    "\nGroup 2:",
    group2,
    "\n"
  )
  
  cat(
    "Sample size:",
    n2,
    "\n"
  )
  
  cat(
    "Sample mean:",
    round(
      mean2,
      4
    ),
    "\n"
  )
  
  cat(
    "Sample standard deviation:",
    round(
      sd2,
      4
    ),
    "\n"
  )
  
  
  # ------------------------------------------------------------
  # Observed difference
  # ------------------------------------------------------------
  
  cat(
    "\nObserved difference (",
    group1,
    " - ",
    group2,
    "): ",
    round(
      observed_difference,
      4
    ),
    "\n",
    sep = ""
  )
  
  
  # ============================================================
  # Display hypotheses
  # ============================================================
  
  cat("\nHypotheses\n")
  cat("--------------------------------------------\n")
  
  cat(
    "H0: mu_",
    group1,
    " - mu_",
    group2,
    " = 0\n",
    sep = ""
  )
  
  if (alternative == "less") {
    
    cat(
      "Ha: mu_",
      group1,
      " - mu_",
      group2,
      " < 0\n",
      sep = ""
    )
    
  } else if (alternative == "greater") {
    
    cat(
      "Ha: mu_",
      group1,
      " - mu_",
      group2,
      " > 0\n",
      sep = ""
    )
    
  } else {
    
    cat(
      "Ha: mu_",
      group1,
      " - mu_",
      group2,
      " != 0\n",
      sep = ""
    )
  }
  
  cat(
    "\nNumber of randomizations:",
    reps,
    "\n"
  )
  
  cat(
    "Randomization p-value:",
    round(
      p_value,
      4
    ),
    "\n"
  )
  
  
  # ============================================================
  # Optional confidence interval: bootstrap the observed data, not H0.
  # This does not alter the null simulation or its p-value.
  # ============================================================
  ci_result <- NULL
  ci_bootstrap <- NULL
  if (ci) {
  ci_bootstrap <- replicate(reps,
    mean(sample(x1, size = n1, replace = TRUE)) -
    mean(sample(x2, size = n2, replace = TRUE)))
    ci_result <- .bootstrap_interval(ci_bootstrap, conf.level, observed_difference, ci.method)
    .print_ci(ci_result, conf.level, "difference in means (group1 - group2)", ci.method)
  }

  # ============================================================
  # Create histogram information
  # ============================================================
  
  # Flag individual simulated outcomes, not histogram midpoints.
  .plot_randomization_hist(difference_random, extreme, 0, observed_difference,
                           alternative, "Randomized Difference in Means", breaks = 40)


  # ============================================================
  # Return results
  # ============================================================
  
  invisible(
    list(
      
      group1 = group1,
      group2 = group2,
      
      n1 = n1,
      n2 = n2,
      
      mean1 = mean1,
      mean2 = mean2,
      
      sd1 = sd1,
      sd2 = sd2,
      
      observed_difference = observed_difference,
      difference = observed_difference,
      
      alternative = alternative,
      
      lower_cutoff = lower_cutoff,
      upper_cutoff = upper_cutoff,
      
      extreme_count = extreme_count,
      reps = reps,
      p_value = p_value,
      ci = ci_result,
      conf.level = if (ci) conf.level else NULL,
      ci.method = if (ci) ci.method else NULL,
      ci_bootstrap = ci_bootstrap,
      
      difference_random = difference_random,
      randomization_distribution = difference_random
    )
  )
}

# ------------------------------------------------------------
# Standalone bootstrap confidence intervals (no null hypothesis)
# ------------------------------------------------------------
# These functions resample the observed data, NOT a null distribution.
# For proportions, basic and normal intervals are clipped to the
# parameter's possible range; clipping is not a coverage guarantee.

.bootstrap_ci_result <- function(observed, boot, conf.level, ci.method,
                                 parameter, bounds = NULL) {
  interval <- .bootstrap_interval(boot, conf.level, observed, ci.method)
  if (!is.null(bounds)) {
    interval$Lower <- pmax(bounds[1], pmin(bounds[2], interval$Lower))
    interval$Upper <- pmax(bounds[1], pmin(bounds[2], interval$Upper))
  }
  ans <- list(estimate = observed, ci = interval, conf.level = conf.level,
              ci.method = ci.method, ci_bootstrap = boot,
              reps = length(boot), parameter = parameter)
  class(ans) <- "bootstrap_ci_result"
  ans
}

print.bootstrap_ci_result <- function(x, ...) {
  cat(sprintf("%g%% bootstrap confidence intervals for %s\n",
              100 * x$conf.level, x$parameter))
  cat(sprintf("Estimate: %.4f\n", x$estimate))
  print(x$ci, row.names = FALSE)
  invisible(x)
}

bootstrap_one_prop_ci <- function(x, success = "Yes", reps = 10000,
                                  conf.level = 0.95,
                                  ci.method = c("percentile", "basic", "normal")) {
  ci.method <- unique(ci.method)
  reps <- .check_reps(reps)
  .check_ci_options(TRUE, conf.level, ci.method)
  success <- .check_scalar_label(success, "success")
  if (!is.atomic(x) || is.list(x) || is.complex(x))
    stop("x must be an atomic categorical or numeric vector.")
  x <- x[!is.na(x)]
  if (length(x) < 2L) stop("x must have at least two non-missing observations.")
  if (!success %in% as.character(x)) stop("success category not found in x.")
  y <- as.character(x) == success
  estimate <- mean(y)
  boot <- stats::rbinom(reps, size = length(y), prob = estimate) / length(y)
  .bootstrap_ci_result(estimate, boot, conf.level, ci.method, "proportion", c(0,1))
}

bootstrap_one_mean_ci <- function(x, reps = 10000, conf.level = 0.95,
                                  ci.method = c("percentile", "basic", "normal")) {
  ci.method <- unique(ci.method)
  reps <- .check_reps(reps)
  .check_ci_options(TRUE, conf.level, ci.method)
  if (!is.numeric(x) || is.complex(x)) stop("x must be numeric.")
  x <- x[!is.na(x)]
  if (length(x) < 2L || any(!is.finite(x)))
    stop("x must have at least two finite non-missing observations.")
  estimate <- mean(x)
  boot <- replicate(reps, mean(sample(x, length(x), replace = TRUE)))
  .bootstrap_ci_result(estimate, boot, conf.level, ci.method, "mean")
}

.bootstrap_groups <- function(x, group, group1, group2) {
  if (length(x) != length(group)) stop("x and group must have equal lengths.")
  keep <- !is.na(x) & !is.na(group)
  x <- x[keep]
  group <- as.character(group[keep])
  lev <- unique(group)
  if (length(lev) != 2L) stop("group must contain exactly two non-missing groups.")
  if (is.null(group1)) group1 <- lev[1]
  if (is.null(group2)) group2 <- lev[lev != group1][1]
  group1 <- .check_scalar_label(group1, "group1")
  group2 <- .check_scalar_label(group2, "group2")
  if (group1 == group2 || !all(c(group1, group2) %in% lev))
    stop("group1 and group2 must be different existing groups.")
  a <- x[group == group1]
  b <- x[group == group2]
  if (length(a) < 2L || length(b) < 2L)
    stop("Each group must have at least two observations.")
  list(a = a, b = b, group1 = group1, group2 = group2)
}

bootstrap_two_prop_ci <- function(x, group, success = "Yes", group1 = NULL,
                                  group2 = NULL, reps = 10000,
                                  conf.level = 0.95,
                                  ci.method = c("percentile", "basic", "normal")) {
  ci.method <- unique(ci.method)
  reps <- .check_reps(reps)
  .check_ci_options(TRUE, conf.level, ci.method)
  success <- .check_scalar_label(success, "success")
  if (!is.atomic(x) || is.list(x) || is.complex(x))
    stop("x must be an atomic categorical or numeric vector.")
  g <- .bootstrap_groups(x, group, group1, group2)
  if (!success %in% as.character(c(g$a, g$b)))
    stop("success category not found in x.")
  p1 <- mean(as.character(g$a) == success)
  p2 <- mean(as.character(g$b) == success)
  estimate <- p1 - p2
  boot <- stats::rbinom(reps, length(g$a), p1) / length(g$a) -
          stats::rbinom(reps, length(g$b), p2) / length(g$b)
  .bootstrap_ci_result(estimate, boot, conf.level, ci.method,
                       paste0("difference in proportions (",g$group1," - ",g$group2,")"), c(-1,1))
}

bootstrap_two_mean_ci <- function(x, group, group1 = NULL, group2 = NULL,
                                  reps = 10000, conf.level = 0.95,
                                  ci.method = c("percentile", "basic", "normal")) {
  ci.method <- unique(ci.method)
  reps <- .check_reps(reps)
  .check_ci_options(TRUE, conf.level, ci.method)
  if (!is.numeric(x) || is.complex(x)) stop("x must be numeric.")
  g <- .bootstrap_groups(x, group, group1, group2)
  if (any(!is.finite(c(g$a, g$b)))) stop("x must contain finite values.")
  estimate <- mean(g$a) - mean(g$b)
  boot <- replicate(reps,
    mean(sample(g$a, length(g$a), replace = TRUE)) -
    mean(sample(g$b, length(g$b), replace = TRUE)))
  .bootstrap_ci_result(estimate, boot, conf.level, ci.method,
                       paste0("difference in means (",g$group1," - ",g$group2,")"))
}
# ------------------------------------------------------------
# Simulation-based chi-square tests
# ------------------------------------------------------------
# The simulated p-value includes a +1 Monte Carlo correction.
# Chi-square tests always use the right tail.

.chisq_stat <- function(observed, expected) {
  sum((observed - expected)^2 / expected)
}

.plot_chisq_simulation <- function(simulated, observed, title) {
  h <- hist(simulated, breaks = 30, plot = FALSE)
  edges <- sort(unique(c(h$breaks, observed)))
  bins <- findInterval(simulated, edges, rightmost.closed = TRUE)
  # Values exactly equal to the cutoff belong to the extreme tail.
  counts <- tabulate(bins, nbins = length(edges) - 1L)
  extreme <- simulated >= observed - 64 * .Machine$double.eps *
    pmax(1, abs(simulated), abs(observed))
  red <- tabulate(bins[extreme], nbins = length(edges) - 1L)
  plot(range(edges), c(0, max(1, counts) * 1.12), type = "n",
       xlab = expression("Simulated " * chi^2 * " statistic"),
       ylab = "Number of simulations", main = title, bty = "l")
  for (i in seq_along(counts)) {
    if (counts[i] > 0) {
      rect(edges[i], 0, edges[i+1L], counts[i],
           col = "lightgray", border = "white")
      if (red[i] > 0) rect(edges[i], 0, edges[i+1L], red[i],
                            col = "tomato", border = "white")
    }
  }
  abline(v = observed, col = "blue", lwd = 1.2)
  legend("topright", legend = c("Extreme outcomes", "Other outcomes",
                                 "Observed statistic"),
         fill = c("tomato", "lightgray", NA),
         border = c("white", "white", NA),
         lty = c(NA, NA, 1), col = c(NA, NA, "blue"),
         bty = "n", cex = 0.8)
  invisible(NULL)
}

.chisq_result <- function(observed_stat, simulated, expected, observed,
                          test, plot) {
  extreme <- simulated >= observed_stat -
    64 * .Machine$double.eps * pmax(1, abs(simulated), abs(observed_stat))
  ans <- list(test = test, statistic = observed_stat,
              p_value = (sum(extreme) + 1) / (length(simulated) + 1),
              extreme_count = sum(extreme), reps = length(simulated),
              observed = observed, expected = expected,
              simulated_statistics = simulated)
  class(ans) <- "chisq_sim_result"
  if (plot) .plot_chisq_simulation(simulated, observed_stat,
                                  paste0(test, " (simulation)"))
  ans
}

print.chisq_sim_result <- function(x, ...) {
  cat(x$test, "\n", sep = "")
  cat(sprintf("Observed chi-square: %.4f\n", x$statistic))
  cat(sprintf("Simulation p-value: %.5f (%d of %d extreme)\n",
              x$p_value, x$extreme_count, x$reps))
  invisible(x)
}

# Goodness-of-fit: observed category counts, hypothesized probabilities.
# If p0 is omitted, equal category probabilities are assumed.
chisq_gof <- function(observed, p0 = NULL, reps = 10000, plot = TRUE) {
  reps <- .check_reps(reps)
  if (!is.logical(plot) || length(plot) != 1L || is.na(plot))
    stop("plot must be TRUE or FALSE.")
  if (!is.numeric(observed) || length(observed) < 2L ||
      anyNA(observed) || any(!is.finite(observed)) ||
      any(observed < 0) || any(observed != floor(observed)) ||
      sum(observed) == 0)
    stop("observed must contain at least two nonnegative integer counts with a positive total.")
  k <- length(observed)
  if (is.null(p0)) p0 <- rep(1 / k, k)
  if (!is.numeric(p0) || length(p0) != k || anyNA(p0) ||
      any(!is.finite(p0)) || any(p0 <= 0) ||
      abs(sum(p0) - 1) > 1e-8)
    stop("p0 must have one positive probability per category and sum to 1.")
  p0 <- p0 / sum(p0)
  n <- sum(observed)
  if (n > .Machine$integer.max) stop("Total count is too large.")
  expected <- n * p0
  names(expected) <- names(observed)
  stat <- .chisq_stat(observed, expected)
  sims <- rmultinom(reps, size = n, prob = p0)
  sim_stats <- colSums((sims - expected)^2 / expected)
  .chisq_result(stat, sim_stats, expected, observed,
                "Chi-square goodness-of-fit test", plot)
}

# Independence: two categorical vectors, or a contingency table.
# Permuting one variable preserves the observed row/column totals.
chisq_ind <- function(x, y = NULL, reps = 10000, plot = TRUE) {
  reps <- .check_reps(reps)
  if (!is.logical(plot) || length(plot) != 1L || is.na(plot))
    stop("plot must be TRUE or FALSE.")
  if (is.null(y)) {
    if (!is.matrix(x) && !inherits(x, "table"))
      stop("Provide two categorical vectors, or a contingency table.")
    observed <- as.matrix(x)
    if (!is.numeric(observed) || anyNA(observed) ||
        any(!is.finite(observed)) || any(observed < 0) ||
        any(observed != floor(observed)))
      stop("Contingency table must contain nonnegative integer counts.")
    if (nrow(observed) < 2L || ncol(observed) < 2L)
      stop("The table must have at least two rows and two columns.")
    if (sum(observed) > .Machine$integer.max)
      stop("Total count is too large.")
    # Expand row and column labels in matching cell order.
    a <- rep(rep(seq_len(nrow(observed)), times = ncol(observed)),
             times = as.vector(observed))
    b <- rep(rep(seq_len(ncol(observed)), each = nrow(observed)),
             times = as.vector(observed))
  } else {
    if (length(x) != length(y)) stop("x and y must have equal lengths.")
    keep <- !is.na(x) & !is.na(y)
    a <- factor(x[keep]); b <- factor(y[keep])
    if (nlevels(a) < 2L || nlevels(b) < 2L)
      stop("Each variable must have at least two observed categories.")
    observed <- table(a, b)
    a <- as.integer(a); b <- as.integer(b)
  }
  if (any(rowSums(observed) == 0) || any(colSums(observed) == 0))
    stop("Every row and column must have a positive count.")
  n <- length(a)
  expected <- outer(rowSums(observed), colSums(observed)) / n
  dimnames(expected) <- dimnames(observed)
  stat <- .chisq_stat(observed, expected)
  nr <- nrow(observed); nc <- ncol(observed)
  sim_stats <- replicate(reps, {
    perm <- sample(b, size = n, replace = FALSE)
    counts <- matrix(tabulate(a + nr * (perm - 1L), nbins = nr * nc),
                     nrow = nr, ncol = nc)
    .chisq_stat(counts, expected)
  })
  .chisq_result(stat, sim_stats, expected, observed,
                "Chi-square test of independence", plot)
}


# -----------------------------------------------------------------------------
# Data-frame interfaces (also retain ordinary vector/table interfaces).
# Unquoted names are resolved in `data`; explicit data$column works as before.
# -----------------------------------------------------------------------------
.resolve_sim_column <- function(expr, data, caller) {
  if (!is.null(data)) {
    if (!is.data.frame(data)) stop("data must be a data.frame.")
    if (is.symbol(expr)) {
      column <- as.character(expr)
      if (column %in% names(data)) return(data[[column]])
      stop(sprintf("Column '%s' was not found in data.", column))
    }
    return(eval(expr, envir = data, enclos = caller))
  }
  eval(expr, envir = caller)
}

.original_randomization_one_prop_test <- randomization_one_prop_test
randomization_one_prop_test <- function(x, success = 1, p0, alternative = "two.sided", reps = 10000, ci = FALSE, conf.level = 0.95, ci.method = c("percentile", "basic", "normal"), data = NULL) {
  caller <- parent.frame()
  resolved_x <- .resolve_sim_column(substitute(x), data, caller)
  .original_randomization_one_prop_test(x = resolved_x, success = success, p0 = p0, alternative = alternative, reps = reps, ci = ci, conf.level = conf.level, ci.method = ci.method)
}

.original_randomization_one_mean_test <- randomization_one_mean_test
randomization_one_mean_test <- function(x, mu0, alternative = "two.sided", reps = 10000, ci = FALSE, conf.level = 0.95, ci.method = c("percentile", "basic", "normal"), data = NULL) {
  caller <- parent.frame()
  resolved_x <- .resolve_sim_column(substitute(x), data, caller)
  .original_randomization_one_mean_test(x = resolved_x, mu0 = mu0, alternative = alternative, reps = reps, ci = ci, conf.level = conf.level, ci.method = ci.method)
}

.original_randomization_two_prop_test <- randomization_two_prop_test
randomization_two_prop_test <- function(x, group, success = 1, group1 = NULL, group2 = NULL, alternative = "two.sided", reps = 10000, ci = FALSE, conf.level = 0.95, ci.method = c("percentile", "basic", "normal"), data = NULL) {
  caller <- parent.frame()
  resolved_x <- .resolve_sim_column(substitute(x), data, caller)
  resolved_group <- .resolve_sim_column(substitute(group), data, caller)
  .original_randomization_two_prop_test(x = resolved_x, group = resolved_group, success = success, group1 = group1, group2 = group2, alternative = alternative, reps = reps, ci = ci, conf.level = conf.level, ci.method = ci.method)
}

.original_randomization_two_mean_test <- randomization_two_mean_test
randomization_two_mean_test <- function(x, group, group1 = NULL, group2 = NULL, alternative = "two.sided", reps = 10000, ci = FALSE, conf.level = 0.95, ci.method = c("percentile", "basic", "normal"), data = NULL) {
  caller <- parent.frame()
  resolved_x <- .resolve_sim_column(substitute(x), data, caller)
  resolved_group <- .resolve_sim_column(substitute(group), data, caller)
  .original_randomization_two_mean_test(x = resolved_x, group = resolved_group, group1 = group1, group2 = group2, alternative = alternative, reps = reps, ci = ci, conf.level = conf.level, ci.method = ci.method)
}

.original_bootstrap_one_prop_ci <- bootstrap_one_prop_ci
bootstrap_one_prop_ci <- function(x, success = "Yes", reps = 10000, conf.level = 0.95, ci.method = c("percentile", "basic", "normal"), data = NULL) {
  caller <- parent.frame()
  resolved_x <- .resolve_sim_column(substitute(x), data, caller)
  .original_bootstrap_one_prop_ci(x = resolved_x, success = success, reps = reps, conf.level = conf.level, ci.method = ci.method)
}

.original_bootstrap_one_mean_ci <- bootstrap_one_mean_ci
bootstrap_one_mean_ci <- function(x, reps = 10000, conf.level = 0.95, ci.method = c("percentile", "basic", "normal"), data = NULL) {
  caller <- parent.frame()
  resolved_x <- .resolve_sim_column(substitute(x), data, caller)
  .original_bootstrap_one_mean_ci(x = resolved_x, reps = reps, conf.level = conf.level, ci.method = ci.method)
}

.original_bootstrap_two_prop_ci <- bootstrap_two_prop_ci
bootstrap_two_prop_ci <- function(x, group, success = "Yes", group1 = NULL, group2 = NULL, reps = 10000, conf.level = 0.95, ci.method = c("percentile", "basic", "normal"), data = NULL) {
  caller <- parent.frame()
  resolved_x <- .resolve_sim_column(substitute(x), data, caller)
  resolved_group <- .resolve_sim_column(substitute(group), data, caller)
  .original_bootstrap_two_prop_ci(x = resolved_x, group = resolved_group, success = success, group1 = group1, group2 = group2, reps = reps, conf.level = conf.level, ci.method = ci.method)
}

.original_bootstrap_two_mean_ci <- bootstrap_two_mean_ci
bootstrap_two_mean_ci <- function(x, group, group1 = NULL, group2 = NULL, reps = 10000, conf.level = 0.95, ci.method = c("percentile", "basic", "normal"), data = NULL) {
  caller <- parent.frame()
  resolved_x <- .resolve_sim_column(substitute(x), data, caller)
  resolved_group <- .resolve_sim_column(substitute(group), data, caller)
  .original_bootstrap_two_mean_ci(x = resolved_x, group = resolved_group, group1 = group1, group2 = group2, reps = reps, conf.level = conf.level, ci.method = ci.method)
}

.original_chisq_gof <- chisq_gof
chisq_gof <- function(observed, p0 = NULL, reps = 10000, plot = TRUE, data = NULL) {
  caller <- parent.frame()
  resolved_observed <- .resolve_sim_column(substitute(observed), data, caller)
  .original_chisq_gof(observed = resolved_observed, p0 = p0, reps = reps, plot = plot)
}

.original_chisq_ind <- chisq_ind
chisq_ind <- function(x, y = NULL, reps = 10000, plot = TRUE, data = NULL) {
  caller <- parent.frame()
  resolved_x <- .resolve_sim_column(substitute(x), data, caller)
  resolved_y <- if (missing(y) || identical(substitute(y), quote(NULL))) NULL else .resolve_sim_column(substitute(y), data, caller)
  .original_chisq_ind(x = resolved_x, y = resolved_y, reps = reps, plot = plot)
}

