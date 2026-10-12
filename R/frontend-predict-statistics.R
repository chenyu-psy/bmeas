# Bounded basic-family response discrepancies. These are descriptive response
# summaries, not latent covariance estimates, calibrated tests or fit verdicts.
.bh_ppc_stats_cap <- function(budget, name, ceiling, minimum = 1L) {
  value <- budget[[name]]
  if (is.null(value)) return(ceiling)
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value != floor(value) || value < minimum || value > .Machine$integer.max)
    .mp_abort("E_DIAGNOSTIC_BUDGET", paste(name, "must be an integer from", minimum, "to", .Machine$integer.max))
  as.integer(value)
}

.bh_ppc_stats_empty <- function() {
  data.frame(statistic_id = character(), domain = character(), level = character(),
    model = character(), target = character(), target1 = character(), target2 = character(),
    group_index = integer(), pair_index = integer(), metric = character(),
    unit = character(), scale = character(), definition = character(),
    n_units = integer(), unit_type = character(), n_rows = integer(),
    n_declared_rows = integer(), n_excluded_rows = integer(), n_people = integer(),
    n_informative_rows = integer(), n_informative_people = integer(),
    n_zero_trial_rows = integer(), total_trials = double(), overlap = integer(),
    rows_left = integer(), rows_right = integer(), trials_left = double(),
    trials_right = double(), stringsAsFactors = FALSE)
}

.bh_ppc_statistics_plan <- function(spec, draws, budget, level) {
  if(.bh_m3cs_predictive_spec(spec))return(.bh_m3cs_statistics_plan(spec,draws,budget,level))
  portable<-identical(.bh_pf_class(spec),"bmeas_portable_context")
  if(portable)spec<-unclass(spec)
  if (!is.character(level) || length(level) != 1L || is.na(level) ||
      !level %in% c("population", "conditional"))
    .mp_abort("E_CHECK_UNSUPPORTED", "basic-family statistic planning requires an explicit population or conditional level")
  if (!is.list(budget)) .mp_abort("E_DIAGNOSTIC_BUDGET", "an explicit predictive budget is required")
  draws <- .mp_integer(draws, "draws", 2L)
  if (draws > .bh_ppc_stats_cap(budget, "max_draws", 20L, 2L))
    .mp_abort("E_DIAGNOSTIC_BUDGET", "requested replications exceed the predictive budget")
  cap_statistics <- .bh_ppc_stats_cap(budget, "max_statistics", 200L, 0L)
  cap_pairs <- .bh_ppc_stats_cap(budget, "max_pair_terms", 50000L, 0L)
  blocks <- spec$models
  mapped <- spec$data_map$blocks
  if (!is.list(blocks) || !is.list(mapped) || !identical(names(blocks), names(mapped)))
    .mp_abort("E_PREDICTION_SUPPORT", "saved block and consumed-row domains differ")
  rows <- .bh_count_sum(vapply(mapped, nrow, integer(1)))
  if (rows > .bh_ppc_stats_cap(budget, "max_observations", 1000L) ||
      .bh_count_product(draws,rows) > .bh_ppc_stats_cap(budget, "max_simulated_values", 20000L))
    .mp_abort("E_DIAGNOSTIC_BUDGET", "complete consumed response design exceeds the predictive budget")
  roster <- spec$data_map$subject_keys
  if (anyNA(roster) || anyDuplicated(roster))
    .mp_abort("E_PREDICTION_SUPPORT", "saved person roster must contain distinct nonmissing keys")
  roles <- spec$tables$parameter_roles
  if (!is.data.frame(roles) || !all(c("model", "parameter", "task", "key", "role") %in% names(roles)))
    .mp_abort("E_PREDICTION_SUPPORT", "saved authoritative response-parameter roles are required")
  families <- character(length(blocks)); names(families) <- names(blocks)
  role_rows <- vector("list", length(blocks)); names(role_rows) <- names(blocks)
  n_groups <- n_marginal <- 0
  # Count complete declared groups and all pairs before making the expanded
  # statistic/pair tables. In particular fixed-only groups obey the same cap.
  for (b in names(blocks)) {
    family <- .bh_pf_basic_kind(blocks[[b]]$model)
    if (is.na(family))
      .mp_abort("E_CHECK_UNSUPPORTED", paste("unqualified basic likelihood contract in block", b))
    families[b] <- family
    primary <- switch(family, gaussian = "mu", binomial = "p", lognormal = "meanlog")
    at <- which(roles$model == b & roles$parameter == primary & roles$role == "predictor")
    tasks <- .bh_parameter_tasks(blocks[[b]], primary)
    if (length(at) != length(tasks) || anyDuplicated(roles$task[at]) ||
        !setequal(roles$task[at], tasks) ||
        (length(at) && !identical(roles$key[at], targets(b, primary, roles$task[at]))))
      .mp_abort("E_PREDICTION_SUPPORT", paste("saved response-domain roles are incomplete or ambiguous in block", b))
    role_rows[[b]] <- at
    n_groups <- n_groups + length(at)
    n_marginal <- n_marginal + .bh_count_product(length(at), switch(family, gaussian = 4L, binomial = 5L, lognormal = 6L))
    if (n_marginal + .bh_count_product(n_groups, max(0,n_groups - 1)) / 2 > cap_statistics)
      .mp_abort("E_DIAGNOSTIC_BUDGET", "all declared response groups and target pairs exceed max_statistics; no groups or pairs were omitted")
  }
  n_pairs <- .bh_count_product(n_groups, max(0,n_groups - 1)) / 2
  allocation_slots <- .bh_count_sum(.bh_ppc_statistics_admit(draws,n_groups,n_marginal+n_pairs,rows,length(roster),budget))
  groups <- list(); row_indices <- informative_indices <- person_rows <- list()
  for (b in names(blocks)) {
    family <- families[[b]]; block <- blocks[[b]]; d <- mapped[[b]]
    response <- if (family == "binomial") "successes" else "y"
    if (!all(c(".person_key", ".task", ".observation_id", response) %in% names(d)) ||
        anyNA(d$.person_key) || any(!d$.person_key %in% roster))
      .mp_abort("E_PREDICTION_SUPPORT", paste("incomplete retained response/person domain in block", b))
    if (family == "binomial" && (!"trials" %in% names(d) || any(!is.finite(d$trials)) ||
        any(d$trials < 0 | d$trials != floor(d$trials))))
      .mp_abort("E_PREDICTION_SUPPORT", paste("invalid retained trial counts in block", b))
    if (any(!d$.task %in% roles$task[role_rows[[b]]]))
      .mp_abort("E_PREDICTION_SUPPORT", paste("consumed rows fall outside declared response groups in block", b))
    for (r in role_rows[[b]]) {
      j <- length(groups) + 1L; task <- roles$task[r]
      index <- which(d$.task == task)
      informative <- if (family == "binomial") index[d$trials[index] > 0] else index
      people <- roster[roster %in% d$.person_key[informative]]
      per_person <- lapply(people, function(p) informative[d$.person_key[informative] == p])
      names(per_person) <- people
      declared <- sum(block$data$.task == task)
      if (declared < length(index))
        .mp_abort("E_PREDICTION_SUPPORT", "consumed response count exceeds declared response count")
      primary <- roles$parameter[r]
      groups[[j]] <- data.frame(group_id = .bh_public_id("ppc_group", b, primary, task),
        block = b, target = roles$key[r], parameter = primary, task = task, family = family,
        response_field = response, unit = block$units,
        summary_unit = if (family == "binomial") "probability" else block$units,
        n_rows = length(index), n_declared_rows = declared,
        n_excluded_rows = declared - length(index), n_people = length(unique(d$.person_key[index])),
        n_informative_rows = length(informative), n_informative_people = length(people),
        n_zero_trial_rows = if (family == "binomial") length(index) - length(informative) else 0L,
        total_trials = if (family == "binomial") sum(d$trials[index]) else NA_real_,
        stringsAsFactors = FALSE)
      row_indices[[j]] <- index; informative_indices[[j]] <- informative; person_rows[[j]] <- per_person
    }
  }
  groups <- if (length(groups)) do.call(rbind, groups) else data.frame(
    group_id = character(), block = character(), target = character(), parameter = character(),
    task = character(), family = character(), response_field = character(), unit = character(),
    summary_unit = character(), n_rows = integer(), n_declared_rows = integer(),
    n_excluded_rows = integer(), n_people = integer(), n_informative_rows = integer(),
    n_informative_people = integer(), n_zero_trial_rows = integer(), total_trials = double(),
    stringsAsFactors = FALSE)
  names(row_indices) <- names(informative_indices) <- names(person_rows) <- groups$group_id
  # Count overlap terms before constructing pair rows or overlap lists.
  pair_overlap <- 0
  if (n_groups >= 2L) for (i in seq_len(n_groups - 1L)) for (j in seq.int(i + 1L, n_groups)) {
    pair_overlap <- pair_overlap + sum(names(person_rows[[i]]) %in% names(person_rows[[j]]))
    if (.bh_count_product(.bh_count_sum(c(draws,1)),pair_overlap) > cap_pairs)
      .mp_abort("E_DIAGNOSTIC_BUDGET", "all retained paired-person terms exceed max_pair_terms; no pairs were omitted")
  }
  pairs <- list(); pair_people <- list()
  if (n_groups >= 2L) for (i in seq_len(n_groups - 1L)) for (j in seq.int(i + 1L, n_groups)) {
    common <- roster[roster %in% names(person_rows[[i]]) & roster %in% names(person_rows[[j]])]
    p <- length(pairs) + 1L
    pairs[[p]] <- data.frame(pair_id = .bh_public_id("ppc_pair", groups$target[i], groups$target[j]),
      left = i, right = j, target1 = groups$target[i], target2 = groups$target[j],
      overlap = length(common), stringsAsFactors = FALSE)
    pair_people[[p]] <- common
  }
  pairs <- if (length(pairs)) do.call(rbind, pairs) else data.frame(pair_id = character(),
    left = integer(), right = integer(), target1 = character(), target2 = character(),
    overlap = integer(), stringsAsFactors = FALSE)
  names(pair_people) <- pairs$pair_id
  statistics <- list()
  for (j in seq_len(nrow(groups))) {
    g <- groups[j, ]; family <- g$family
    metrics <- if (family == "binomial") c("total_successes", "weighted_success_rate",
      "row_rate_sd_given_trials", "zero_success_rows", "all_success_rows") else
      c("mean", "median", "sd", "q90", if (family == "lognormal") c("log_mean", "log_sd"))
    for (metric in metrics) {
      unit <- if (family == "binomial") switch(metric, total_successes = "count",
        zero_success_rows = "row_count", all_success_rows = "row_count", "probability") else
        if (metric %in% c("log_mean", "log_sd")) paste0("log(response/", g$unit, ")") else g$unit
      definition <- switch(metric, mean = "arithmetic mean of consumed responses",
        median = "median of consumed responses", sd = "sample SD of consumed responses; denominator n-1",
        q90 = "empirical response quantile at 0.90; R quantile type 7",
        log_mean = "arithmetic mean of natural-log responses in the recorded response unit",
        log_sd = "sample SD of natural-log responses; denominator n-1",
        total_successes = "sum of successes on rows with positive trials",
        weighted_success_rate = "sum of successes divided by sum of trials on positive-trial rows",
        row_rate_sd_given_trials = "sample SD of row successes/trials; positive-trial rows, trials held fixed",
        zero_success_rows = "count of positive-trial rows with zero successes",
        all_success_rows = "count of positive-trial rows with successes equal to trials")
      statistics[[length(statistics) + 1L]] <- data.frame(
        statistic_id = .bh_public_id("ppc_statistic", g$target, metric), domain = "marginal", level = level,
        model = g$block, target = g$target, target1 = g$target, target2 = NA_character_,
        group_index = j, pair_index = NA_integer_, metric = metric, unit = unit,
        scale = if (metric %in% c("log_mean", "log_sd")) "log_response" else if (family == "binomial")
          if (metric %in% c("weighted_success_rate", "row_rate_sd_given_trials")) "response_rate" else "count" else "response",
        definition = definition, n_units = g$n_informative_rows,
        unit_type = if (family == "binomial") "positive-trial rows" else "consumed rows",
        n_rows = g$n_rows, n_declared_rows = g$n_declared_rows, n_excluded_rows = g$n_excluded_rows,
        n_people = g$n_people, n_informative_rows = g$n_informative_rows,
        n_informative_people = g$n_informative_people, n_zero_trial_rows = g$n_zero_trial_rows,
        total_trials = g$total_trials, overlap = NA_integer_, rows_left = NA_integer_,
        rows_right = NA_integer_, trials_left = NA_real_, trials_right = NA_real_, stringsAsFactors = FALSE)
    }
  }
  for (p in seq_len(nrow(pairs))) {
    pair <- pairs[p, ]; a <- groups[pair$left, ]; b <- groups[pair$right, ]; common <- pair_people[[p]]
    left <- unlist(person_rows[[pair$left]][common], use.names = FALSE)
    right <- unlist(person_rows[[pair$right]][common], use.names = FALSE)
    statistics[[length(statistics) + 1L]] <- data.frame(
      statistic_id = .bh_public_id("ppc_statistic", a$target, b$target, "paired_person_covariance"),
      domain = if (level == "population") "measurement_structure" else "conditional_association", level = level,
      model = "across_response_groups", target = pair$pair_id, target1 = a$target, target2 = b$target,
      group_index = NA_integer_, pair_index = p, metric = "paired_person_covariance",
      unit = paste(a$summary_unit, b$summary_unit, sep = " * "), scale = "response_summary_covariance",
      definition = paste("Sample covariance on exactly the retained common people; denominator overlap-1.",
        "Per-person response mean, or summed successes/summed positive trials for binomial; not latent R or Sigma."),
      n_units = pair$overlap, unit_type = "paired people", n_rows = length(left) + length(right),
      n_declared_rows = a$n_declared_rows + b$n_declared_rows,
      n_excluded_rows = a$n_excluded_rows + b$n_excluded_rows, n_people = pair$overlap,
      n_informative_rows = length(left) + length(right), n_informative_people = pair$overlap,
      n_zero_trial_rows = a$n_zero_trial_rows + b$n_zero_trial_rows, total_trials = NA_real_,
      overlap = pair$overlap, rows_left = length(left), rows_right = length(right),
      trials_left = if (a$family == "binomial") sum(mapped[[a$block]]$trials[left]) else NA_real_,
      trials_right = if (b$family == "binomial") sum(mapped[[b$block]]$trials[right]) else NA_real_, stringsAsFactors = FALSE)
  }
  statistics <- if (length(statistics)) do.call(rbind, statistics) else .bh_ppc_stats_empty()
  rownames(groups) <- rownames(pairs) <- rownames(statistics) <- NULL
  list(allocation_slots=allocation_slots,version = "basic-family-statistics-v1.1", level = level, draws = draws, groups = groups,
    row_indices = row_indices, informative_row_indices = informative_indices, person_rows = person_rows,
    pairs = pairs, pair_person_keys = pair_people, statistics = statistics,
    counts = list(groups = n_groups, marginal_statistics = n_marginal, pair_statistics = n_pairs,
      statistics = n_marginal + n_pairs, pair_overlap = pair_overlap, pair_terms = .bh_count_product(.bh_count_sum(c(draws,1)),pair_overlap)),
    limits = list(max_statistics = cap_statistics, max_pair_terms = cap_pairs),
    quantile_type = 7L, status = if (nrow(statistics)) "planned" else "no_statistics")
}

.bh_ppc_statistics <- function(spec, plan, responses = NULL) {
  if(identical(plan$version,"m3cs-statistics-v1"))return(.bh_m3cs_statistics(spec,plan,responses))
  portable<-identical(.bh_pf_class(spec),"bmeas_portable_context")
  if(portable)spec<-unclass(spec)
  if (!is.list(plan) || !identical(plan$version, "basic-family-statistics-v1.1"))
    .mp_abort("E_PREDICTION_SUPPORT", "a current bounded basic-family statistic plan is required")
  blocks <- names(spec$models)
  if (!is.null(responses) && (!is.list(responses) || anyDuplicated(names(responses)) ||
      !setequal(names(responses), blocks)))
    .mp_abort("E_PREDICTION_SUPPORT", "replicated responses must name every consumed block exactly once")
  values <- vector("list", length(blocks)); names(values) <- blocks
  for (b in blocks) {
    family <- .bh_pf_basic_kind(spec$models[[b]]$model)
    if (is.na(family)) .mp_abort("E_CHECK_UNSUPPORTED", paste("unqualified basic likelihood contract in block", b))
    d <- spec$data_map$blocks[[b]]
    y <- if (is.null(responses)) d[[if (family == "binomial") "successes" else "y"]] else responses[[b]]
    if (!is.numeric(y) || !is.null(dim(y)) || length(y) != nrow(d) || any(!is.finite(y)))
      .mp_abort("E_PREDICTION_SUPPORT", paste("nonfinite or wrong-length response vector in block", b))
    if (family == "binomial" && any(y != floor(y) | y < 0 | y > d$trials) ||
        family == "lognormal" && any(y <= 0))
      .mp_abort("E_PREDICTION_SUPPORT", paste("response values outside basic-family support in block", b))
    values[[b]] <- y
  }
  out <- plan$statistics
  out$estimate <- rep(NA_real_, nrow(out)); out$status <- rep("not_assessed", nrow(out))
  out$degenerate <- rep(FALSE, nrow(out)); out$low_information <- rep(FALSE, nrow(out))
  person_values <- vector("list", nrow(plan$groups))
  for (j in seq_len(nrow(plan$groups))) {
    g <- plan$groups[j, ]; all_y <- values[[g$block]]
    index <- plan$informative_row_indices[[j]]; y <- all_y[index]
    trials <- spec$data_map$blocks[[g$block]]$trials
    rates <- if (g$family == "binomial") y / trials[index] else y
    pv <- vapply(plan$person_rows[[j]], function(at) if (g$family == "binomial")
      sum(all_y[at]) / sum(trials[at]) else mean(all_y[at]), numeric(1))
    if (any(!is.finite(pv))) .mp_abort("E_PREDICTION_SUPPORT", "nonrepresentable per-person response summary")
    person_values[[j]] <- pv
    degenerate <- length(rates) >= 2L && all(rates == rates[1L])
    for (r in which(out$group_index == j)) {
      metric <- out$metric[r]
      if (!g$n_rows) { out$status[r] <- "no_observations"; next }
      if (!length(y)) { out$status[r] <- "no_positive_trials"; next }
      out$degenerate[r] <- degenerate
      out$low_information[r] <- degenerate || length(y) < 2L
      if (metric %in% c("sd", "log_sd", "row_rate_sd_given_trials") && length(y) < 2L) {
        out$status[r] <- "insufficient_rows"; next
      }
      value <- switch(metric, mean = mean(y), median = stats::median(y), sd = stats::sd(y),
        q90 = unname(stats::quantile(y, 0.9, type = 7L)), log_mean = mean(log(y)), log_sd = stats::sd(log(y)),
        total_successes = sum(y), weighted_success_rate = sum(y) / sum(trials[index]),
        row_rate_sd_given_trials = stats::sd(rates), zero_success_rows = sum(y == 0),
        all_success_rows = sum(y == trials[index]))
      if (length(value) != 1L || !is.finite(value))
        .mp_abort("E_PREDICTION_SUPPORT", paste("nonrepresentable discrepancy", out$statistic_id[r]))
      out$estimate[r] <- value; out$status[r] <- "computed"
    }
  }
  for (p in seq_len(nrow(plan$pairs))) {
    r <- which(out$pair_index == p); pair <- plan$pairs[p, ]; common <- plan$pair_person_keys[[p]]
    if (length(common) < 2L) {
      out$status[r] <- if (length(common)) "insufficient_paired_people" else "no_paired_people"
      out$low_information[r] <- TRUE
      next
    }
    x <- person_values[[pair$left]][common]; y <- person_values[[pair$right]][common]
    degenerate <- all(x == x[1L]) || all(y == y[1L])
    value <- if (degenerate) 0 else stats::cov(x, y)
    if (!is.finite(value)) .mp_abort("E_PREDICTION_SUPPORT", paste("nonrepresentable covariance", out$statistic_id[r]))
    out$estimate[r] <- value; out$status[r] <- "computed"
    out$degenerate[r] <- degenerate; out$low_information[r] <- degenerate
  }
  out$low_information[out$status != "computed"] <- TRUE
  attr(out, "coverage") <- list(planned = nrow(out), computed = sum(out$status == "computed"),
    unavailable = sum(out$status != "computed"), status = if (!nrow(out)) "no_statistics" else
      if (all(out$status == "computed")) "complete" else if (any(out$status == "computed")) "partial" else "unavailable")
  attr(out, "level") <- plan$level
  out
}

.bh_ppc_aggregate <- function(observed, reps) {
  if (!is.data.frame(observed) || !all(c("statistic_id", "estimate", "status", "domain", "level") %in% names(observed)) ||
      anyDuplicated(observed$statistic_id) || !is.matrix(reps) || !is.numeric(reps) ||
      ncol(reps) != nrow(observed) || nrow(reps) < 2L || 
      !identical(if (is.null(colnames(reps))) character() else colnames(reps), observed$statistic_id))
    .mp_abort("E_PREDICTION_SUPPORT", "replicate aggregates require a bounded D-by-statistic matrix in exact planned statistic-ID order")
  table <- observed; table$observed <- table$estimate; table$estimate <- NULL
  table$replicated_median <- table$lower90 <- table$upper90 <- rep(NA_real_, nrow(table))
  table$finite_replications <- colSums(is.finite(reps)); table$requested_replications <- rep(nrow(reps), nrow(table))
  table$outside_interval <- rep(NA, nrow(table)); table$quantile_type <- rep(7L, nrow(table))
  table$replication_coverage <- ifelse(table$finite_replications == nrow(reps), "complete",
    ifelse(table$finite_replications == 0L, "unavailable", "partial"))
  for (j in seq_len(nrow(table))) {
    values <- reps[, j]; values <- values[is.finite(values)]
    if (length(values) >= 2L) {
      q <- stats::quantile(values, c(0.05, 0.5, 0.95), type = 7L, names = FALSE)
      table$lower90[j] <- q[1L]; table$replicated_median[j] <- q[2L]; table$upper90[j] <- q[3L]
      if (is.finite(table$observed[j]))
        table$outside_interval[j] <- table$observed[j] < q[1L] || table$observed[j] > q[3L]
    }
  }
  if (is.null(rownames(reps))) rownames(reps) <- paste0("replication_", seq_len(nrow(reps)))
  evaluable <- table$status == "computed" & is.finite(table$observed) & table$finite_replications >= 2L
  complete <- evaluable & table$finite_replications == nrow(reps)
  coverage <- function(at) list(planned = sum(at), evaluable = sum(evaluable[at]), complete = sum(complete[at]),
    unavailable = sum(!evaluable[at]), status = if (!any(at)) "unavailable" else
      if (all(complete[at])) "complete" else if (any(evaluable[at])) "partial" else "unavailable")
  level <- unique(table$level)
  if (!length(level)) level <- attr(observed, "level")
  paired <- table$domain == "measurement_structure"
  measurement <- coverage(paired)
  if (identical(level, "conditional")) {
    measurement$status <- "unassessed"
    measurement$reason <- "conditional response checks only; population measurement structure is unassessed"
  } else if (!any(paired)) measurement$reason <- "no cross-target statistics were planned" else
    if (!any(evaluable[paired])) measurement$reason <- "no planned paired-person covariance is evaluable"
  list(table = table, replicate_statistics = reps, quantile_type = 7L,
    coverage = coverage(rep(TRUE, nrow(table))), measurement_structure = measurement,
    conditional_association = coverage(table$domain == "conditional_association"),
    interpretation = "coarse descriptive finite-replication intervals; no calibrated p-value or good-fit verdict")
}
