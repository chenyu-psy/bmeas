# Descriptive CS response discrepancies. Counts are pooled within person/task;
# people, rather than rows or trials, have equal weight in the summaries.
.bh_m3cs_statistics_plan <- function(spec, draws, budget, level) {
  if (!is.character(level) || length(level) != 1L || is.na(level) ||
      !level %in% c("population", "conditional"))
    .mp_abort("E_CHECK_UNSUPPORTED", "M3-CS statistics require population or conditional level")
  if (!is.list(budget)) .mp_abort("E_DIAGNOSTIC_BUDGET", "an explicit predictive budget is required")
  draws <- .mp_integer(draws, "draws", 2L)
  cap_statistics <- .bh_ppc_stats_cap(budget, "max_statistics", 200L, 0L)
  cap_pairs <- .bh_ppc_stats_cap(budget, "max_pair_terms", 50000L, 0L)
  if (draws > .bh_ppc_stats_cap(budget, "max_draws", 20L, 2L))
    .mp_abort("E_DIAGNOSTIC_BUDGET", "requested replications exceed the predictive budget")
  blocks <- spec$models; mapped <- spec$data_map$blocks
  if (!is.list(blocks) || !length(blocks) || length(blocks)>2L || is.null(names(blocks)) ||
      anyDuplicated(names(blocks)) || !is.list(mapped) || !identical(names(blocks), names(mapped)) ||
      (length(blocks)==1L&&!identical(blocks[[1L]]$model$name, "m3_cs_reference")) ||
      (length(blocks)==2L&&!.bh_m3cs_joint_spec(spec)))
    .mp_abort("E_CHECK_UNSUPPORTED", "M3-CS statistics require one or two canonical M3-CS blocks")
  active <- c(1L, 2L, 3L, 4L, 10L)
  roster <- spec$data_map$subject_keys
  if (anyNA(roster) || anyDuplicated(roster))
    .mp_abort("E_PREDICTION_SUPPORT", "invalid retained M3-CS person roster")
  O <- .bh_count_sum(vapply(mapped,nrow,integer(1)))
  if (O > .bh_ppc_stats_cap(budget, "max_observations", 1000L) ||
      .bh_count_product(draws,O,10) > .bh_ppc_stats_cap(budget, "max_simulated_values", 20000L))
    .mp_abort("E_DIAGNOSTIC_BUDGET", "complete consumed response design exceeds the predictive budget")
  tasks_by_block <- totals_by_block <- task_people <- list()
  for(b in names(blocks)) {
    block<-blocks[[b]];d<-mapped[[b]]
    if (!is.data.frame(d) || !all(c(".task", ".person_key", ".observation_id", "counts", "options") %in% names(d)))
      .mp_abort("E_PREDICTION_SUPPORT", "incomplete consumed M3-CS response domain")
    .bh_m3cs_counts(d$counts,d$options,nrow(d))
    if (anyNA(d$.person_key) || any(!d$.person_key %in% roster) || anyNA(d$.task) ||
        anyNA(d$.observation_id) || anyDuplicated(d$.observation_id))
      .mp_abort("E_PREDICTION_SUPPORT", "invalid retained M3-CS person/task/observation identities")
    tasks <- sort(unique(block$data$.task))
    if (anyNA(tasks) || any(!d$.task %in% tasks))
      .mp_abort("E_PREDICTION_SUPPORT", "consumed M3-CS rows fall outside declared tasks")
    tasks_by_block[[b]]<-tasks
    totals_by_block[[b]]<-rowSums(d$counts[,active,drop=FALSE])
  }
  nt <- sum(lengths(tasks_by_block)); ng <- .bh_count_product(5,nt)
  nm <- .bh_count_product(10,nt); np <- .bh_count_product(25,nt,max(0,nt-1))/2
  # Every block/task is a distinct domain, including duplicate task labels.
  if (nm + np > cap_statistics)
    .mp_abort("E_DIAGNOSTIC_BUDGET", "all M3-CS task/category statistics exceed max_statistics; none were omitted")
  allocation_slots <- .bh_count_sum(.bh_ppc_statistics_admit(draws,ng,nm+np,O,length(roster),budget))
  for(b in names(blocks)) {
    d<-mapped[[b]]
    for(task in tasks_by_block[[b]])task_people[[length(task_people)+1L]]<-
      unique(d$.person_key[d$.task==task & totals_by_block[[b]]>0])
  }
  pair_overlap <- 0
  if (nt >= 2L) for (i in seq_len(nt - 1L)) for (j in seq.int(i + 1L, nt)) {
    pair_overlap <- pair_overlap + 25 * sum(task_people[[i]] %in% task_people[[j]])
    if (.bh_count_product(.bh_count_sum(c(draws,1)),pair_overlap) > cap_pairs)
      .mp_abort("E_DIAGNOSTIC_BUDGET", "all M3-CS paired-person terms exceed max_pair_terms; none were omitted")
  }
  groups <- row_indices <- informative_indices <- person_rows <- list()
  for(b in names(blocks)) {
    block<-blocks[[b]];d<-mapped[[b]];totals<-totals_by_block[[b]]
    for (task in tasks_by_block[[b]]) {
    index <- which(d$.task == task); informative <- index[totals[index] > 0]
    people <- roster[roster %in% d$.person_key[informative]]
    pr <- lapply(people, function(p) informative[d$.person_key[informative] == p]); names(pr) <- people
    declared <- sum(block$data$.task == task)
    if (declared < length(index)) .mp_abort("E_PREDICTION_SUPPORT", "consumed rows exceed declared rows")
    for (category in active) {
      j <- length(groups) + 1L
      target <- .bh_public_id("m3cs_category", b, task, as.character(category))
      groups[[j]] <- data.frame(group_id = .bh_public_id("ppc_group", target), block = b,
        target = target, parameter = "category_rate", task = task, category_index = category,
        family = "m3_cs_reference", response_field = "counts", unit = "probability", summary_unit = "probability",
        n_rows = length(index), n_declared_rows = declared, n_excluded_rows = declared - length(index),
        n_people = length(unique(d$.person_key[index])), n_informative_rows = length(informative),
        n_informative_people = length(people), n_zero_trial_rows = length(index) - length(informative),
        total_trials = sum(totals[index]), stringsAsFactors = FALSE)
      row_indices[[j]] <- index; informative_indices[[j]] <- informative; person_rows[[j]] <- pr
    }
  }
  }
  groups <- if (length(groups)) do.call(rbind, groups) else data.frame(group_id = character(), task = character())
  names(row_indices) <- names(informative_indices) <- names(person_rows) <- groups$group_id
  pairs <- pair_people <- list()
  if (ng >= 2L) for (i in seq_len(ng - 1L)) for (j in seq.int(i + 1L, ng)) {
    if (groups$block[i] == groups$block[j] && groups$task[i] == groups$task[j]) next
    common <- roster[roster %in% names(person_rows[[i]]) & roster %in% names(person_rows[[j]])]
    p <- length(pairs) + 1L
    pairs[[p]] <- data.frame(pair_id = .bh_public_id("ppc_pair", groups$target[i], groups$target[j]),
      left = i, right = j, target1 = groups$target[i], target2 = groups$target[j],
      overlap = length(common), stringsAsFactors = FALSE)
    pair_people[[p]] <- common
  }
  pairs <- if (length(pairs)) do.call(rbind, pairs) else data.frame(pair_id = character(),
    left = integer(), right = integer(), target1 = character(), target2 = character(), overlap = integer())
  names(pair_people) <- pairs$pair_id
  statistics <- list()
  for (j in seq_len(nrow(groups))) {
    g <- groups[j, ]
    for (metric in c("person_category_rate_mean", "person_category_rate_sd")) {
      statistics[[length(statistics) + 1L]] <- data.frame(
        statistic_id = .bh_public_id("ppc_statistic", g$target, metric), domain = "marginal", level = level,
        model = g$block, target = g$target, target1 = g$target, target2 = NA_character_, group_index = j,
        pair_index = NA_integer_, metric = metric, unit = "probability", scale = "response_rate",
        definition = paste("Category", g$category_index, "in task", g$task,
          if (metric == "person_category_rate_mean") "equal-person mean" else "equal-person sample SD; denominator people-1",
          "of pooled category counts / pooled active-category total within person/task; zero-total people excluded."),
        n_units = g$n_informative_people, unit_type = "positive-total people", n_rows = g$n_rows,
        n_declared_rows = g$n_declared_rows, n_excluded_rows = g$n_excluded_rows, n_people = g$n_people,
        n_informative_rows = g$n_informative_rows, n_informative_people = g$n_informative_people,
        n_zero_trial_rows = g$n_zero_trial_rows, total_trials = g$total_trials, overlap = NA_integer_,
        rows_left = NA_integer_, rows_right = NA_integer_, trials_left = NA_real_, trials_right = NA_real_,
        stringsAsFactors = FALSE)
    }
  }
  for (p in seq_len(nrow(pairs))) {
    pair <- pairs[p, ]; a <- groups[pair$left, ]; z <- groups[pair$right, ]; common <- pair_people[[p]]
    left <- unlist(person_rows[[pair$left]][common], use.names = FALSE)
    right <- unlist(person_rows[[pair$right]][common], use.names = FALSE)
    statistics[[length(statistics) + 1L]] <- data.frame(
      statistic_id = .bh_public_id("ppc_statistic", a$target, z$target, "paired_person_covariance"),
      domain = if (level == "population") "measurement_structure" else "conditional_association", level = level,
      model = "across_response_groups", target = pair$pair_id, target1 = a$target, target2 = z$target,
      group_index = NA_integer_, pair_index = p, metric = "paired_person_covariance", unit = "probability * probability",
      scale = "response_summary_covariance", definition = paste("Sample covariance of pooled person category rates:",
        "task", a$task, "category", a$category_index, "with task", z$task, "category", z$category_index,
        "; exactly the positive-total common people; denominator overlap-1; not latent R or Sigma."),
      n_units = pair$overlap, unit_type = "paired people", n_rows = length(left) + length(right),
      n_declared_rows = a$n_declared_rows + z$n_declared_rows, n_excluded_rows = a$n_excluded_rows + z$n_excluded_rows,
      n_people = pair$overlap, n_informative_rows = length(left) + length(right), n_informative_people = pair$overlap,
      n_zero_trial_rows = a$n_zero_trial_rows + z$n_zero_trial_rows, total_trials = NA_real_, overlap = pair$overlap,
      rows_left = length(left), rows_right = length(right), trials_left = sum(totals_by_block[[a$block]][left]), trials_right = sum(totals_by_block[[z$block]][right]),
      stringsAsFactors = FALSE)
  }
  statistics <- if (length(statistics)) do.call(rbind, statistics) else .bh_ppc_stats_empty()
  rownames(groups) <- rownames(pairs) <- rownames(statistics) <- NULL
  list(allocation_slots=allocation_slots,version = "m3cs-statistics-v1", level = level, draws = draws, groups = groups,
    row_indices = row_indices, informative_row_indices = informative_indices, person_rows = person_rows,
    pairs = pairs, pair_person_keys = pair_people, statistics = statistics, active_categories = active,
    counts = list(groups = ng, marginal_statistics = nm, pair_statistics = np, statistics = nm + np,
      pair_overlap = pair_overlap, pair_terms = .bh_count_product(.bh_count_sum(c(draws,1)),pair_overlap)),
    limits = list(max_statistics = cap_statistics, max_pair_terms = cap_pairs),
    quantile_type = 7L, status = if (nrow(statistics)) "planned" else "no_statistics")
}

.bh_m3cs_statistics <- function(spec, plan, responses = NULL) {
  if (!is.list(plan) || !identical(plan$version, "m3cs-statistics-v1"))
    .mp_abort("E_PREDICTION_SUPPORT", "a current bounded M3-CS statistic plan is required")
  blocks <- names(spec$models)
  if (!length(blocks) || length(blocks)>2L || (!is.null(responses) && (!is.list(responses) ||
      anyDuplicated(names(responses)) || !setequal(names(responses), blocks))))
    .mp_abort("E_PREDICTION_SUPPORT", "replicated counts must name every consumed block exactly once")
  values<-totals_by_block<-list()
  for(b in blocks) {
    d<-spec$data_map$blocks[[b]]
    y<-if(is.null(responses))d$counts else responses[[b]]
    totals_by_block[[b]]<-rowSums(d$counts)
    .bh_m3cs_counts(y,d$options,nrow(d),totals_by_block[[b]])
    values[[b]]<-y
  }
  out <- plan$statistics
  out$estimate <- rep(NA_real_, nrow(out)); out$status <- rep("not_assessed", nrow(out))
  out$degenerate <- rep(FALSE, nrow(out)); out$low_information <- rep(FALSE, nrow(out))
  person_values <- vector("list", nrow(plan$groups))
  for (j in seq_len(nrow(plan$groups))) {
    g <- plan$groups[j, ];y<-values[[g$block]];totals<-totals_by_block[[g$block]]
    pv <- vapply(plan$person_rows[[j]], function(at) sum(y[at, g$category_index]) / sum(totals[at]), numeric(1))
    if (any(!is.finite(pv))) .mp_abort("E_PREDICTION_SUPPORT", "nonrepresentable per-person category rate")
    person_values[[j]] <- pv
    degenerate <- length(pv) >= 2L && all(pv == pv[1L])
    for (r in which(out$group_index == j)) {
      if (!g$n_rows) { out$status[r] <- "no_observations"; next }
      if (!length(pv)) { out$status[r] <- "no_positive_trials"; next }
      out$degenerate[r] <- degenerate; out$low_information[r] <- degenerate || length(pv) < 2L
      if (out$metric[r] == "person_category_rate_sd" && length(pv) < 2L) {
        out$status[r] <- "insufficient_people"; next
      }
      out$estimate[r] <- if (out$metric[r] == "person_category_rate_mean") mean(pv) else stats::sd(pv)
      out$status[r] <- "computed"
    }
  }
  for (p in seq_len(nrow(plan$pairs))) {
    r <- which(out$pair_index == p); pair <- plan$pairs[p, ]; common <- plan$pair_person_keys[[p]]
    if (length(common) < 2L) {
      out$status[r] <- if (length(common)) "insufficient_paired_people" else "no_paired_people"
      out$low_information[r] <- TRUE; next
    }
    x <- person_values[[pair$left]][common]; z <- person_values[[pair$right]][common]
    degenerate <- all(x == x[1L]) || all(z == z[1L])
    value <- if (degenerate) 0 else stats::cov(x, z)
    if (!is.finite(value)) .mp_abort("E_PREDICTION_SUPPORT", "nonrepresentable category-rate covariance")
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
