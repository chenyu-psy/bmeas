# v3 count admission bounds declared slots/work, never allocator overhead or RSS.
.bh_ppc_work_admit <- function(counts,budget) {
  total <- .bh_count_sum(counts,"predictive allocation/work slots")
  cap <- .bh_ppc_stats_cap(budget,"max_work_values",1000000L,0L)
  if(any(counts>.Machine$integer.max)||total>cap)
    .mp_abort("E_DIAGNOSTIC_BUDGET","predictive allocation/work slots exceed max_work_values or representable dimensions before allocation")
  invisible(total)
}
.bh_ppc_geometry_admit <- function(N,J,K,U,S,JT,budget) {
  dims <- c(N,J,K,U,S,JT)
  for(x in dims) .bh_count(x,"predictive dimension",upper=.Machine$integer.max)
  q <- .bh_count_sum(c(K,U))
  counts <- c(predictor_slots=.bh_count_product(4,N,J),
    generated_coordinate_slots=.bh_count_product(2,N,q),
    centered_coordinate_slots=.bh_count_product(3,JT,N),
    loading_slots=.bh_count_product(J,q),
    covariance_slots=.bh_count_sum(c(.bh_count_product(K,K),.bh_count_product(U,U),.bh_count_product(JT,JT))),
    scalar_name_slots=.bh_count_sum(c(J,S,.bh_count_product(N,J),.bh_count_product(J,q),.bh_count_product(K,K),.bh_count_product(U,U),.bh_count_product(JT,JT))))
  .bh_ppc_work_admit(counts,budget)
  as.list(counts)
}
.bh_ppc_statistics_admit <- function(draws,groups,statistics,rows,people,budget) {
  counts <- c(replicate_statistic_slots=.bh_count_product(draws,statistics),
    statistic_table_slots=.bh_count_product(40,statistics),
    person_group_slots=.bh_count_product(people,groups),
    row_map_slots=.bh_count_product(8,rows),
    pair_scan_work=.bh_count_product(groups,groups,people))
  .bh_ppc_work_admit(counts,budget)
  invisible(counts)
}
.bh_ppc_preflight_work <- function(spec,draws,budget,level) {
  O <- .bh_count_sum(vapply(spec$data_map$blocks,nrow,integer(1)))
  cells <- if(.bh_m3cs_predictive_spec(spec))10 else 1
  N <- nrow(spec$tables$person_roster)
  latent <- if(level=='conditional')0 else if(identical(spec$tables$person_law$distribution,'iid'))
    .bh_count_product(N,.bh_count_sum(c(nrow(spec$tables$components),nrow(spec$tables$parameter_residuals)))) else
    .bh_count_product(length(spec$tables$augmentation$target_index),max(0,N-1))
  if(O>budget$max_observations || .bh_count_product(draws,O,cells)>budget$max_simulated_values ||
     .bh_count_product(draws,latent)>budget$max_latent_values)
    .mp_abort('E_DIAGNOSTIC_BUDGET','complete response/latent work exceeds explicit budget before forward allocations')
  invisible(TRUE)
}
