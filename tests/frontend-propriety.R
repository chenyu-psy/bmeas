source(file.path("helpers", "native-guard.R"))
source_mode <- nzchar(Sys.getenv("MEASUREMENTPKG_SOURCE"))

if (source_mode) for (f in list.files(file.path(Sys.getenv("MEASUREMENTPKG_SOURCE"), "R"), full.names = TRUE)) source(f) else library(bmeas)

passed <- 0L

ok <- function(x, label) {
    stopifnot(isTRUE(x))
    passed <<- passed + 1L
    cat("PASS", label, "\n")
}

err <- function(x, code) {
    e <- tryCatch({
        force(x)
    }, error = identity)
    ok(inherits(e, "measurement_error") && e$code == code, code)
}

d <- data.frame(person = paste0("p", 1:3), x1 = rep(2, 3), x2 = c(-1, 0, 1), x3 = c(1, -1, 0))

make <- function(data = d, form = bhmf(G = ~x1 + x2 + x3), prior = NULL) bhmm(form, data, subject = "person", prior = prior, 
    fit = FALSE)

s <- make()

checks <- check_model(s)

ok(checks$identification$status == "local_rank_full_not_global_proof" && checks$posterior_propriety$status == "proven_improper", 
    "structural rank and data posterior propriety are distinct")

f <- checks$posterior_propriety$findings

ok(nrow(f) == 1L && f$observations == 3L && f$independently_shrinking_coefficients == 1L, "constant single-loading target sufficient shell boundary")

err(bhmm(s, chains = 1, iter = 20, warmup = 10, exploratory = TRUE, execution = "experimental", resources = "development"), "E_POSTERIOR_IMPROPER")

ok(is.character(stancode(s)) && identical(s$data_map$blocks$scores$y[s$data_map$blocks$scores$.task == "x1"], rep(2, 3)), 
    "model/data inspection retained without jitter")

p <- c(set_prior("normal(0,2)", "Intercept"), set_prior("normal(0,1)", "loading"), set_prior("half_normal(1)", "sigma"))

ok(check_model(make(prior = p))$posterior_propriety$status == "proven_improper", "explicit locally positive proper priors do not fix collapse")

d2 <- d

d2$x1[3] <- 2 + 1e-10

ok(check_model(make(d2))$posterior_propriety$status == "no_scoped_collapse_detected", "near-constant values are not rounded to constant")

f2 <- bhmf(G = ~x1 + x2 + x3, H = ~x1 + x2 + x3, cor = "independent")

ok(check_model(make(form = f2))$posterior_propriety$status == "no_scoped_collapse_detected", "two independent coefficients change sufficient exponent; no global propriety assertion")

d4 <- rbind(d, transform(d[3, ], person = "p4"))

ok(check_model(make(d4, f2))$posterior_propriety$findings$independently_shrinking_coefficients[1] == 2L, "M4 reaches two-coefficient sufficient boundary")

qu <- matrix(c(1, -1, 0), 3, 1, dimnames = list(paste0("x", 1:3), "q"))

md <- d

md$x1 <- c(-1, 0, 1)

md$x3 <- 4

mo <- make(md, bhmf(P = component(~x1 + x2 + x3, qu, loading = "shared"), cor = "independent"), set_prior("half_normal(1)", 
    "amplitude"))

ok(check_model(mo)$posterior_propriety$findings$independently_shrinking_coefficients[1] == 0L, "mean-only constant target needs zero incoming coefficients")

repd <- data.frame(person = c("p1", "p1", "p2", "p3"), x1 = c(-1, -1, 0, 1), x2 = c(0, 1, -1, 2), x3 = c(1, -1, 0, 2))

r <- make(repd)

f <- check_model(r)$posterior_propriety$findings

ok(!f$constant_target && f$all_within_person_values_equal && grepl("repeat_sigma", f$sufficient_paths), "varying target with exact within-person repeated values has separate collapse")

repd2 <- repd

repd2$x1[2] <- -1 + 1e-10

ok(check_model(make(repd2))$posterior_propriety$status == "no_scoped_collapse_detected", "one genuinely varying repeated group blocks this exact-repeat proof")

Q <- cbind(q1 = c(1, -1, 1), q2 = c(2, 1, -1))

rownames(Q) <- paste0("x", 1:3)

b <- make(d, bhmf(P = component(~x1 + x2 + x3, Q, loading = "shared"), cor = "independent"), set_prior("half_normal(1)", 
    "amplitude"))

ok(check_model(b)$posterior_propriety$findings$independently_shrinking_coefficients[1] == 1L, "shared basis amplitude counted once across columns")

u <- make(d, bhmf(G = ~x1 + x2 + x3, residual = list(targets = ~x1 + x2 + x3)))

ok(check_model(u)$posterior_propriety$status == "no_scoped_collapse_detected", "extra parameter SD adds independent shrink coordinate")

cat("Gaussian data-propriety checks passed:", passed, "\n")


extreme<-d;extreme$x2<-c(-8e307,0,8e307)
sx<-make(extreme);cx<-check_model(sx)
ok(cx$priors$status=="invalid"&&cx$posterior_propriety$status=="not_assessed","invalid overflowed calibration cannot establish propriety premises")
ok(any(get_prior(sx)$status=="invalid")&&any(grepl("Inf",get_prior(sx)$prior)),"uncapped invalid prior value and validation error remain inspectable")
err(standata(sx),"E_PRIOR_INVALID")
cat("Propriety and finite-prior checks:",passed,"\n")
