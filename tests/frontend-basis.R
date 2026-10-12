source(file.path("helpers", "native-guard.R"))
source_mode <- nzchar(Sys.getenv("MEASUREMENTPKG_SOURCE"))

if (source_mode) for (f in list.files(file.path(Sys.getenv("MEASUREMENTPKG_SOURCE"), "R"), full.names = TRUE)) source(f) else library(bmeas)

private <- function(n) if (source_mode) get(n, .GlobalEnv) else getFromNamespace(n, "bmeas")

passed <- 0L

ok <- function(x, label) {
    stopifnot(isTRUE(x))
    passed <<- passed + 1L
    cat("PASS", label, "\n")
}

err <- function(x, code) {
    e <- tryCatch({
        force(x)
        NULL
    }, error = identity)
    ok(inherits(e, "measurement_error") && e$code == code, code)
}

d <- data.frame(person = rep(paste0("p", 1:4), 2))

for (j in 1:4) d[[paste0("x", j)]] <- sin((1:8) * j/4) + j

Q <- matrix(c(-1, -1, 1, 1), 4, 1, dimnames = list(paste0("x", 1:4), "linear"))

pp <- set_prior("half_normal(.5)", "amplitude", component = "P")

prepare <- function(m, prior = pp, data = d) bhmm(m, data, subject = "person", prior = prior, fit = FALSE)

form <- bhmf(G = ~x1 + x2 + x3 + x4, P = component(~x1 + x2 + x3 + x4, Q, loading = "shared"), cor = "independent", residual = list(targets = ~x1 + 
    x2 + x3 + x4))

s <- prepare(form)

i <- check_model(s)$identification

ok(i$free_covariance_parameters == 13L && all(i$probe_ranks == 13L), "S07 unique amplitude moment rank13")

ok(nrow(model_table(s, "loadings")) == 8L && nrow(model_table(s, "free_loadings")) == 4L && nrow(model_table(s, "loading_coefficients")) == 
    5L, "derived cells distinct from five coefficients")

ok(sum(get_prior(s)$class == "amplitude") == 1L && sum(get_prior(s)$class == "loading") == 4L, "one amplitude prior and no derived-cell priors")

ok(nrow(model_table(s, "components")) == 2L && all(model_table(s, "loadings")$sign[5:8] == "basis_derived"), "basis signs have no extra anchors")

sd <- standata(s)

ok(sd$B == 1L && sd$L == 4L && sd$A == 1L && sd$K == 2L && all(sd$basis_multiplier[, 2] == Q[, 1]), "unified lowering counts and exact Q")

ok(sum(vapply(s$data_map$blocks, nrow, integer(1))) == 32L, "basis does not duplicate likelihood observations")

missing <- prepare(form, NULL)

ok(check_model(missing)$priors$status == "incomplete" && get_prior(missing)$status[get_prior(missing)$class == "amplitude"] == 
    "missing", "amplitude prior required and inspectable")

err(standata(missing), "E_PRIOR_INCOMPLETE")

err(prepare(form, c(pp, set_prior("normal(0,1)", "loading", component = "P"))), "E_PRIOR_SELECTOR")

err(set_prior("half_normal(1)", "amplitude", target = ~x1), "E_PRIOR_SELECTOR")

err(set_prior("normal(0,1)", "amplitude"), "E_PRIOR")

global <- prepare(form, c(pp, set_prior("normal(0,2)", "loading")))

ok(all(get_prior(global)$prior[get_prior(global)$class == "loading"] == "normal(0,2)") && sum(get_prior(global)$class == 
    "amplitude") == 1L, "ordinary global prior keeps free-role semantics")

no_repeat <- prepare(form, data = d[1:4, ])

ok(check_model(no_repeat)$identification$status == "proven_unidentified", "no-repeat two-noise confounding retained")

shear <- prepare(bhmf(G = ~x1 + x2 + x3 + x4, P = component(~x1 + x2 + x3 + x4, Q, loading = "shared"), residual = list(targets = ~x1 + 
    x2 + x3 + x4)))

ok(check_model(shear)$identification$status == "proven_unidentified" && !is.null(check_model(shear)$identification$basis_nonidentification), 
    "free G-P exact shear proof")

err(stancode(shear), "E_IDENTIFICATION")

q2 <- cbind(linear = Q[, 1], quadratic = c(1, -2, 2, -1))

rownames(q2) <- rownames(Q)

mult <- prepare(bhmf(P = component(~x1 + x2 + x3 + x4, q2, loading = "shared"), cor = list(default = "zero", free = list(~P["linear"] + 
    P["quadratic"]))))

ok(nrow(model_table(mult, "components")) == 2L && sum(model_table(mult, "correlations")$state == "free") == 1L && nrow(model_table(mult, 
    "amplitudes")) == 1L, "expanded coordinates and one h with explicit marginal edge")

ok(all(check_model(mult)$identification$probe_ranks == 6L), "multicolumn actual h/rho/noise map rank6")

clique <- bhmf(G = ~x1 + x2 + x3 + x4, P = component(~x1 + x2 + x3 + x4, q2, loading = "shared"), cor = list(default = "zero", 
    free = list(~P)))

cross <- bhmf(G = ~x1 + x2 + x3 + x4, P = component(~x1 + x2 + x3 + x4, q2, loading = "shared"), cor = list(default = "zero", 
    free = list(~G:P)))

ok(sum(clique$correlations$state == "free") == 1L && sum(cross$correlations$state == "free") == 2L, "group clique and cross have distinct expanded semantics")

err(bhmf(P = component(~x1 + x2 + x3 + x4, q2, loading = "shared"), cor = list(default = "zero", free = list(~P[1] + P["linear"]))), 
    "E_CORRELATION_SYNTAX")

err(bhmf(P = component(~x1 + x2 + x3 + x4, q2, loading = "shared"), cor = list(default = "zero", free = list(~P["linear"]))), 
    "E_CORRELATION_SYNTAX")

err(bhmf(P = component(~x1 + x2 + x3 + x4, q2, loading = "shared"), cor = list(default = "zero", free = list(~P:P["linear"]))), 
    "E_CORRELATION_SYNTAX")

err(bhmf(P = component(~x1 + x2 + x3 + x4, q2, loading = "shared"), cor = list(default = "zero", free = list(~P["missing"] + 
    P["linear"]))), "E_CORRELATION_TARGET")

err(bhmf(P = component(~x1 + x2 + x3 + x4, q2, loading = "shared"), cor = list(default = "zero", free = list(~P), zero = list(~P["linear"] + 
    P["quadratic"]))), "E_CORRELATION_CONFLICT")

qr <- q2[c(3, 1, 4, 2), , drop = FALSE]

captured <- component(~x1 + x2 + x3 + x4, qr, loading = "shared")

qr[] <- 99

reordered <- prepare(bhmf(P = captured, cor = list(default = "zero", free = list(~P))))

ok(identical(standata(mult)$basis_multiplier, standata(reordered)$basis_multiplier), "named rows copied and aligned, source mutation harmless")

reloaded <- unserialize(serialize(mult, NULL))

ok(identical(model_table(mult), model_table(reloaded)), "prepared basis serialization stable")

bad <- captured

bad$basis[1, 1] <- 99

err(bhmf(P = bad), "E_SPEC_CHANGED")

bad <- mult

bad$tables$loading_map$multiplier[1] <- 99

err(model_table(bad), "E_SPEC_CHANGED")

err(component(~x1 + x2 + x3 + x4, unname(Q), loading = "shared"), "E_BASIS_NAMES")

bad <- Q

rownames(bad)[1] <- "other"

err(component(~x1 + x2 + x3 + x4, bad, loading = "shared"), "E_BASIS_ROWS")

bad <- Q

bad[1] <- Inf

err(component(~x1 + x2 + x3 + x4, bad, loading = "shared"), "E_BASIS")

err(component(~x1 + x2 + x3 + x4, Q + (0+0i), loading = "shared"), "E_BASIS")

bad <- Q

class(bad) <- c("method_object", "matrix", "array")

err(component(~x1 + x2 + x3 + x4, bad, loading = "shared"), "E_BASIS")

qm <- matrix(c(1, -1, 0), 3, 1, dimnames = list(paste0("x", 1:3), "q"))

mean_only <- prepare(bhmf(P = component(~x1 + x2 + x3, qm, loading = "shared"), cor = "independent"), data = d[1:4, ])

ok(all(check_model(mean_only)$identification$probe_ranks == 4L) && standata(mean_only)$J == 3L, "mean-only declared row rank4 with original likelihood")

ok(identical(model_table(mean_only, "target_coverage")$mean_only, c(FALSE, FALSE, TRUE)), "declared coverage separated from nonzero support")

zero <- Q

zero[] <- 0

empty <- prepare(bhmf(P = component(~x1 + x2 + x3 + x4, zero, loading = "shared"), cor = "independent"))

ok(check_model(empty)$identification$status == "proven_unidentified" && nrow(model_table(empty, "amplitudes")) == 1L, "effect-free h retained and proven uninformed")

aux <- cbind(Q, unused = 0)

a <- prepare(bhmf(P = component(~x1 + x2 + x3 + x4, aux, loading = "shared"), cor = "independent"))

ok(all(check_model(a)$identification$probe_ranks == 5L) && standata(a)$K == 2L, "unused independent coordinate does not remove identifiable h")

au <- prepare(bhmf(P = component(~x1 + x2 + x3 + x4, aux, loading = "shared")))

ok(check_model(au)$identification$status == "proven_unidentified" && length(check_model(au)$identification$uninformed_correlations) == 
    1L, "invisible free edge is not silently marginalized")

dup <- cbind(first = Q[, 1], second = Q[, 1])

rownames(dup) <- paste0("x", 1:4)

ind <- prepare(bhmf(P = component(~x1 + x2 + x3 + x4, dup, loading = "shared"), cor = "independent", residual = list(targets = ~x1 + 
    x2 + x3 + x4)))

ok(all(check_model(ind)$identification$probe_ranks == 9L), "rank-deficient Q preserved with identifiable unique h")

conf <- prepare(bhmf(P = component(~x1 + x2 + x3 + x4, dup, loading = "shared"), residual = list(targets = ~x1 + x2 + x3 + 
    x4)))

ok(check_model(conf)$identification$status == "possible_nonidentification" && all(check_model(conf)$identification$probe_ranks == 
    9L), "duplicate-column free rho-h confounding flagged")

qi <- diag(3)

dimnames(qi) <- list(paste0("x", 1:3), paste0("q", 1:3))

conf2 <- prepare(bhmf(P = component(~x1 + x2 + x3, qi, loading = "shared"), cor = "independent", residual = list(targets = ~x1 + 
    x2 + x3)))

ok(check_model(conf2)$identification$status == "proven_unidentified" &&
    check_model(conf2)$identification$observable_covariance_moments == 6L &&
    check_model(conf2)$identification$free_covariance_parameters == 7L,
    "full-rank Q cannot separate h from u; fixed-zero cross moments do not add information dimensions")

partial <- prepare(bhmf(G = ~x1 + x2 + x3 + x4, P = component(~x1 + x2 + x3, Q[1:3, , drop = FALSE], loading = "shared"), 
    cor = "independent"))

ok(!any(model_table(partial, "loading_map")$group == "P" & model_table(partial, "loading_map")$target == targets("scores", 
    "mu", "x4")), "partial basis never expands to excluded target")

plain <- bhmm(bhmf(G = ~x1 + x2 + x3), d, subject = "person", fit = FALSE)

ok(standata(plain)$B == 0L && nrow(model_table(plain, "loading_coefficients")) == 3L && sum(get_prior(plain)$class == "loading") == 
    3L, "ordinary-only compiler uses same map with no phantom h")

if (!private(".bh_coefficient_generator_qualified")()) {
    ok(check_model(s)$backend$status == "coefficient_generator_not_runtime_qualified" && check_model(plain)$backend$status == 
        "coefficient_generator_not_runtime_qualified", "changed generator never inherits old execution qualification")
    err(bhmm(s, chains = 1, iter = 20, warmup = 10, execution = "experimental", exploratory = TRUE, resources = "development"), "E_FRONTEND_BACKEND_UNSUPPORTED")
}

cat("Basis checks passed:", passed, "\n")

err(component(~x1+x2+x3+x4,Q),'E_BASIS_LOADING')
cat('Explicit loading-mode total checks:',passed,'\n')

bad<-Q
labels<-rownames(bad);attr(labels,"environment")<-new.env();dimnames(bad)<-list(labels,"linear")
err(component(~x1+x2+x3+x4,bad,loading="shared"),"E_BASIS_NAMES")
bad<-Q;attr(bad,"environment")<-new.env()
err(component(~x1+x2+x3+x4,bad,loading="shared"),"E_BASIS")
cat("Basis capture final checks:",passed,"\n")
local({
  requested<-NULL
  backend<-list(
    summary=function(variables,...) {requested<<-variables;data.frame(variable=c('amplitude[1]','loading[1]','obs_sd[1]'),mean=c(.7,.8,.6),median=c(.7,.8,.6),sd=.1,q5=.4,q95=1,rhat=1.5,ess_bulk=5,ess_tail=5,mcse_mean=.03)},
    diagnostic_summary=function(quiet)list(num_divergent=0L,num_max_treedepth=0L,ebfmi=.5),
    output=function()character(),
    draws=function(variables,format) {
      k<-if(variables=='L_component')2L else 4L
      matrix(as.vector(diag(k)),1,k*k,dimnames=list(NULL,paste0(variables,'[',rep(seq_len(k),k),',',rep(seq_len(k),each=k),']')))
    })
  cache<-private('.bh_cache_scalar_fit')(backend,s,list(posterior_propriety=check_model(s)$posterior_propriety))
  ok('amplitude'%in%requested&&cache$parameter_summary$parameter_key[1]=='amplitude:P','amplitude monitored and keyed once in cached summary')
  ok(cache$parameter_summary$parameter_key[2]==model_table(s,'free_loadings')$parameter[1],'ordinary loading cache retains independent coefficient key')
  fit<-structure(list(spec=s,backend=backend,cache=cache,run=list(chains=1,iter_warmup=10,iter_sampling=10,reduction='serial'),provenance=list()),class='bhmm_fit')
  output<-capture.output(print(summary(fit)));fit$backend<-NULL
  ok(identical(output,capture.output(print(summary(fit))))&&any(grepl('1 shared amplitudes',output,fixed=TRUE)),'group/coordinate/amplitude summary is backend-free and explicit')
})
cat('Basis plus cached-summary checks:',passed,'\n')

annotated<-Q;attr(annotated,"assign")<-1L;attr(annotated,"contrasts")<-list(group="contr.sum");names(dimnames(annotated))<-c("target","coordinate")
ann<-component(~x1+x2+x3+x4,annotated,loading="shared")
ok(identical(ann$basis,Q)&&identical(ann$input_annotations$assign,1L)&&identical(ann$input_axis_names,c("target","coordinate")),"standard inert model-matrix annotations captured without altering Q")
bad<-annotated;attr(bad,"contrasts")<-list(group=new.env())
err(component(~x1+x2+x3+x4,bad,loading="shared"),"E_BASIS")
cat("Basis annotation total checks:",passed,"\n")

local({
  d<-data.frame(person=rep(paste0("p",1:3),each=2),task="T",y=c(.1,.3,.4,.6,.2,.7),id=paste0("o",1:6))
  blocks<-list(score=observe(gaussian_model(),d,"person","task","y",units="points",observation_id="id",source_id="point_source"),
    time=observe(gaussian_model(),transform(d,y=y*1000),"person","task","y",units="milliseconds",observation_id="id",source_id="time_source"))
  Q<-matrix(c(1,0),2,1,dimnames=list(c("score::mu::T","time::mu::T"),"one"))
  prepare<-function(Q)bhmm(bhmf(P=component(~score$mu["T"]+time$mu["T"],Q,loading="shared"),cor="independent"),blocks,subjects="shared",prior=set_prior("half_normal(1)","amplitude"),fit=FALSE)
  s<-prepare(Q)
  ok(model_table(s,"amplitudes")$units=="points"&&model_table(s,"target_coverage")$mean_only[2],"zero row in another unit remains mean-only without a fabricated conversion")
  ok(standata(s)$J==2L&&standata(s)$B==1L&&sum(standata(s)$basis_multiplier!=0)==1L,"nonzero unit map lowers without losing the zero-row likelihood")
  Q[2,1]<-1;err(prepare(Q),"E_BASIS_UNITS")
  Q[]<-0;s0<-prepare(Q)
  ok(model_table(s0,"amplitudes")$units=="undetermined_zero_effect"&&check_model(s0)$identification$status=="proven_unidentified","all-zero amplitude retains undetermined units and exact uninformed status")
})
cat("Basis final zero-row unit checks:",passed,"\n")
