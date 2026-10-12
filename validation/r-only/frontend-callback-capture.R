# Deterministic callback capture regressions: source/default/keep-source modes.
# Optional pinned M3 constructors use no simulator, likelihood or RNG.

source(file.path("helpers", "native-guard.R"))

root <- Sys.getenv("MEASUREMENTPKG_SOURCE")

if (nzchar(root)) for (f in list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)) source(f) else library(bmeas)

internal <- function(n) if (nzchar(root)) get(n, .GlobalEnv) else getFromNamespace(n, "bmeas")

factory <- Sys.getenv("MEASUREMENTPKG_CALLBACK_FACTORY")

if (nzchar(factory)) local({
    count <- 0L
    ok <- function(x, label) {
        stopifnot(isTRUE(x))
        count <<- count + 1L
        cat("PASS", label, "\n")
    }
    stopifnot(factory %in% c("m3_cs_model", "m3_model"))
    resource <- Sys.getenv("MEASUREMENTPKG_M3_RESOURCE")
    stopifnot(nzchar(resource), file.exists(resource))
    stopifnot(!exists(".Random.seed", .GlobalEnv, inherits = FALSE))
    make <- get(factory)
    model <- make(resource)
    hash <- internal(".mp_hash")
    callbacks <- if (factory == "m3_cs_model") 
        list(informative_rows = ".mp_m3_cs_informative", validate_data = ".mp_validate_m3_cs")
    else list(informative_rows = ".mp_m3_full_informative", validate_data = ".mp_validate_full_m3", 
        validate_bindings = ".mp_validate_m3_bindings")
    retained <- model[names(callbacks)]
    originals <- lapply(callbacks, internal)
    if (factory == "m3_model") {
        retained <- c(retained, list(simulate = model$methods$simulate, posterior_predict = model$methods$posterior_predict))
        originals <- c(originals, list(simulate = internal("simulate_m3_responses"), posterior_predict = internal("simulate_m3_responses")))
    }
    before <- c(model = hash(model), vapply(retained, hash, character(1)))
    for (name in c(".bh_diagnostic_refresh_validate_spec", ".bh_diagnostic_refresh_compute", 
        ".bh_diagnostic_metric_profile")) invisible(internal(name))
    after <- c(model = hash(model), vapply(retained, hash, character(1)))
    ok(identical(before, after), "fresh pinned M3 constructor and every late callback survive lazy namespace resolution")
    ok(identical(model$reference_parameter_hash, hash(model$parameters)), "reference parameter identity is unchanged")
    for (name in names(retained)) {
        actual <- retained[[name]]
        original <- originals[[name]]
        expected <- internal(".mp_callback_source_copy")(original)
        ok(identical(environment(actual), environment(original)) && identical(formals(actual), 
            formals(expected)) && identical(body(actual), body(expected)) && identical(attributes(actual), 
            attributes(expected)), paste(name, "preserves executable syntax environment and meaningful attributes"))
    }
    snapshots <- lapply(originals, serialize, connection = NULL, version = 2)
    invisible(make(resource))
    ok(identical(snapshots, lapply(originals, serialize, connection = NULL, version = 2)), 
        "M3 capture does not mutate caller namespace callbacks")
    d <- data.frame(row = 1:4)
    d$counts <- matrix(0, 4, 10)
    d$counts[1, 1] <- 1
    d$counts[2, 5] <- 1
    d$counts[4, 1] <- NA
    d$counts[4, 2] <- 2
    expected <- if (factory == "m3_cs_model") 
        c(TRUE, FALSE, FALSE, TRUE)
    else c(TRUE, TRUE, FALSE, TRUE)
    ok(identical(model$informative_rows(d), expected) && identical(model$informative_rows(d), 
        originals$informative_rows(d)), "tiny deterministic row-information semantics are unchanged")
    v <- data.frame(route = if (factory == "m3_cs_model") 
        1L
    else 1:3, content = 1L, .observation_id = if (factory == "m3_cs_model") 
        "one"
    else c("one", "two", "three"))
    v$counts <- matrix(0, nrow(v), 10)
    v$options <- matrix(0, nrow(v), 10)
    active <- list(c(1:4, 10), c(5:7, 9:10), 5:10)
    for (i in seq_len(nrow(v))) {
        cols <- active[[v$route[i]]]
        v$options[i, cols] <- 1
        v$counts[i, cols[1]] <- 1
    }
    ok(isTRUE(model$validate_data(v)) && identical(model$validate_data(v), originals$validate_data(v)), 
        "tiny pinned validator semantics are unchanged")
    if (factory == "m3_model") {
        mapping <- m3_reference_bindings(v, observation_id = ".observation_id")
        ok(isTRUE(model$validate_bindings(v, mapping)) && identical(model$validate_bindings(v, 
            mapping), originals$validate_bindings(v, mapping)), "tiny pinned binding-validator semantics are unchanged")
    }
    ok(identical(before, c(model = hash(model), vapply(retained, hash, character(1)))), "deterministic M3 callback calls leave retained model bytes stable")
    ok(!exists(".Random.seed", .GlobalEnv, inherits = FALSE), "M3 constructors and deterministic checks use no RNG or simulator")
    cat(count, factory, "callback capture checks passed; no likelihood or native execution\n")
}) else local({
    count <- 0L
    ok <- function(x, label) {
        stopifnot(isTRUE(x))
        count <<- count + 1L
        cat("PASS", label, "\n")
    }
    changed <- function(s, label) {
        e <- tryCatch({
            internal(".bh_diagnostic_refresh_validate_spec")(s)
            NULL
        }, error = identity)
        ok(inherits(e, "measurement_error") && identical(e$code, "E_SPEC_CHANGED"), label)
    }
    rng_present <- exists(".Random.seed", .GlobalEnv, inherits = FALSE)
    rng_before <- if (rng_present) 
        get(".Random.seed", .GlobalEnv)
    else NULL
    d <- data.frame(person = as.character(1:8))
    for (j in 1:6) d[[paste0("x", j)]] <- sin((1:8) * j/4) + j/10
    s <- bmeas(bmf(A = ~x1 + x2 + x3, B = ~x4 + x5 + x6), d, subject = "person", fit = FALSE)
    internal(".bh_diagnostic_refresh_validate_spec")(s)
    ok(TRUE, "fresh prepared spec validates in source/default/keep-source modes")
    before <- serialize(s, NULL, version = 2)
    for (name in c(".bh_diagnostic_refresh_plan", ".bh_diagnostic_refresh_compute", ".bh_diagnostic_catalogue")) invisible(internal(name))
    ok(identical(before, serialize(s, NULL, version = 2)), "lazy namespace resolution leaves prepared bytes unchanged")
    internal(".bh_diagnostic_refresh_validate_spec")(s)
    ok(TRUE, "validation stays stable after unrelated namespace resolution")
    m <- gaussian_model()
    hash <- internal(".mp_hash")
    hm <- hash(m)
    invisible(internal(".bh_diagnostic_metric_profile"))
    ok(identical(hm, hash(m)), "observation model hash excludes lazy source-cache state")
    source_refs <- function(x) {
        if (is.function(x)) 
            return(!is.null(attr(x, "srcref", exact = TRUE)) || source_refs(formals(x)) || 
                source_refs(body(x)))
        if (!is.call(x) && !is.pairlist(x) && !is.expression(x)) 
            return(FALSE)
        if (any(c("srcref", "wholeSrcref", "srcfile") %in% names(attributes(x)))) 
            return(TRUE)
        if (is.call(x) && length(x) == 4L && identical(x[[1L]], as.name("function")) && inherits(x[[4L]], 
            "srcref")) 
            return(TRUE)
        any(vapply(seq_along(x), function(i) source_refs(x[[i]]), logical(1)))
    }
    ok(!any(vapply(m$methods, source_refs, logical(1))), "new model methods have no syntax source-debug references")
    original <- internal(".mp_epred_gaussian")
    original_bytes <- serialize(original, NULL, version = 2)
    new_model <- gaussian_model()
    ok(identical(original_bytes, serialize(original, NULL, version = 2)), "model capture leaves namespace callback bytes unchanged")
    ok(identical(environment(original), environment(new_model$methods$posterior_epred)), 
        "namespace callback environment is retained")
    e <- new.env(parent = baseenv())
    e$offset <- 3
    callback <- eval(parse(text = "function(x, multiplier = 2, ...) { if (x < 0) stop(\"callback failed\"); list(value = x * multiplier + offset, call = sys.call(), parent = parent.frame(), omitted = missing(multiplier), dots = list(...)) }", 
        keep.source = TRUE), e)
    attr(callback, "contract") <- list(role = "deterministic fixture", version = 7L)
    attr(body(callback), "semantic_tag") <- "preserved body metadata"
    old_body <- body(callback)
    old_formals <- formals(callback)
    old_attributes <- attributes(callback)
    old_bytes <- serialize(callback, NULL, version = 2)
    captured <- internal(".mp_stable_callback")(callback)
    ok(identical(old_bytes, serialize(callback, NULL, version = 2)) && identical(old_body, 
        body(callback), ignore.srcref = FALSE) && identical(old_formals, formals(callback), 
        ignore.srcref = FALSE) && identical(old_attributes, attributes(callback), ignore.srcref = FALSE), 
        "custom caller function including debug references is not mutated")
    ok(identical(environment(captured), e), "custom lexical environment is preserved by reference")
    ok(identical(formals(captured), formals(callback)) && identical(body(captured), body(internal(".mp_callback_source_copy")(callback))) && 
        isTRUE(all.equal(body(captured), body(callback))), "executable body and formal values are preserved")
    ok(identical(attr(captured, "contract"), attr(callback, "contract")) && identical(attr(body(captured), 
        "semantic_tag"), attr(body(callback), "semantic_tag")), "meaningful function and syntax attributes are retained")
    ok(!source_refs(captured), "captured custom syntax debug attributes are removed")
    hc <- hash(captured)
    invoke <- function(fn) {
        value <- fn(2, tag = "kept")
        stopifnot(identical(value$parent, environment()))
        value$parent <- NULL
        value
    }
    ok(identical(invoke(callback), invoke(captured)), "deterministic values defaults dots sys.call and parent.frame semantics are preserved")
    ok(identical(hc, hash(captured)), "first deterministic invocation does not alter captured bytecode hash")
    fail <- function(fn) tryCatch(fn(-1), error = identity)
    a <- fail(callback)
    b <- fail(captured)
    ok(identical(conditionMessage(a), conditionMessage(b)) && identical(conditionCall(a), 
        conditionCall(b)), "ordinary error message and call remain useful without source line metadata")
    literal <- structure(1:8, class = "srcref", contract = "literal data")
    f <- function(x) x
    environment(f) <- baseenv()
    formals(f) <- as.pairlist(list(x = literal))
    preserved <- internal(".mp_stable_callback")(f)
    ok(identical(formals(preserved)$x, literal), "literal srcref-class default is preserved as data")
    missing_nodes <- eval(parse(text = "function(x, ...) x[, 1, drop = FALSE]", keep.source = TRUE), 
        baseenv())
    missing_copy <- internal(".mp_stable_callback")(missing_nodes)
    grid <- matrix(1:6, nrow = 3)
    ok(identical(formals(missing_copy), formals(missing_nodes)) && identical(missing_copy(grid), 
        missing_nodes(grid)), "missing formal defaults ellipsis and omitted matrix indexes are preserved")
    nested <- eval(parse(text = "function(x) { f <- function(y = 1, ...) { x + y }; f() }", 
        keep.source = TRUE), baseenv())
    nested_before <- serialize(nested, NULL, version = 2)
    nested_copy <- internal(".mp_stable_callback")(nested)
    nested_hash <- hash(nested_copy)
    ok(!source_refs(nested_copy) && identical(nested_before, serialize(nested, NULL, version = 2)), 
        "nested function-definition source slot is removed only from the captured copy")
    ok(identical(nested_copy(2), nested(2)) && identical(nested_hash, hash(nested_copy)), 
        "nested closures preserve execution and stable first-use representation")
    for (text in c("function(x) lapply(x, function(y) y + 1)", "function(x, f = function(y = 1) y + 1) f(x)")) {
        fn <- eval(parse(text = text, keep.source = TRUE), baseenv())
        fn_before <- serialize(fn, NULL, version = 2)
        fn_copy <- internal(".mp_stable_callback")(fn)
        fn_hash <- hash(fn_copy)
        ok(!source_refs(fn_copy) && identical(fn_before, serialize(fn, NULL, version = 2)) && 
            identical(fn_copy(2), fn(2)) && identical(fn_hash, hash(fn_copy)), "anonymous body/default function source slots preserve values and caller state")
    }
    tagged <- eval(parse(text = "function(x, f = function(y = 1) y + 1) f(x)", keep.source = TRUE), 
        baseenv())
    tagged_args <- formals(tagged)
    inner <- tagged_args$f
    attr(inner[[2L]], "contract") <- list(kind = "nested formals")
    tagged_args$f <- inner
    attr(tagged_args, "contract") <- "outer formals"
    formals(tagged) <- tagged_args
    tagged_copy <- internal(".mp_stable_callback")(tagged)
    ok(is.pairlist(formals(tagged_copy)$f[[2L]]) && identical(attr(formals(tagged_copy), 
        "contract"), attr(formals(tagged), "contract")) && identical(attr(formals(tagged_copy)$f[[2L]], 
        "contract"), attr(formals(tagged)$f[[2L]], "contract")) && identical(tagged_copy(2), 
        tagged(2)), "outer and nested formal pairlist types and meaningful attributes are retained")
    ok(identical(internal(".mp_stable_callback")(`+`), `+`), "primitive callbacks remain unchanged")
    guard_env <- new.env(parent = baseenv())
    guard_env$calls <- 0L
    guard <- eval(parse(text = "function(...) { calls <<- calls + 1L; stop(\"callback invoked during capture\") }", 
        keep.source = TRUE), guard_env)
    template <- gaussian_model()
    guarded <- new_observation_model("guarded", "1", template$data_schema, template$parameters, 
        template$likelihood, capabilities = template$capabilities, validate_data = guard, 
        prepare_data = guard, methods = list(simulate = guard, posterior_predict = guard, 
            posterior_epred = guard))
    ok(identical(guard_env$calls, 0L), "constructor invokes no validation preparation or model callback")
    ok(all(vapply(c(guarded$methods, list(guarded$validate_data, guarded$prepare_data)), 
        function(f) identical(environment(f), guard_env), logical(1))), "every callback capture preserves its lexical environment")
    bad <- s
    body(bad$models$scores$model$methods$posterior_epred) <- quote(eta + 1)
    changed(bad, "changed callback executable body remains rejected")
    bad <- s
    environment(bad$models$scores$model$methods$posterior_epred) <- new.env(parent = baseenv())
    changed(bad, "replaced callback lexical environment remains rejected")
    bad <- s
    bad$data_map$blocks$scores$y[1] <- bad$data_map$blocks$scores$y[1] + 1
    changed(bad, "changed prepared data remains rejected")
    bad <- s
    bad$tables$loadings$target[1] <- "changed"
    changed(bad, "changed prepared table remains rejected")
    env <- new.env(parent = baseenv())
    env$offset <- 0
    method <- eval(quote(function(eta, observation_sd = NULL, trials = NULL) eta + offset), 
        env)
    custom <- new_observation_model("gaussian", "1.1.0", template$data_schema, template$parameters, 
        template$likelihood, capabilities = template$capabilities, methods = list(simulate = template$methods$simulate, 
            posterior_predict = template$methods$posterior_predict, posterior_epred = method))
    custom$backend <- "gaussian_cfa"
    long <- data.frame(person = rep(d$person, 6L), task = rep(paste0("x", 1:6), each = nrow(d)), 
        y = unlist(d[-1], use.names = FALSE))
    block <- observe(custom, long, "person", "task", "y", units = "unscaled_response")
    bound <- bmeas(bmf(A = ~scores$mu[c("x1", "x2", "x3")], B = ~scores$mu[c("x4", "x5", 
        "x6")]), list(scores = block), fit = FALSE)
    internal(".bh_diagnostic_refresh_validate_spec")(bound)
    env$offset <- 1
    changed(bound, "mutation of a retained local callback binding remains rejected")
    bad <- s
    bad$fingerprint <- paste0("old-", s$fingerprint)
    invisible(gaussian_model())
    changed(bad, "old mismatched seal is neither rewritten nor accepted")
    ok(identical(s$fingerprint, unserialize(before)$fingerprint), "existing prepared fingerprint remains unchanged")
    ok(identical(rng_present, exists(".Random.seed", .GlobalEnv, inherits = FALSE)) && (!rng_present || 
        identical(rng_before, get(".Random.seed", .GlobalEnv))), "all checks preserve RNG existence and state without seeding or draws")
    cat(count, "callback capture checks passed\n")
})

