# A creation-time scientific snapshot, not provenance authentication.
# Reuse the bounded canonical plain-data codec; never serialize callbacks.
.bh_structural_evidence_policy <- function() {
    list(max_context_bytes = 67108864, max_context_nodes = 4000000, max_depth = 64)
}

.bh_structural_evidence_node <- function(x, label, nullable = FALSE) {
    if (is.null(x) && nullable) return(x)
    if (typeof(x) != "list" || isS4(x))
        .bh_structural_fail(paste(label, "must be a non-S4 list"))
    fields <- attr(x, "names", exact = TRUE)
    if (typeof(fields) != "character" || !is.null(attributes(fields)) ||
        length(fields) != length(unclass(x)) || length(fields) > 256L ||
        anyNA(fields) || any(!nzchar(fields)) || anyDuplicated(fields))
        .bh_structural_fail(paste(label, "requires bounded distinct field names"))
    x
}

.bh_structural_evidence_get <- function(x, field, label, nullable = FALSE) {
    x <- .bh_structural_evidence_node(x, label)
    if (!field %in% attr(x, "names", exact = TRUE))
        .bh_structural_fail(paste(label, "is missing", field))
    value <- base::.subset2(x, field)
    .bh_structural_evidence_node(value, paste(label, field), nullable)
}

.bh_structural_evidence_replace <- function(x, field, value) {
    if (typeof(x) != "list" || isS4(x))
        .bh_structural_fail("Evidence containers must be ordinary lists")
    saved_attributes <- .bh_portable_codec_attributes(x)
    out <- unclass(x)
    out[[field]] <- value
    saved_attributes$names <- names(out)
    attributes(out) <- saved_attributes
    out
}

.bh_structural_evidence_payload <- function(x) {
    if (typeof(x) != "list" || !identical(attr(x, "class", exact = TRUE), "bmeas_structural_draws"))
        .bh_structural_fail("Expected a structural mapping result")
    x <- .bh_structural_evidence_node(x, "structural result")
    fields <- attr(x, "names", exact = TRUE)
    if (is.null(fields) || anyNA(fields) || anyDuplicated(fields))
        .bh_structural_fail("Structural fields require distinct names")
    payload <- base::.subset(unclass(x), which(fields != "evidence_binding"))
    provenance <- .bh_structural_evidence_get(payload, "provenance", "structural result")
    prior <- .bh_structural_evidence_get(provenance, "prior", "provenance", nullable = TRUE)
    kind <- NULL
    if (!is.null(prior)) {
        origin <- .bh_structural_evidence_get(prior, "provenance", "prior")
        kind <- base::.subset2(origin, "source_kind")
    }
    if (identical(kind, "bmeas_component_prior")) {
        upstream <- .bh_structural_evidence_get(prior, "upstream", "prior")
        spec <- .bh_structural_evidence_get(upstream, "spec", "prior upstream")
        fingerprint <- base::.subset2(spec, "fingerprint")
        if (typeof(fingerprint) != "character" || !is.null(attributes(fingerprint)) ||
            length(fingerprint) != 1L || is.na(fingerprint) || !nzchar(fingerprint))
            .bh_structural_fail("Missing creation-validated prior specification identity")
        # All resolved scientific fields and generation evidence remain above.
        # The old executable spec is retained in x, not traversed by this codec.
        projected_spec <- list(creation_validated_fingerprint = fingerprint,
            scientific_checks = base::.subset2(spec, "checks"),
            excluded_legacy_spec_remainder = TRUE)
        upstream <- .bh_structural_evidence_replace(upstream, "spec", projected_spec)
        prior <- .bh_structural_evidence_replace(prior, "upstream", upstream)
        provenance <- .bh_structural_evidence_replace(base::.subset2(payload, "provenance"), "prior", prior)
        payload <- .bh_structural_evidence_replace(payload, "provenance", provenance)
    }
    payload
}

.bh_structural_evidence_digest <- function(payload) {
    policy <- .bh_structural_evidence_policy()
    locale <- Sys.getlocale("LC_CTYPE")
    # Admission occurs before opening a file or emitting canonical bytes.
    accounting <- .bh_portable_codec_count(payload, policy, locale)
    path <- tempfile("bmeas-structural-evidence-")
    on.exit(unlink(path), add = TRUE)
    connection <- file(path, "wb")
    on.exit(if (isOpen(connection)) close(connection), add = TRUE)
    .bh_portable_codec_write(payload, connection, policy, locale)
    close(connection)
    # Avoid asking isOpen() about an already-closed R connection on exit.
    on.exit(NULL, add = FALSE)
    on.exit(unlink(path), add = TRUE)
    list(digest = unname(tools::md5sum(path)), accounting = accounting, policy = policy)
}

.bh_structural_bind <- function(x) {
    evidence <- .bh_structural_evidence_digest(.bh_structural_evidence_payload(x))
    binding <- c(list(schema_version = "bmeas_structural_scientific_binding_v1"), evidence,
        list(scope = "Accidental mutation detection for complete scientific snapshot; not authentication. Legacy prior spec remainder excluded; creation-validated fingerprint and scientific checks are bound."))
    .bh_structural_evidence_replace(x, "evidence_binding", binding)
}

.bh_structural_binding_check <- function(x) {
    binding <- base::.subset2(x, "evidence_binding")
    if (is.null(binding)) return("blocked_unbound_result")
    .bh_portable_codec_count(binding,
        list(max_context_bytes = 16384, max_context_nodes = 2048, max_depth = 16),
        Sys.getlocale("LC_CTYPE"))
    payload <- .bh_structural_evidence_payload(x)
    # This pass rejects functions, environments, language/S4 and oversized data
    # without invoking their methods or descending into executable captures.
    actual <- .bh_structural_evidence_digest(payload)
    expected <- c(list(schema_version = "bmeas_structural_scientific_binding_v1"), actual,
        list(scope = "Accidental mutation detection for complete scientific snapshot; not authentication. Legacy prior spec remainder excluded; creation-validated fingerprint and scientific checks are bound."))
    if (!identical(binding, expected))
        .bh_structural_fail("Structural scientific snapshot changed after mapping", "E_STRUCTURAL_IDENTITY")
    "bound_scientific_snapshot"
}
