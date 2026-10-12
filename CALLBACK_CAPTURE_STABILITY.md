# New callback capture and source-debug metadata

The .37 candidate fixes an inherited prepared-spec fingerprint instability in R installations using --with-keep.source. Frozen .36 reproduces the failure; a normal .36 installation and a normal pre-fix .37 installation pass the same deterministic fixture. No RNG is involved.

Retained function srcref attributes can reach a srcfilealias/srcfilecopy environment with an unforced lazyLoadDBfetch promise. Resolving an unrelated package namespace function changes serialized lazy-load cache entries. Consequently an unchanged scientific model can acquire different saveRDS bytes and fail its original seal. This is source-debug/lazy-load serialization state, not a change in the measurement law or the prior generator.

## Repair boundary

Only newly captured observation-model functions are normalized. The demonstrated late capture sites also use the same helper: CS informative_rows, and full-M3 informative_rows/validate_data/validate_bindings/methods. No unrelated factories were broadened. The constructor copies each function, removes reserved srcref/wholeSrcref/srcfile attributes from function and syntax nodes and the recognized parser-inserted source-reference slot on nested function definitions, preserves other attributes and literal values, and performs the existing callback bytecode/lifecycle stabilization after normalization. Primitives remain unchanged. Original caller functions and lexical environments are not rewritten, and callbacks are not invoked during capture.

The general utils::removeSource routine was deliberately not adopted wholesale: its recursive treatment of objects inheriting srcref can discard literal data. The package helper removes recognized debug metadata, rather than deleting classed literal values. It preserves pairlist types and meaningful pairlist attributes while traversing nested formal arguments. Nested callbacks, function-valued defaults and omitted-index syntax are checked separately.

The .mp_hash implementation, old-spec validation and historical seals are unchanged. Existing objects are not normalized in place, migrated, reclassified or resealed. A historical mismatched seal remains rejected. Normalization is a new-model construction boundary, not a fallback after validation fails.

## Preserved behavior and limits

Executable language/formal values, lexical environment identity and non-debug attributes are preserved. Ordinary return values, missing/default arguments, dots, calls, parent frames and error calls/messages are tested deterministically. Source-file/line metadata and bytecode representation are not preserved: callbacks depending on debug-source introspection are outside this capture contract. Stack calls remain available, while original source-line navigation may not be. The constructor does not freeze global or external dependencies.

Newly captured method functions now also undergo the existing serialization-lifecycle check previously used for validation/preparation callbacks. A genuinely serialization-unstable closure environment may therefore be rejected earlier, with the existing lifecycle error. It is not silently cloned, replaced or accepted.

All baseline reproductions, source/default/keep-source regression results and independent review are preserved in delivery validation evidence. Current validation status is recorded in the final freeze/check receipts, not inferred from this design note.
