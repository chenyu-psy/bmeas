# Test boundaries

Ordinary push/PR CI builds one source tar and runs `R CMD check --no-manual --no-stop-on-test-error` on that tar. Its 27 default scripts protect public APIs, small deterministic mathematical and policy contracts, and representative detached/CSV engineering integrations. `test-suites.tsv` names every retained or moved script, its purpose, the reason for its placement, and its default counterpart. File timing is evidence about cost, not the reason to remove an assertion.

The 85 original extended scripts remain intact in `r-only/`, with their support fixtures in `support/`. They cover broader model/prior/marker/basis/shared combinations, numerical qualification boundaries, provider formats, optional external resources, and actual lavaan optimizer parity. They are excluded from the installable source tar. No environment variable or ordinary CI event launches them.

After building and installing the exact tar into an isolated library, explicitly run:

```sh
R_LIBS=/absolute/isolated-library Rscript --vanilla validation/run.R extended /absolute/new-log-directory
```

The runner stages all 112 scripts (27 default plus 85 extended), preserving peer filenames and fixture paths. It uses one process at a time and one BLAS/OpenMP thread, retains failures, completion counts, elapsed times, and explicit skip lines. A missing resource branch remains a skip. No failure is converted to success or skipped to make CI pass.

Native smoke/qualification commands in `tools/` require separate deliberate execution, appropriate resource inputs and scientific review. This runner blocks real CmdStan model construction. Neither default checks nor an R-only extended run establishes posterior recovery, usable inference, formal BF or density MCSE. This remains development version 0.0.47.9000; Release engineering checks do not change that evidence boundary.
