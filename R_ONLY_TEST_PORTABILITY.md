# R-only lifecycle portability: 0.0.40.9000 candidate

This isolated candidate layers on frozen 0.0.39.9000. It is not a completed local qualification and does not extend scientific or provider support.

## Why the test changed

The centered final-marginal lifecycle test replaces native compilation and sampling with deterministic R doubles. Most fits use the eager development preset. Its exact-output-budget positive case deliberately uses deferred caching and therefore enters the genuine provider preflight before the compiler double. Previously that one case inspected the host CmdStanR version, coupling an R-only contract test to an optional installed provider.

The test now supplies reviewed-version metadata through a scoped discovery double, restoring the original function on exit. It asserts that the real deferred preflight records that exact metadata and that discovery is restored afterward. Production preflight, strict version/platform rejection, lowering/model gates, output accounting, initialization and fitting dispatch are unchanged. Native traps remain active. Metadata fixtures and synthetic backend output are engineering evidence only, never real provider, sampler, posterior or inferential qualification.

The separate provider suite retains its existing negative cases and explicitly rejects development version 0.9.0.9000. Actual execution compatibility for that installed development version remains unqualified. Installing or downgrading CmdStanR is not part of this patch.

## Imports

NAMESPACE now imports file_test, getFromNamespace, head and tail from utils, and setNames from stats. DESCRIPTION declares these standard R package dependencies. This resolves unqualified symbol ownership explicitly without changing the calling code or numerical algorithms.

## Qualification status

Static source inspection and independent read-only review found no blocking issues in the portability patch. No R parser, focused suite, full check, native compilation or HMC has been run for this candidate in the cloud. The Mac-focused validation plan accompanies the frozen input bundle. Prior .39 results do not qualify .40. A separately selectable local-only integration harness is supplied under tools/ to exercise actual first-party prior generation through analytic mapping to S4a, and actual lavaan mapping through new bindings to S4a. Its successful complete invocation budgets 24 actual prior states and eight lavaan fits, with synthetic retained-posterior fixtures clearly labeled. It is preparation only: those integration gaps remain open until local execution; no formal BF, density MCSE or real posterior qualification is claimed.
