# bmeas

Bayesian hierarchical measurement modeling in R.

Package development currently takes place on
[`work/current-0.0.47.9000`](https://github.com/chenyu-psy/bmeas/tree/work/current-0.0.47.9000).
This branch contains an experimental snapshot; no stable GitHub Release or CRAN
release is available. Main and develop currently contain repository configuration
only. Package source enters them only through explicitly approved pull requests.

## Development workflow

Feature and fix branches target develop. Only a same-repository develop pull
request may target main. Both branches require a pull request and successful
branch-policy, R-CMD-check and release-metadata checks. Force pushes and deletion
are blocked; there are no permanent bypass actors or required second-person
approvals. The maintainer approves each pull request before it is created.

Configuration-only initialization runs automation boundary tests; it does not
claim a package check. When package source is present, CI builds the installable
R source tar and checks that exact tar, preserving tests and reporting optional
skips. Native construction guards prohibit real CmdStan model construction.

## Future releases

A merged develop-to-main pull request is the only publication trigger. A direct
push or manual verification never publishes. The workflow checks and builds the
exact merge SHA before creating a DESCRIPTION-version tag and GitHub Release.
Development versions with a fourth component >= 9000 are prereleases. Existing
conflicting tags, release metadata or assets cause failure; assets are never
clobbered. Only the publication job has write access through GITHUB_TOKEN.
No PAT or CRAN submission is configured.

An authorized maintainer can run the release workflow's manual verification with
an exact SHA to check/build a package without publishing. A configuration-only
SHA has no package and cannot publish. The first release remains subject to
explicit approval of the relevant develop-to-main pull request.

## Package website

The [package homepage](https://chenyu-psy.github.io/bmeas/) is deployed by
GitHub Pages after main updates. Until package source reaches main, it explicitly
shows that the stable release and API reference are unavailable. Once source is
integrated through an approved PR, pkgdown builds the API from man/*.Rd into
_site, preserving the hand-written docs directory. Website builds do not run
examples, compile CmdStan models or sample.
