\name{ezdm_model}
\alias{ezdm_model}
\title{Pinned revised three-parameter EZ summary observation model}
\usage{ezdm_model(version = "revised_54a07b6_v1")}
\arguments{\item{version}{The sole supported, immutable revised likelihood profile.}}
\description{Creates a registered joint-row adapter for mean RT, unbiased sample RT variance, and upper-boundary count. The diffusion standard deviation is one and the relative starting point is one half. Bounded native density checks have passed; arbitrary hierarchical execution and HMC are not qualified.}
\details{Bind mean_rt in seconds, var_rt in seconds squared, n_upper, and n_trials with observe and units="second". All fields must be observed; means and variances positive; integer n_trials at least three; integer n_upper between zero and n_trials. Parameters drift, bound, ndt default to subject-task predictor roles; bound and ndt have log links and are received once on their natural scales. Explicit scientific priors are required by the existing missing-prior safeguards. The likelihood contract is pinned to bmm commit 54a07b6fa34506bd14116d295eea9db234071f28, but this independently authored implementation contains no upstream source. It does not reproduce the historical released likelihood. No simulator or PPC is registered. See EZDM_REVISED.md for derivation, limitations, and qualification status.}
\value{An observation_model with a registered Stan adapter.}
