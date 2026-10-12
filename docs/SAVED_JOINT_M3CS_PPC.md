# Saved two-block M3-CS PPC (0.0.34.9000 candidate)

Current 0.0.35.9001 reader supplement: complete existing Gaussian-product capsules and relocated archives now admit metadata planning and explicit population/conditional PPC through a separate R-only reader; see [SAVED_PRODUCT_PPC.md](SAVED_PRODUCT_PPC.md). Product diagnostic refresh remains closed. The 0.0.34.9000 product PPC exclusion below records the earlier boundary; original producer capabilities and no-PPC notices are unchanged. This does not expand M3-CS scope or native/inference qualification.

This increment admits exactly two canonical pinned M3-CS blocks in newly exported
portable fits and their archives. It does not admit mixed families, full routed
M3, EZDM, SDM, generic callbacks, or more than two M3-CS blocks.

## Producer and reader contract

New exports receive `bmeas_joint_m3cs_context_v1` and
`bmeas_joint_m3cs_export_v1`. Admission requires complete consumed block rows,
block-scoped observation identities, declared response/options agreement,
block/task/local-person/crosswalk alignment, a full retained person roster,
and all four row operands. Existing source/context/schema/provenance checks
remain required. Current forward planning additionally validates target roles,
links, geometry, indexing and finite work allowances before source acquisition.

Historical capsules are not reclassified, migrated or resealed. An older
`bmeas_postprocess_context_v1` capsule with two M3 blocks remains readable for
its historical evidence and refuses active PPC. Create a new portable export
from the original intact live fit to use this profile; do not edit the old
capsule's profile or seal. Single-block producer/reader behavior is unchanged.
The Gaussian product profile still refuses PPC and diagnostic refresh.

The new reader receipt is `bmeas_saved_joint_m3cs_reader_v1`. Population and
conditional PPC use exactly one aligned posterior row, one shared full-roster
eta, and one stacked `bmeas_m3cs_rng_v2` response call per replicate. Each response
row receives separate RNG noise. Repeated task names and observation IDs in
different blocks remain distinct block-scoped domains. Complete cross-block
category discrepancies, pair overlap, and all work/source budgets remain in force.

Assessment requires an explicitly supplied compatible simulator. It never
compiles one, replays omitted callbacks, reconstructs a model or invokes HMC.
Direct simulator/source/data APIs remain closed on detached contexts.

Archives require an explicit streaming posterior budget. They reopen and verify
owned members, then pin relocated posterior bytes during acquisition. A current
resource binding belongs to the supplied simulator; historical paths, source
records, protected evidence and original seals remain unchanged. Caller RNG and
thread settings are restored, including partial failures.

## Evidence limit

The added tests use invented CSV states, an inert executable-binding file and
an R-only deterministic controller double. They test routing, alignment,
relocation, immutability, budgets and failure cleanup. Their numerical double is
not the scientific CS logits implementation. No native saved-joint simulation,
HMC, inference, recovery or real-study qualification follows from these tests.
The complete package check is separate and has not been run for this candidate.
