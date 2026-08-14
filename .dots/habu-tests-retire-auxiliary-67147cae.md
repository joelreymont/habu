
STAKE ADDED (does-conv lane 2026-08-14): this deletion also closes
2 of the last 5 E-NFEED-SCAN census rows (ENUM+/ENUM4+ in
src/core/enums.f are does>-definers). The conversion lane
(a65e56e5) LEFT them to this owner rather than convert-then-delete.
Consumer list the deletion must sweep, measured (the zero-call-
sites claim was FALSE): test/gate-dictionary-lib.f ENUMS case;
tools/lint/def.f rows 33/34 + def-test.f; package-diff-lint
fixtures; create-axiom-test.f prose; 3 rows each in habu2.f and
bootstrap/cg/forth.fs; build-fixpoint / hb-build-lib / boot-pin /
diagnose-hb-core / bootstrap.sh / package-diff-lint-core
exemption; docs/forth.md x2.
