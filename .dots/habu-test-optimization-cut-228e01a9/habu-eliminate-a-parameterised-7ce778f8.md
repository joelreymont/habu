---
title: Eliminate a parameterised sum over an open payload type
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T21:50:03.239125+03:00"
---

Found by lane 537 (dot 8a8912cc; owner 5ae048a7 superseded with no child): inside a package, `ENUM rr 1 ...` then `: K ( rr<a> -- n ) MATCH rr ...` is refused rc 70 with a misleading "undefined word 'rr'", while the same word over `rr<n>` certifies (rc 0) and a concrete eliminator over `NUM:numeric-result<NUM:byte-len>` runs (probes $HOME/.cache/tmp/kestrel-jerry-defectcmt/p2/res1..res4.f). So lib/num-types.f (~:37) and lib/num-types-test.f (~:221) repeat seven-arm MATCHes per role. Acceptance: first, the refusal names the real cause, not an undefined word (seen failing first); then K certifies and runs on two instantiations of different payload width, with the checker's width facts agreeing with the native layout; the repeated MATCHes in num-types.f collapse where the eliminator now serves; comments name no missing dot; baked: rebuild, g1 == g2, two-generation build.
