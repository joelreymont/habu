---
title: Eliminate a parameterised sum over an open payload type
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T21:50:03.239125+03:00"
---

Found by lane 537 (dot 8a8912cc; owner 5ae048a7 superseded with no child): inside a package, `ENUM rr 1 ...` then `: K ( rr<a> -- n ) MATCH rr ...` is refused rc 70 with a misleading "undefined word 'rr'", while the same word over `rr<n>` certifies (rc 0) and a concrete eliminator over `NUM:numeric-result<NUM:byte-len>` runs. On the installed hb `package RRT ENUM rr<a> VARIANT ok FIELD v a ;VARIANT VARIANT bad ;VARIANT ;ENUM : K ( rr<a> -- n ) MATCH rr ok OF drop 0 ENDOF bad OF 1 ENDOF ;MATCH ; ;package` is refused rc 70 with E-MATCH-OPEN-ARGS 'bad match: scrutinee type arguments are unresolved' at 'rr' under --load, tools/check.f and --verify-only, and the same file over `rr<n>` certifies (rc 0). So src/lib/num-types-test.f (fourteen `*-CODE` words from :18) repeats seven-arm MATCHes per role, as src/lib/num-types.f:34-36 says. Acceptance: K certifies and runs on two instantiations of different payload width, with the checker's width facts agreeing with the native layout; the repeated MATCHes in num-types-test.f collapse where the eliminator now serves; comments name no missing dot; baked: rebuild, g1 == g2, two-generation build.
