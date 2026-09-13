---
title: Run pre-trust-defer against a host that reads its prefix from source
status: active
priority: 2
issue-type: task
created-at: "2026-09-12T12:48:02.937972+03:00"
---

The fixture patched a private src/lib tree, then executed a captured product
that never loaded those files. The positive PTDX-POS was undefined and every
intended refusal returned 0. Product F repeats that failure.

2026-09-14 repair: the selected HABU_UNDER_TEST host builds one private cold stdin
engine through the production RUN-PRELUDE, COMMON, seal and driver appenders.
All six cases boot that same engine against the copied tree. No recovery host
or runtime change is needed. The tree, generated source and engine share the
existing cleanup registration; builder refusals also run cleanup.

The actual cold prefix now first rejects undrained CWIN-STATE in checked
CHECKER-CALLS:INSTALL, before its later `is`. With the checker disabled, generated
constructor validation refuses before the final seal. Preserve that rc76 case,
and isolate the production SEAL-CAPTURE directly after the blanked hook in a
matched pair that terminates immediately: drained reaches a unique marker/rc0;
undrained refuses rc73, names TFAM-RESOLVE-XT and never reaches the marker.

Actual F native load of test/pre-trust-defer.f passes: positive checked `is` and
42 dispatch, overflow72, checker70 with CWIN-STATE, constructor76, drained seal0,
undrained seal73. Log: `/home/joel/.cache/cedar-maker-prefix-xr276adl/pre-trust-cold-native.log`.
Focused source is ready for independent review; the registered suite remains
the full-gate acceptance owned by the integration run.
