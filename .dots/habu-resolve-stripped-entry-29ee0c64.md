---
title: Resolve stripped entry names through visible dictionary bindings
status: open
priority: 1
issue-type: bug
created-at: "2026-09-14T12:50:00Z"
---

Confirmed against integration 1948ba71 and /tmp/cedar-data-site-native (SHA-256 515efca813407dc1992614a87e1103f5e07670546f21e30139bb65f39aa3db24). FINDMAIN in src/habu/aot-closure.f scanned dictionary records from index zero and selected the first case-insensitive name match, without checking the record's wordlist. The audited file's SHA-256 was 2901cede85642f020177013fd6c0ac23e1c50a861f7f10702ad06d3335f98f04.

Minimal source (/tmp/cedar-entry-private-before.f):

    package ENTRY-PRIVATE-BEFORE
    private
    : MAIN ( -- ) s" private-entry" type cr ;
    ;package
    : MAIN ( -- ) s" global-entry" type cr ;

The native engine with 1 set-tier, this source and a trailing MAIN prints global-entry and exits 0. From the frozen .jj-ws/cedar-entry-baseline workspace at 1948ba71, the following build succeeds, but its fresh stripped image prints private-entry and exits 0:

    HABU_FIXPOINT_ENGINE=/tmp/cedar-data-site-native /tmp/cedar-data-site-native --load tools/hb-build.f -- /tmp/cedar-entry-private-before.f -o /tmp/cedar-entry-private-before-baseline-image

Build/run logs are /tmp/cedar-entry-private-before-baseline-build.log and /tmp/cedar-entry-private-before-baseline-run.log. The incorrect image SHA-256 is 619b08ba51cb4bab745231c4aa9f50d33384be9cc9c7304381020f5332cd1b94. The engine override pins the native producer, and bin/hb points to the same engine for the separate lint child. Earlier exploratory drivers did not pin that producer; the final evidence above was rerun after verifying both identities.

Responsible layer: AOT root selection. Resolve ENTRY-NAME$ with XREF-FIND, which already implements case-insensitive global and public-qualified lookup and excludes private/retired bindings. Default MAIN must resolve globally; absence is a named entry-not-found refusal. Explicit preseed names obey the same token contract and never guess a binding in another wordlist.

docs/type-families.md describes selecting a matched helper through --preseed-entry NAME. The CLI fixture selects global ALTERNATE. One older layout-fetch fixture defined public AOT-LAYOUT-FETCH-BAD:HLP but selected bare HLP; that accidentally relied on the same cross-wordlist scan. Qualify that selector while preserving its seeded invalid-tag result and existing cache checks. The standalone bare public HLP baseline control built and printed public-helper, confirming this was previous accidental behavior rather than an already-qualified path.

The focused test/stripped-entry.f passes with real optimizing hb-build children and fresh image executions: hostile private/public MAIN before global MAIN; mixed-case explicit global and qualified public HLP despite competing spellings; qualified private SECRET refusal; and missing global MAIN refusal. It isolates HABU_BUILD_CACHE and pins HABU_FIXPOINT_ENGINE to the selected candidate. Log: /tmp/cedar-stripped-entry-check.log, exit 0. No DATA/ADDRESS relocation code is changed. Parent owns independent review and combined gate registration.
