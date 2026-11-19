---
title: Hide sets and properties in an OData model
status: active
priority: 2
issue-type: task
created-at: "2026-11-19T17:23:21.693504+01:00"
---

Problem: ODATA:HIDDEN-NAME$ misses two query paths to hidden data. A V4 `$root/<singleton>/<field>` whose singleton's type is hidden is not found: METADATA skips singletons (src/lib/odata/metadata.f METADATA, about :1212) and TALLY-NAMES indexes only sets and members, so QUERY-HIDDEN? never sees one. A set, property or navigation name holding a byte NAME-BYTE? rejects (`X-y`, `To-B`) loads, is hidden by a cascade, and the scan splits it, so a query naming it passes. Acceptance: METADATA records each singleton's type and HIDDEN-NAME$ finds a singleton of a hidden type; METADATA rejects a set, property or navigation name holding a byte NAME-BYTE? rejects, place/expected/found/fix, and the ONE-NAME? checks in HIDE-SET and HIDE-PROPERTY go; the odata suite shows both. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-odhide.
