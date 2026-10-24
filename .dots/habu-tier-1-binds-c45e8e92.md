---
title: Tier 1 binds a layout local after one-cell locals wrongly
status: active
priority: 2
issue-type: task
created-at: "2026-10-24T00:35:39.439934+03:00"
---

A layout local declared after one-cell locals in a {: :} bind is carved at the wrong cells by tier 1. 'STRUCTURE pr FIELD a n FIELD b n ;STRUCTURE  TRUSTED: M5C ( n n n n -- ) {: a z p:pr :} p PR:UNMAKE . . a . ;  TRUSTED: S ( -- ) 1 2 3 4 M5C cr ; S' prints 4 3 1 at tier 0 and 4 1 2 at tier 1. The same family: 'TRUSTED: W2 ( -- pr ) s" x" drop 1 2 5 {: q:n p:pr r :} p ;  : S ( -- ) W2 PR:UNMAKE . . cr ; S' prints 2 1 at tier 0 and garbage at tier 1 (also on master 74c5551b). Found by jerry during the tier-0 rejected-bind lane (dot habu-lower-a-trusted-c16cd810), 2026-10-24. Not yet worked.

Lane: workspace .jj-ws.noindex/jerry-bind (jerry), reproducers ~/.cache/tmp/jerry-repro.ryKf/c45-*.f.
