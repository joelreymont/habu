---
title: Make every failure say what failed, where, and with what values
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T13:29:52.309091+02:00"
---

Problem: the census (habu-census-the-failures-e1dcc13c) found 1257 throws guarded by a comparison, 419 of them capacity or limit checks, none of whose reports carries the values compared; the catches between thrower and reporter add a definition name at most (src/compiler/native/definition.f:520-531) or nothing (src/core/include.f:2002-2006); arm64 kernel exits print nothing on fd 2 (seal violations 83/84, task guards 79, die with an empty text and a code over 255 exits 67 and drops the code); about 40 tool and harness lines print a caught code as a number. The null stack's E-HIR-CAP printed only 'hb: uncaught throw code -8283' where 100 rows met a 99-row table. Joel: errors as complete as Rust's. Acceptance: the design under way names what a failure carries from throw to report and how the sites convert; its child dots carry the slices; habu-uncaught-throws-name-1a43b055 is folded into it or ordered beside it. Depends: none. Claim: unassigned.
