---
title: "Throw E-SPAN-CAPACITY for a short JSON destination"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T17:00:50.984199+01:00"
---

Problem: a string longer than READ-STR's destination throws E-JSON-READ-STATE (src/lib/json/read.f WRITE-BYTE :757 and the copy loop :840), where every other span writer throws E-SPAN-CAPACITY (span.f:8), so a caller cannot tell a capacity shortfall from a token of another kind. Acceptance: those two sites throw E-SPAN-CAPACITY, E-JSON-READ-STATE's text and READ-STR's comment no longer name a short destination, and json-read-test asserts the new code. Depends: none. Claim: unassigned.
