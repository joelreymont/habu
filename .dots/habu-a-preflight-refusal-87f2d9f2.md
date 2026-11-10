---
title: A preflight refusal names the wrong line when the body holds comments
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T16:39:13.989208+02:00"
---

Problem: the immediate preflight's packet takes its place from the text the engine captured, which drops comments but keeps string payloads, so a body with comments before the immediate is refused at the wrong line, column and bytes. Reproducer (two lines): ': P ( -- ) s" a does> b" 2drop ( does> here ) \ x does>' then 'RAW-IMM ;' after 'TRUSTED: RAW-IMM ( -- ) ; immediate': E-UNMODELED-IMMEDIATE at line 2, column 32, bytes 67-74, while RAW-IMM is at line 3, column 2. Found by the does-clause worker (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/immdoes-stop.md, Other finding). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first: where the preflight's packet maps a captured offset to a source place (src/core/check-hook.f CHECK:IMMEDIATE, src/core/checker.f CHECKER-PREFLIGHT, the engine's capture) and why comments shift it. Acceptance: the packet names RAW-IMM's own line, column and bytes with comments of both kinds before it, under the load and the check. Files: named by the hunt. Verify: the reproducer in test/diag-position-test.f. Depends: none. Ownership: the immediate preflight. Claim: agent=bob workspace=.jj-ws.noindex/habu-a-preflight-refusal-87f2d9f2
