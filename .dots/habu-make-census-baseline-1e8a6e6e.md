---
title: Make census baseline provenance reproducible
status: open
priority: 3
issue-type: task
created-at: "2026-07-26T09:02:20.584529+02:00"
---

Problem: cutover final review finding F5 - the census record verb cannot re-derive the committed baseline on the pre-cutover commit because REQUIRE-CLEAN (tools/enum-census.f line 44) aborts on the eight legitimate second-parser tripwire findings present there, so the provenance of tools/enum-census-baseline.txt is not tool-reproducible. Required result: add a report-without-abort mode to the record verb (findings printed, baseline still written, exit still distinguishes findings-present) or, if that weakens the tripwire unacceptably, a header note in the baseline naming the exact commit and invocation that produced it. Prefer the flag: provenance a tool can re-derive beats provenance a comment asserts. Acceptance: on a tree with tripwire findings the record verb with the flag writes the baseline and reports the findings; without the flag it still aborts; the committed baseline gains its provenance line. Files: tools/enum-census.f, tools/enum-census-baseline.txt header. Verify: the census suite and a record run on a fixture tree with a synthetic tripwire finding. Depends: none. Ownership: census record verb provenance only. Claim: unassigned.
