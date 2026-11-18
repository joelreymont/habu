---
title: Make the type-family registry private
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T16:25:27.290940+01:00"
closed-at: "2026-11-18T16:58:11.470900+01:00"
---

Problem: src/core/type-family.f package TFAM keeps 229 words public and four global (TFAM-NAME$, TFAM-CTOR-WORD?, TFL-CON-FAM?, TFL-MATCH-FAM?), mutators included, so a program changes the type registry: `: P ( -- ) 0 5 TFAM:TFAM-KIND! ;` passes tools/check.f -- --verify-only, rc 0. No Zen, Tender or Maki file, library file or tool names a TFAM word other than its error codes. src/habu/linker.f TYPE-SOURCE still falls back to TFAM's public words for TYPE-WORD. Acceptance: every TFAM word but its error codes (E-*) is E-UNDEFINED outside TFAM, globals included; engine callers take them by EXPORT; TYPE-SOURCE looks only in TFAM's private words. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-ta.
