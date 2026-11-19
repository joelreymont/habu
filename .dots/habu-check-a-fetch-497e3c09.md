---
title: Reject unknown names after a FUNCTION: group
status: active
priority: 2
issue-type: task
created-at: "2026-11-19T12:06:02.847639+01:00"
---

Problem: src/habu/verify-source.f's dispatcher ran NOMINAL-MARK and RENDERS-MARK? before its FFI-CLOSER? arm took `;FUNCTION`, which defines with define-word and so counted as a renderer: CHECKER-VERIFY-RENDERS set the file-wide marks, and every later unknown type or undefined word was deferred (W-CHECK-DEFERRED) in place of rejected, as in ': S ( ^u8 -- n ) {: buf :} buf 1 GLA {: got:i32 :} buf CELL-VIEW @ ;' after 'PROCESS-SYMBOLS FUNCTION: GLA getloadavg ( ^u8 n -- i32 ) ;FUNCTION'. Acceptance: after a FUNCTION: group an undefined word is E-UNDEFINED and a local typed i32 is E-UNKNOWN-SIGNATURE-TYPE; the group's own word still checks; a test pins each and fails before. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-i32
