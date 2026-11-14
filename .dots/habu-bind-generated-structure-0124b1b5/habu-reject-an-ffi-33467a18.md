---
title: "Reject an FFI declaration's syntax with a record"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-14T21:19:15.376915+01:00"
closed-at: "2026-11-14T22:24:23.028082+01:00"
close-reason: sznuzuos rejects FFI declaration syntax with placed records
---

Problem: src/lib/ffi-abi.f throws E-FFI-SYNTAX bare at its signature and name readers (e.g. :986-987, :1022, :1069, :1082, :1098-1112), so CALLBACK: CB ( nosuch -- n ) 0 FALLBACK ;CALLBACK exits 'hb: uncaught throw code -3501'; CALLBACK: A:B:C is rejected under the made name 'A:B:C-BODY' (src/lib/ffi-callback.f:367) instead of where NAME$ is read (:253). Acceptance: each such rejection a declaration's text reaches prints a Rust-style record at the token the user wrote, with the fix. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-ffirej.
