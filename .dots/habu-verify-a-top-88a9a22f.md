---
title: Verify a top-level store of an xt into a typed cell
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T23:42:17.536268+01:00"
---

Problem: tools/check.f --verify-only answers rc 0, with no note, for /tmp/jerry-p1/vprobe/x.f (TYPED-VARIABLE BODY [ n ^u8 n -- n ]; : SEND ( n str -- n ) drop ; ' SEND BODY !), which the run rejects at the `!` with E-MISMATCH, rc 70; src/lib/net/tls.f:162 met it. check.f claims a file verified while top-level code it did not check would be rejected. Acceptance: check.f --verify-only rejects that file as the run does (or names the stretch it leaves to the run); a test holds the case. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-vhunt (hunt).
