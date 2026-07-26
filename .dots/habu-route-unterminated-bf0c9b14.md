---
title: Route unterminated declarations as packets
status: open
priority: 3
issue-type: task
created-at: "2026-07-26T09:02:20.555235+02:00"
---

Problem: unterminated ENUM and STRUCTURE in the check tool still report through raw CHK-FAIL while SUMTYPE routes through the declaration packet. tools/check-core.f line 932 answers missing ;ENUM with a bare CHK-FAIL string; SUMTYPE has CHK-SUM-DO-NOEND (line 807) driving CHECKER-DEFSUM-NOEND so the reject renders family name, reason, and token through the shared declaration diagnostic. Required result: give unterminated ENUM and STRUCTURE the same NOEND routing through the declaration packet so their diagnostics match the rendered reject surface. Acceptance: fixtures with unterminated ENUM and STRUCTURE report through the declaration packet with family name and token, exit nonzero; the SUMTYPE fixture is unchanged; a mutation restoring the raw CHK-FAIL fails the fixture. Files: tools/check-core.f, the checker NOEND entries if ENUM and STRUCTURE lack them, focused check-tool fixtures. Verify: the check tool suite. Depends: none. Ownership: unterminated-declaration reporting in the check tool only. Claim: unassigned.
