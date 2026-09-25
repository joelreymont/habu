---
title: Make TCP shutdown E2E state deterministic
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-25T08:52:48.703909+02:00\\\"\""
closed-at: "2026-09-25T09:28:29.696716+02:00"
close-reason: "Reordered the existing shutdown E2E so receive shutdown occurs before peer FIN, preserving both success assertions and production errno behavior. Original delayed order reproduces ENOTCONN on baseline and candidate; corrected order passes both with the same delay. Independent review approved; integrated 490-suite gate passes. Evidence: ~/.cache/tmp/habu-opt-round2/tcp-shutdown/RESULTS.md."
---

Combined optimization gate on frozen source 382a0b13 and engine SHA256 730f69dac961702ea8593685d5f7641df32cf2d8d20725ba996b38f422e63aae reports tcp4 assertion 50: expected 0 got 57. Evidence: ~/.cache/tmp/habu-opt-round2/combined/gate.log. Investigate the real socket state and macOS shutdown contract before changing code or assertions; compare the accepted baseline. Preserve errno propagation and real loopback transfer/half-close coverage. Acceptance: a root-cause correction, independent review, repeated focused real TCP execution with preserved logs, and the unchanged optimization product passing the complete native registry. No timeout relaxation or blanket error acceptance.

RCA before correction: assertion 50 calls client RECEIVING shutdown after the
peer has already shut down BOTH directions. A diagnostic copy of the complete
existing E2E with a 50 ms delay between those calls fails identically on the
accepted baseline and optimization candidate: only assertion 50, errno 57
(ENOTCONN). Darwin's FIN path marks receive closed; shutdown(SHUT_RD) then
correctly returns ENOTCONN. Test the client's live receive half before sending
the peer's FIN. Keep both exact success assertions and the runtime unchanged.
Failure modes, forced-delay driver and both logs:
`~/.cache/tmp/habu-opt-round2/tcp-shutdown/`.
