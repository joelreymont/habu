---
title: "Add CMD:RUN, which throws on a failed run"
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T20:35:51.035951+02:00"
---

Joel 2026-10-28 (interview): public CMD:RUN ( CMD:command ptr u8 n ms n -- CMD:command ) runs to the deadline, returns on rc 0, otherwise writes CMD:ERR$ to stderr and throws the given code (timeout: E-PROC-TIMEOUT via PROC-OUTCOME>RC). The private RUN in src/lib/process-command.f takes a private name that says what it does. Replaces Maki's ENDED/RUN-ENDED/RUN-CHILD. Claim: tim (hands to carl).
