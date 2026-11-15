---
title: Print a kernel fault a program causes as a rejection record
status: closed
priority: 2
issue-type: task
created-at: "2026-11-14T20:03:19.423473+01:00"
closed-at: "2026-11-16T04:07:34.922783+01:00"
---

Problem: a kernel exit a program causes (a VM stack overflow, a memory fault, a task's define or cp!, a C callback's bad slot, a sealed vocabulary's policy exit, xt! outside data) prints the one-line `<class> habu: <text> lr <hex>` from src/habu/crash.f LDIE-LINE, not the Rust-style record every other rejection prints. Acceptance: each such site prints the record REJECTION would print for the same parts (code, message, notes `in` naming the word, help), in JSON under --json-errors, with its exit status unchanged; a test compares the kernel record's bytes with REJECTION's; habu-name-the-primitive-ff0806a0 closes with it. Design: /tmp/jerry-c4/design-kfault.md. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
