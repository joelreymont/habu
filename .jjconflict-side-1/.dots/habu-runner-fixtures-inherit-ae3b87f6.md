---
title: Runner fixtures inherit stdin and idle to deadline
status: open
priority: 2
issue-type: task
created-at: "2026-08-21T10:43:30.477562+02:00"
---

From gtrc-1 (2026-08-21, cost an hour of false reds): every bin/hb <script> child spawned by lib/test/runner-test.f inherits the caller's stdin; a fixture with no terminator sits in the REPL until its deadline. Measured: the file runs 2s with stdin at EOF, 35s with five spurious rc-137 timeouts when stdin never EOFs - one command shows both (bash battery.sh < /dev/null vs without). The three pre-existing fixtures (ok.f, fail.f, record.f) carry the exposure; the new one ends with die and does not. Fix: fixtures end with an explicit terminator, or the runner spawns children with stdin at /dev/null (decide which is the contract - a fixture that NEEDS stdin would break under the latter; measure whether any does).
