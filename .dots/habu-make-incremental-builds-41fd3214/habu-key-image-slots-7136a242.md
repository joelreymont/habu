---
title: Key image slots by the engine that builds them
status: closed
priority: 2
issue-type: task
created-at: "2026-10-27T18:36:11.639937+02:00"
closed-at: "2026-10-27T19:58:53.370431+02:00"
close-reason: "OUT3 ywrmwosn landed on master d81d39e9; integrated proof: engine, two-generation, bootstrap check rc 0, suite 501/502 (engine-stack-machine red on master, dot ed0ee303)"
---

Lane OUT3, workspace .jj-ws.noindex/carl-out3, brief carl-dots/.out/carl-file/brief-out3.md. Every keyed image slot is <out>/<RUNNING-engine-key>/img/<family>, so one suite run publishes aot-builder and app-builder under two keys (bin/hb, ENGINE-CANDIDATE, HABU_FIXPOINT_ENGINE) into one slot; interleaved publishers can pair one engine's image with the other's key. Fix: slot and records live under the directory of the engine binary that builds them. Also: hb-build-lib.f HBB-INPUTS-READ throws an uncaught E-NUNIT-PROFILE on an empty maker record (suite red in hb-build-fixtures).
