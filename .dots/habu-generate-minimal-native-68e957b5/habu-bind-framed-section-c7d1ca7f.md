---
title: Bind framed section identity
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-19T23:07:54.158810+02:00\\\"\""
closed-at: "2026-07-20T10:38:00.855215+02:00"
close-reason: "Merged 5968cfe2: framed section bound to exactly one parser section, byte-exact side identity, status/presence matrix, suite registered in gate"
---

tools/lint/diff-frame.f VALIDATE-SECTION validates declared old/new paths only syntactically, then replays raw bytes without comparing them to DIFF:SECTION-OLD$/NEW$ or requiring exactly one parser section. Proven on 04034c04: a raw a.f diff framed with declared.f paths returns catch code 0; two complete modified-file diffs packed into one frame SECTION also return 0; a modified raw body declared as old-absent/new-present also returns 0. This breaks the one-file-per-section identity/count/status contract and lets one framed section stream multiple FILE events under one declared path. During SCAN-RAW capture exactly one section event, compare its old/new presence and exact path bytes with the framed fields, enforce the status/side-presence matrix, and reject zero, duplicate, or mismatched parser sections before publication. Bind status/form/body/mode to that same identity and preserve arbitrary length-bearing path bytes. The M5 contract requires arbitrary POSIX path bytes including tab/LF/CR, but diff-frame-test.f explicitly narrows coverage to line-parser-safe bytes; either make framing deliver the stated byte contract or fail the milestone rather than silently narrowing it. Register the focused frame test in its owning native gate; it currently appears only in FILEMAP and manual runs, so the full gate cannot catch any codec regression. Add the three exact reproducers plus old-only/new-only, add/delete, rename/copy, arbitrary path bytes, duplicate section, cross-swapped path and trailer-count mutations. Verify registered frame, shared parser, both diff consumers, host/filemap/dot and full gates. Files: tools/lint/diff-frame.f/tests and owning suite registration; protocol migration remains with habu-tools-frame-diff-e98f8a6a.

Claim: agent=fdsec workspace=.jj-ws/habu-bind-framed-section-c7d1ca7f
