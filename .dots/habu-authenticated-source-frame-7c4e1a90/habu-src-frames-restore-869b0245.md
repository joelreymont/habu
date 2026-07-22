---
title: "Source frames: restore nested parents"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T12:22:25.386746+02:00"
blocks:
  - habu-src-frames-bind-cbd0b7c4
---

Problem: nested include or evaluate can overwrite ambient parser state and cannot prove exact parent restoration. Acceptance: add the sole bounded push/pop stack over authenticated frames. Push saves the complete active parent input state before consuming and activating a live child; normal child EOF pops and releases exactly one child, then restores the parent frame, cursor, limit, and parser state byte-for-byte. Push preflights every refusal; a later injected failure restores the parent and releases the consumed child exactly once. Reject depth overflow, underflow, wrong-parent, double-pop, stale generation, and restore mismatch before mutation. This leaf does not resolve files, capture persistent origins, or own exceptional rollback. Files: nested frame stack and focused nesting tests. Verify: nested, repeated, empty-child, no-final-newline, maximum-depth, push-failure, child-comment, and parent-byte-consumption fixtures. Depends: Source frames: bind parser bounds. Ownership: normal push, EOF pop, child release, and exact parent restoration only. Claim: unassigned.
