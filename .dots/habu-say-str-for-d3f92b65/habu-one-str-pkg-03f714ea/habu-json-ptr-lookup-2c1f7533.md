---
title: JSON pointer lookup takes a vector, not a raw scratch
status: closed
priority: 2
issue-type: task
created-at: "2026-11-25T12:34:40.279138+01:00"
closed-at: "2026-11-25T22:49:23.846352+01:00"
---

Problem: src/lib/json/value.f AT (:409) passes WITH-CURSOR (:68) an extra of the pointer's length, so POINTED (:389) and src/lib/json/pointer.f SEEK (:138) take a raw ^u8 scratch to decode pointer tokens into; it is the last WITH-CURSOR call with a non-zero extra (found by the JSON vectors lane). Joel: it is either str or vector. Acceptance: SEEK and POINTED decode into a VEC:vector<u8> their caller owns; WITH-CURSOR's extra and its ^u8 go if nothing else needs them; the json suites pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-jptr.
