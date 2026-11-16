---
title: Resolve a path of any written length
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T06:04:39.253886+01:00"
---

Problem: the resolver bounds the written path by its fixed work buffers (src/core/include.f:197 WORK-BYTES, twice PATH-BYTES): CHECK (:266-269) and JOIN! (:415-424, working directory plus path) reject a path written at 8191 bytes or more, or less under a long working directory, with 'a path resolves to more than the 4095 bytes' even when it resolves within PATH-CAP; rule (a) says only the resolved length is judged. Acceptance: a path of any written length (a ./ chain past 8 KB, a relative path under a deep directory) that resolves within PATH-CAP loads through hb P, hb --load P, include, require and check.f; the work buffers grow to fit through ARENA-ROOM. Depends: none. Claim: unassigned.
