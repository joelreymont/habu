---
title: Fix storage block comment in error map
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T19:29:19.834115+02:00"
---

Full context: lib/errors.f documents the storage parity gate's error block as -6820..-6839 in its own paragraph, but that is the STRUCTURE gate's block; the storage gate's codes are actually -6840..-6859. A copy-paste error in the one place parallel compiler lanes read before minting a code — and two lanes already collided on a block this session. One-line fix, worth recording so it is not lost.
