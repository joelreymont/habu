---
title: A caught failed load leaves list heads pointing into freed data
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T14:46:03.380434+02:00"
---

Problem: when a load or evaluate text fails and its caller catches, the frame undo (src/habu/habu2.f EM-EVAL-THROW-RECOVER :2233, which lowers DP at :2277) gives the text's data space back but leaves every list head the text stored into older data pointing into it: ERROR:CODE's HEAD (src/lib/error.f :1182, rows :1292-1307) and DECL-REJECT's NEWEST (src/core/generated-declaration.f, batch commit ttwrqznt). Once the space is laid again the next walk reads foreign bytes: with the installed hb, an ERROR:CODE row inside a caught failed include, then -100001 ERROR:TEXT$, ends E-MEMORY-FAULT exit 110. Builds cannot reach it (LINKER refuses a build that gives space back, src/habu/linker.f:44); any program or session that catches a failed load can. Acceptance: after a caught failed load every list it extended walks as it did before the load, with no fix in the lists' walkers; a test loads a failing file under catch, lays data, and looks up an earlier ERROR:CODE text and DECL-REJECT reason. Depends: none. Claim: unassigned.
