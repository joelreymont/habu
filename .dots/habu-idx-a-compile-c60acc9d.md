---
title: "Index a compile's recorded reads"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T18:07:54.613913+01:00"
---

Problem: src/core/compile-reads.f APPEND checks SEEN? by scanning every earlier read, so a compile that records many reads is quadratic: inside a build 8000 one-word evaluates took 2.6 s against 0.35 s in plain hb (W91). The scan dedupes repeated reads; an index needs a hash probe, and src/lib/hashmap.f HM loads after core (boot-files.f BASELIB/RESTLIB rows after CORE). Design (lead): where the one probe lives so core can use it. Acceptance: APPEND is constant time per read on average; the 8000-evaluate build within 2x plain hb. Depends: habu-campaign-c4-diagnostics-3b6de147. Claim: unassigned.
