---
title: Re-key on x86-64 as arm64 does
status: active
priority: 2
issue-type: task
created-at: "2026-11-19T11:08:13.396928+01:00"
---

Problem: src/habu/kernel-x64.f INSERT, (:600-626) writes a record at the first stale slot of its chain even when the record sits further along it, so a re-key (REKEY-LBL :664, called by record-wid! and record-rekey) can give a record a second slot; arm64's LREKEY first walks the stretch to the first empty slot (habu1.f C-HIDX-HELD? :4415) and returns when the record is there. The two machines differ, and INSERT,'s comment says what arm64 does. Acceptance: x86-64's re-key returns without a write when the record's chain holds it before its first empty slot, as arm64's does; INSERT,'s comment says what it does; the x86-64 cross build exits 0 and an x86-64 run of the linker suite passes. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-xk.
