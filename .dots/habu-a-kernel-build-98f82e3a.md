---
title: "Owner-record offsets are raw numbers in the kernel, so records keep dead cells"
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T09:02:03.817231+02:00"
---

Problem: the arm64 kernel's record-append routine (src/habu/habu1.f EM-DEF-OCC:LAPPEND, :5609-5610) and its x86-64 mirror (src/habu/kernel-x64.f :3890-3891) write RECORD-PUBLISHED's offset as numbers, so moving an owner-record field makes every new record call the wrong field at generation 1 (SQ fails, generation 2 throws 7121). The checker-handover deletion therefore left zero cells in the owner records ($48, $50, $58, $390 and others) and a $20008 unclaimed gap before DATA-START (src/habu/layout.f) where the deleted DECLARED-LOG band was; docs/bootstrap.md says the link refuses an object whose DATA-START is not hb's. Acceptance: the kernel reads owner-record offsets by name; the owner records hold no dead cell; the gap before DATA-START closes. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-xfer
