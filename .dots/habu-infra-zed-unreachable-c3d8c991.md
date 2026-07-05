---
title: "Infra: zed unreachable + habu-ldmx cleanup"
status: open
priority: 2
issue-type: task
created-at: "2026-07-06T00:26:46.881631+02:00"
---

2026-07-06: ssh zed times out (Orin offline - power/network unknown; JetPack untouched per the deferred decision). When it returns: (1) rm -rf ~/Work/habu-ldmx (isolated-copy leftover from the ldmatrix lane - the lane completed and committed before zed dropped; ~/Work/habu itself was verified untouched by the lane pre-drop); (2) git -C ~/Work/habu pull --ff-only to fable tip; (3) re-run maki/test.f + the device tests from the pushed tree as the standard post-merge verification that was skipped for the ldmx merge (Mac gates were green: 68/68 + full native).
