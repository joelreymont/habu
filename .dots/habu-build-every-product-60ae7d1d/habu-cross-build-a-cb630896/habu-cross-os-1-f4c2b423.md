---
title: "Cross-OS 1: name a build's target on the command line"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T09:45:47.439736+02:00"
closed-at: "2026-11-10T10:29:09.103772+02:00"
close-reason: landed; engines byte-equal with and without --target macos-aarch64 before the signature, refusals rc 64, full suite 270 of 270
---

Step 1 of the cross-OS design (.jj-ws.noindex/carl-layout/.out/design-cross-os-build.md, items 1-3). Problem: tools/hb-build.f cannot name another target; HB-BUILD-CORE's build words pass RTARGET:HOST-TARGET (tools/hb-build-core.f:273-306) and LOAD? picks OS files by the builder's own predicates (:81-90). Acceptance: `--target macos-aarch64|linux-aarch64|linux-x86-64`, one name per target, a triple refused; KERNEL-BUILD, ENGINE, REPL and APP take the target as their last argument; a build without the flag and one with the host's name are byte-equal to before, before the signature. Files: tools/hb-build-core.f, tools/hb-build-lib.f, tools/hb-build.f, tools/build-profile.f, src/compiler/target/model.f, four test callers. Verify: the brief .jj-ws.noindex/carl-layout/.out/briefs/xos-1-target-flag.md. Depends: none. Ownership: those files. Claim: agent=carl workspace=.jj-ws.noindex/carl-xos
