---
title: Metacompiler baked-data capability for boot-time pin
status: open
priority: 2
issue-type: task
created-at: "2026-07-07T09:30:00+02:00"
---

Capability gap found while closing dot habu-boot-pin-bake: bin/hb bakes ONLY
primitives into its boot dictionary (the #PL registry; stdin.f: EMIT-DICT bakes
the emit-builder #PL list, NOT the host dictionary). There is no way for the
build to bake a computed VALUE (bytes/constant) plus a word that reads it such
that both survive into the installed binary and are name-resolvable at boot
BEFORE the re-read boot prefix is parsed. The AOT REPL seed (EM-SEED-AOT /
ACAP-BOOTRUN+) is the closest mechanism but its boot-run list only arms on the
interactive REPL entry (AOT-SEED-ARM-CELL, habu2.f), not on --load/pipe runs.

Needed: a metacompiler capability to bake build-computed data — e.g. a
`BAKED-BYTES` emitter that registers a primitive-like dictionary record whose
body is a data blob captured at build time, resolvable by name at boot on ALL
entry paths.

First consumer: autonomous boot-time verification of the boot-prefix pin
(tools/boot-pin.f computes the digest today; the build would bake it via this
capability, and an emitted boot check would verify the on-disk prefix against
it). Design constraints proven in the habu-boot-pin-bake rescope:

- Fail policy MUST default off/warn with strict opt-in (HABU_BOOT_PIN=strict):
  a fail-closed-by-default boot check deadlocks self-hosting because the
  rebuild command boots bin/hb over the drifted prefix before it can rebuild.
- The verifier itself must be baked (primitive/AOT-record), never part of the
  re-read prefix, or it cannot tamper-proof the files it rides in (an injected
  VERIFY-BOOT-PIN source token died E-UNDEFINED on every boot and bricked
  self-rebuild).
- Hash scheme already settled and regression-covered: manifest SHA-256 over
  ordered per-file SHA-256s in PFX-LOAD-BASE-FILES + script-argv order
  (tools/boot-pin.f BP-HASH; cross-checked against habu2.f by
  test/boot-pin-test.f). Per-boot cost ~1-2ms for the 19-file prefix.
