---
title: Force EM-AOTWIDGATE reject for a red-first fixture
status: open
priority: 2
issue-type: task
created-at: "2026-07-10T15:40:28.145223+02:00"
---

EM-AOTWIDGATE (habu2.f, the AOT/snapshot boot-pass protected-WID gate) is now LABELED on fd 2 (LPROTAOT 'hb: AOT protected-WID gate reject') before exit 84, message baked into bin/hb and proven by-construction (identical ADR+write+exit as the firing LPROTPUB publish label and LSRCFULL/BPROTWIDADD). It fires only when a baked AOT call-site (habu2.f:2943 reloc), bootrun entry (:3027), or snapshot rebased call (:5000) resolves into a protected WID -- not reachable from user source. An automated red-first fixture needs a crafted-AOT-image / pwid-variant build harness (bake a call/bootrun into a protected WID, then boot -> forced reject), analogous to the habu-tfam-2b-v-9cbd0019 'pwid-variant, wids 300+70000 baked' harness which is not in the repo. Build that variant-engine builder (a build-fixpoint seam that bakes a protected-WID target into an AOT call/bootrun), forge the boot, and assert rc 84 + CONTAINS 'hb: AOT protected-WID gate reject'. Related: deferred habu-aot-protected-wid-08716547 (batch pwid restore timing) touches the same LAOTPWID/EM-AOT-REGISTER-PROT-WIDS machinery.

## AUDIT (2026-07-11) — STOP: forcing the reject needs an engine seam that does not exist

WHERE THE GATE LIVES (read-only): src/habu/habu2.f, two baked call sites into
LAOTWIDGATE (EM-AOTWIDGATE, :3144) — the AOT reloc gate at habu2.f:2943
(EM-AOT-PATCH-SITES: every relocated call-site NAME is LFIND'd, the found
record's WID checked) and the bootrun gate at :3027 (EM-AOT-BOOTRUN: each
bootrun [len][name] is LFIND'd, WID checked) — LPROTWIDQ true -> write LPROTAOT
("hb: AOT protected-WID gate reject") to fd 2, exit E-SEAL-PACKAGE (84). Both
run INSIDE EM-SEED-AOT AFTER EM-AOT-REGISTER-PROT-WIDS (:3047), so at gate time
the baked pwid registry is already restored — the gate is reachable in the seed
pass, not gated by the batch-timing caveat of dot habu-aot-protected-wid-08716547.
habu2.f is FORBIDDEN this task (wide-ADT stack).

WHY NO TEST-SIDE FIXTURE CAN FORCE IT (three independent blockers):
1. The captured surface is metabuild-OWNED, not source-injectable. ACAP-CAPTURE,
   ACAP-BOOTRUN+, and ACAP-PWID-CAPTURE are called ONLY from src/habu/stdin.f
   CAPTURE-REPL (:88-92), over the FIXED REPL compile span [REPL-R0,REPL-R1) with
   a FIXED bootrun list (INSTALL / BPW-INSTALL / S-INSTALL from repl/debug-watch/
   stepper). No user source, no `--load` file, no hb-build `--preseed-entry`
   (that only selects the AOT *entry* via FINDMAIN/ENTRY-NAME; it never touches
   the pwid registry or the reloc/bootrun target set) can add a protected-WID
   name to the captured records, the pwid buffer, or the bootrun list.
2. A protected-WID target is a public family ctor (e.g. GEMT:ONE) — exactly the
   word class the gate protects — and it cannot ride today's capture: the AOT
   closure walk (aot-closure.f AOT-UNSAFE?) + record capture (aot-capture.f
   ACAP-ADD-REC) were built for plain colon defs; and to be bootrun-resolvable
   its record must be inside the captured span AND its WID inside the pwid buffer.
   Empirically confirmed: preseeding a family ctor as the AOT entry (--preseed-
   entry GEMT:ONE / ONE / one) builds a normal-MAIN-shaped closure that boots
   rc 0 — the ctor never lands as a reloc/bootrun target.
3. To make a NORMAL bootrun target (INSTALL, WID 0/base) trip the gate you would
   have to bake a pwid registry entry for its wordlist WID — but ACAP-PWID-CAPTURE
   reads the LIVE registry, which is populated ONLY by real family declarations
   getting protected ctor-package WIDs; the base wordlist is never a family, so
   its WID is never protected without registry surgery.
   The only existing pwid exercise, aot-capture.f ACAP-PWID-SELFTEST (:364), just
   round-trips the pwid BUFFER (proves 300/1000 survive the u32 field) — it never
   bakes a bootrun/call target into a protected WID, so it never reaches the gate.

REVISED BLOCKER — the exact missing hook:
A build-side "pwid-variant" seam in src/habu/stdin.f + src/habu/aot-capture.f
(both BOOT-PREFIX = byte-fixpoint engine files, adjacent to the forbidden habu2.f
territory) that, under a test-only build flag/env, (a) declares a public family
inside the captured record span so its ctor record is baked, (b) leaves that
family's protected WID in the live registry so ACAP-PWID-CAPTURE bakes it, and
(c) ACAP-BOOTRUN+ s that ctor name (or plants a reloc call to it) so boot LFINDs
a protected-WID target and EM-AOTWIDGATE fires rc 84. The dot's cited "pwid-
variant, wids 300+70000 baked" harness (habu-tfam-2b-v-9cbd0019) is precisely
this and is NOT in the repo. Building it is engine work (stdin.f/aot-capture.f,
byte-fixpoint x2), NOT a test-side fixture, and it lands in the same boot-prefix
engine surface the wide-ADT stack contests. Per the task's STOP clause this is
handed back: schedule it into the engine lane (with, or after, the deferred
habu-aot-protected-wid-08716547 fix, which owns the same LAOTPWID / EM-AOT-
REGISTER-PROT-WIDS machinery), not the tfam tools lane. LPROTAOT stays
proven-by-construction + baked (identical ADR+write+exit as the firing LPROTPUB /
LSRCFULL / BPROTWIDADD labels) until that seam exists.
