---
title: Rewrite INTEL.md for the campaign
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.406681+03:00"
---

Problem: `INTEL.md` describes the superseded hazel/neubau/cedar integration (bookmarks, landing procedure, the Intel-machine setup list, the earlier integration table, the Landed narrative, the experiment reconciliation table) instead of this campaign.
Acceptance: `INTEL.md` states: the mission (one paragraph: tier-1-only x86 engine; Habu interpreter captured into both products and entered through `ENGINE-MAIN:XT-CELL`; dual-emission cross-build from spark; write-time link with fixed segments; self-host fixpoint on the ThinkPad); the current state (landed: assembler, prims table + parity gate, seam + ELF64 + MOVABS site, register file, registry, x64ir/select/partial emit/passes; executed: `hb-x64-peer` 0 and `hb-x64-peer-negative` 21 natively on the ThinkPad; spark baseline 488/491 with the two libzip rows as host setup and `fs-mutate` as R1); the hosts (ThinkPad x86-64 Arch = lead, cross-built images until X6 then native; spark aarch64 Ubuntu 24.04, 20 cores, at `~/Work/habu/krait`; no CI; macOS untested by us, Alder pulls master and fixes macOS; shared-file landings keep macOS arms correct by construction and report macOS untested); the gate commands per host; ownership (every x86 dot is this lane's; the `alder` claim on habu-cross-build-the-d25a959d re-claimed; habu-make-build-fixpoint-eeaf6c00 stays Alder's); the dot tree entry point habu-finish-the-linux-57cb3952 and the workflow rules from `CLAUDE.md`; the design facts that must not drift (registers, MOVABS, guard model, recovery = cross-build) plus the new invariants (no cold route on x86; every code-bearing record comes from the compiler; the capture reads recorded sites; the region and DATA are fixed `PT_LOAD`s on x86; `MAIN` is a DATA cell the kernel calls). Deleted: the hazel/neubau/cedar ownership and landing procedure (`hazel/integration`, `intel/<dot>` bookmarks, ssh both ways, the `qemu-user` remark), the Intel-machine setup list, the "Earlier integration state" table and the "Landed" narrative (surviving facts move to `docs/x86-64.md`), the experiment-reconciliation table (one sentence: superseded and archived). First commit of the campaign; rewritten again at G3. habu-point-restart-md-3e7996ca rewrites `RESTART.md`.
Files: `INTEL.md`, `docs/x86-64.md` (facts moved out of `INTEL.md`).
Verify: read-through against this list; every dot id and path named resolves.
Depends: none.
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.
Correction: the landing procedure INTEL.md describes is the current one. Shared-file leaves go to Alder as `intel/<dot-id>` bookmarks on GitHub origin, listed in the lane's receipt; that replaces the old hazel/neubau procedure the Acceptance deletes.
