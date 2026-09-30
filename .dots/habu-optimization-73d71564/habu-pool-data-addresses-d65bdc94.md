---
title: Pool DATA addresses in island records
status: open
priority: 1
issue-type: task
created-at: "2026-09-30T10:58:43.400474+02:00"
blocks:
  - habu-publish-and-use-db11d4c1
---

Campaign: ARM64 code-size fixes, design revision 3 (§3.7). Line references are at master 8c9b75af; re-verify before editing.

Problem: every shared-DATA address is a 12-byte carrier (PUT-DATAADDR, src/compiler/native/emit.f:1523-1529); 14,760 carriers over 4,237 distinct cells in 3 reach islands on the 2026-09-28 engine (habu-measure-live-literal-aa9d119f), about 84 KB above what one 8-byte slot per distinct address per island plus a 4-byte ldr literal per site would cost. ADRP cannot reach DATA-VA (src/os/*/layout.f:9). DATA never moves (habu1.f:2514-2523; EM-MMAP-DATA-REGION is MAP_FIXED), so pooled DATA addresses need no snapshot relocation.

Acceptance: ENC-LDRLIT with golden rows; publish.f keeps an open pool record (sealed system-private, 512 slots, allocated in the code band when first needed; a new one when full or more than 1 MiB behind); PUT-DATAADDR writes `ldr xd,=slot` where a slot is in reach, recorded as an address site of kind LDRLIT (emission.f:151-157), else the carrier; INSNS-OF reflects the chosen form before MEASURE. Capture marks pool records with the spare record byte and canonicalizes their cells like carrier values; EM-SEED-AOT adds the DATA delta to every cell of a pool record; the stripped link keeps a pool record whole when any reachable site names a cell of it, relocates its cells as DATA addresses, remaps ldr-literal sites by OLD>NEW target like b.cond (BTGT19) and rechecks reach; AOT-FILE:VERSION bumps (number assigned at integration) with the old reader refusing; image-size-lib.f reports aot/literal-pool and excludes pool records from the B/BL reach decode; imgdump.f names them; CODE addresses keep their carrier; pool rows invalidate on rewind through the address-cell registrar (habu-idx-the-addr-3c5f6d9b). Fixture: one DATA address used in three tier-1 words — live spans hold one ldr literal each and the pool one slot; saved as an application image, restored twice and run; captured by tools/native-build.f into a booting engine; built stripped with one word unreachable, run, and its --size-report shows the pool class; a payload whose pool record overlaps a code record is refused by name. Artifact: images, size reports, DATA-carrier census row with per-island distinct counts before and after.

Files: src/arch/arm64/asm.f, src/compiler/native/emit.f, emission.f, publish.f, src/habu/aot-capture.f, aot-file.f, aot-lib.f, aot-closure.f, habu2.f (boot rebase), tools/image-size-lib.f, tools/imgdump.f, test/compiler/insn-schema.f, native-emit.f, test/app-image.f, a stripped fixture, test/gate-aot-negative.f, docs/native-applications.md. Engine text: yes; seed mirror: no. Run as worker-max. Coordinate with swift for tools/native-build*.f.

Verify: tools/native-build.f product; bin/hb --load test/compiler/insn-schema.f; bin/hb --load test/compiler/native-emit.f; bin/hb --load test/app-image.f; the stripped fixture; bin/hb --load test/gate-aot-negative.f; census; tools/engine-size.f; tools/two-generation-build.f; bin/hb --load test/run.f; one Gforth recovery check as an audit.

Depends: habu-publish-and-use-db11d4c1 (shared emit.f/publish.f); habu-leave-zero-filled-089e4588 (shared habu2.f and one format-bump window).

Ownership: the files above.

Claim: unassigned.
