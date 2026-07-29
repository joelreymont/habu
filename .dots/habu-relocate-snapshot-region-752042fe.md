---
title: Relocate snapshot region-to-text calls
status: active
priority: 2
issue-type: task
created-at: "2026-07-29T19:58:29.544526+02:00"
---

Full context: PRIORITY 1 design regression; blocks habu-fix-owner-wid-e2bc360c and every master fast-forward. SNAP format v4 moved the live JIT region from the fixed RBASE-VA to text plus REGION-OFF, but restored region code still contains direct BL instructions into engine text and NOTHING relocates them: EM-SNAPSHOT-REBASE-DICT (src/habu/habu2.f:4069-4086) rewrites only dictionary record fields 0 and 24. The displacement is not a run invariant, because align64k(content_base + REGION-OFF) in EM-MMAP-CODE-REGION (src/habu/habu2.f:3513) rounds to 64 KiB while the macOS ASLR slide is 16 KiB granular — four possible displacements selected by image_base mod 0x10000. Proven on a real artifact: the BL word at region offset 0x3377a0 is 0x97b322fa; solving for the callee under each candidate writer residue and inspecting the image shows only residue 0x4000 lands on a real prim entry (preceded by ret), so a reader with any other residue mis-calls by a multiple of 0x4000 and takes EXC_BAD_ACCESS. Also: the mapping hint itself collides with dyld — 51 of 60 bare runs exit 78 at EM-MMAP-CODE-REGION because REGION-OFF (16 MiB) leaves only 1.66 MiB of clearance over a 14.34 MiB image and dyld lands in that window. Fix by making region-to-text calls position independent across runs — an indirection table in the fixed-VA DATA region, or recorded far-call sites relocated at restore — then drop the exact-hint requirement. Do NOT widen REGION-OFF or accept the kernel's returned address: the first is a magic clearance constant over a growing image, the second makes the displacement vary freely instead of over four values, which is strictly worse. Acceptance: a snapshot image boots to exit 0 in 200 consecutive bare runs on macOS.

Claim: agent=snaprel workspace=.jj-ws/habu-relocate-snapshot-region-752042fe

MEASURED 2026-07-29 (agent=snaprel). The dot's conclusion is right and its
mechanism is now proven, but two premises in it need correcting, and the
prescribed fix should change.

What was measured. A plain snapshot image built by
`bin/hb --load tools/build-fixpoint-refresh.f -- snap` was run 200 times bare on
macOS 26.5.1: 169 runs exit 78 "hb: cannot map fixed code region", 18 runs die
of SIGBUS, and 13 runs exit 0. So the mapping collision is real and is the
dominant failure, and a snapshot image that does get its mapping still crashes
about half the time.

Why the mapping collision happens - this is the corrected part. It is not that
the image nearly fills the 16 MiB offset. The image is 14.33 MiB and the hint
lands about 1.7 MiB above its end, which is genuinely free. Probing the virtual
memory map at the process entry point with lldb across six runs shows that the
runtime places a roughly 1.1 MiB cluster of its own mappings (shared memory,
malloc zone metadata, guard pages, and one anonymous allocation) at a RANDOM
offset after the image: measured cluster starts at +0xE90000, +0xEF4000,
+0xFB8000, +0x10C4000, +0x1134000 and +0x1214000 from the load address, against
a hint that sits at about +0x100B000. When the cluster starts below the hint the
mapping succeeds; when it starts at or above the hint it collides and the boot
fails closed. The placement is randomised per run, so NO fixed offset from
__TEXT is safe. That is independent proof that widening REGION-OFF cannot work -
not because the constant would be magic, but because the cluster would simply be
re-straddled at the new offset. Keep the prohibition; the reason is stronger
than the one recorded here.

Why the surviving runs still crash. Confirmed from the engine's own crash
register dump: signal 10 (SIGBUS), program counter inside engine __text, link
register inside the restored JIT region, and x26 (DBASE) exactly equal to
align64k(text content base + REGION-OFF). Because the Mach-O load address moves
in 16 KiB steps while that rounding is to 64 KiB, the region-to-text distance
takes one of exactly four values, 0x1003000, 0x1007000, 0x100B000 and 0x100F000,
and a restored image only runs when the reader happens to draw the same value
the writer had. Four values, one correct, which matches 13 clean boots out of
the 31 runs that mapped at all.

The exact regression, found in the tree. `bootstrap/cg/forth.fs:4138` still
carries `EMIT-SNAPSHOT-REBASE-CALLS`, the pre-BL relocation pass: it walks the
region code area, matches the exact four-instruction absolute call sequence
(movz x16 / movk x16 / movk x16 / blr x16), and rebases the 48-bit literal from
the canonical text band to the live text base. `src/habu/habu2.f` has no
equivalent word at all. The direct-BL campaign (dot habu-aot-repl-bl-a71440da,
landed 1e9a3926) replaced those 16-byte absolute call chains with one 4-byte BL
and DELETED the relocation pass without providing a BL-shaped replacement. That
is the whole defect, and the Gforth mirror is the surviving copy of the design
that used to work.

Recommended fix, revised. Do NOT go back to a fixed region address with indirect
calls: that would undo a measured 13 percent live-code reduction for a
correctness problem that has a cheaper answer. Instead restore the missing
relocation in the BL wire format, and then stop demanding an exact address:
  1. Add `EM-SNAPSHOT-REBASE-CALLS` to src/habu/habu2.f. At snapshot write time,
     for every region-to-text call site, rewrite the BL immediate to a canonical
     displacement computed as if the region sat exactly REGION-OFF above the text
     base; at restore, rewrite it again for the live distance. Region-internal
     calls need no work because their displacement is already position
     independent. The two classes are distinguishable without any value guess:
     a canonical region-internal target lands at a NON-NEGATIVE region offset and
     a canonical text target lands at a negative one.
  2. Identify the sites exactly rather than by scanning for anything that looks
     like a BL. `LCEMITBL` is the single emit chokepoint - only three callers,
     `C-CALL`, `EMIT-P2-VALID-EMIT` and `EMIT-P2-STORE` - so a site table can be
     recorded at emit time. If a scan is used instead, it must be bounded by the
     dictionary's own code extents (record [0] start and [8] length) and must be
     proven against inline non-instruction bytes, not assumed.
  3. Once the immediates are relocated, the mapping no longer needs the exact
     hinted address. Accept the address the kernel returns and keep the existing
     boot assertion that the whole region lies within BL's +/-128 MiB of __text
     as the fail-closed check. That is a range check on a real capability, not a
     value heuristic, and it removes exit 78 entirely. The prohibition recorded
     above against accepting the kernel's address was correct only while the
     immediates were unrelocated.
  4. This changes the meaning of persisted region bytes, so bump
     SNAP-FORMAT-VERSION and mirror the new pass in bootstrap/cg/forth.fs.
Acceptance stands: a snapshot image boots to exit 0 in 200 consecutive bare runs.
