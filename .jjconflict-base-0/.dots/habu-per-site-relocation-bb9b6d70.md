---
title: Per-site relocation record for AOT capture
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T16:56:23.143178+02:00"
---

Follow-up from the literal-split landing (6856f799): the emission-side separation is complete, but aot-capture.f's value-range scans (ACAP-SCAN-DATA/CODE) remain as the backstop because fully removing them needs an explicit per-site relocation record consumed at capture - a runtime recording table written at emit time (site offset + kind), reset per capture window in stdin.f, mirrored in forth.fs, serialized carefully in the fixpoint-critical AOT area. The register-kind-marker alternative was evaluated and REJECTED as value-fragile (an incidental 4-chunk scalar into x9 would collide with DATA recognition). With the record in place the scans retire and the linker chain-reject stays the only heuristic.

SCOPE WIDENED 2026-08-10 (quotations design lane, ruling by the cut owner):
this leaf now explicitly owns BOTH shape gaps in AOT capture, code AND data.
(1) ['] of a named word from the chain needs the engine's canonical x9
four-chunk absolute carrier plus an ADDRMAP bit set at final placement
(a new RELOC-ADDRS in publish.f mirroring RELOC-CALLS, fed by emitter-recorded
site indices, never byte-decoding) - 1 census definition, deferred until this
lands. (2) LATENT ON MASTER: the chain already emits DATA address literals in
MATERIALISE's minimal shape (string bodies via NSTR, fixed words), which
ACAP-SCAN-DATA cannot see (it recognises only the x9 four-chunk shape); this
bites the moment chain-compiled code enters an AOT capture window, which the
: cut makes true. Ordered BEFORE THE CUT, as already ruled on the strings
tranche. Quotation bodies themselves need NOTHING here (PC-relative adr,
P_pc_relative_adr class).

DESIGN RULED 2026-08-11 (aotsite lane; full report in the lane transcript,
probes in the session scratchpad). Failure mode REPRODUCED as silent: capture
succeeds, boot succeeds, the chain word reads 16 zero bytes at the baked
address; a naive fixed-width site record CORRUPTS (the 4-word rebase decode
swallows the next instruction). TWO LEAF CORRECTIONS: (1) the x9 four-chunk
carrier applies ONLY to the CODE/['] half - the DATA half needs a fixed-width
carrier in ANY register plus a new DATAMAP band (EM-AOT-RELOC-DATA never
checks Rd); (2) a per-site record is INSUFFICIENT for string literals - the
bytes do not travel (EM-AOT-RELOC-DATA reserves zeroed space, never copies,
and NSTR's arena sits below d0 by construction). RULED DESIGN, stage 1+2 =
the cut's hard requirement: hir.addr attribute on hir.const staged only by
EMIT-FIXED-SYM/EMIT-STRING (the knowledge lives at elaboration; the hir.quot
attribute road); BOTH literal memos key on (kind,value) - required, else an
address and an equal integer merge; MATERIALISE-ADDR always-4-lane carrier
(rebasing can change nonzero halves - minimal chains and movn are excluded
structurally); emit.f ADDR-SITES table; publish.f RELOC-ADDRS beside
RELOC-CALLS; habu1.f datamap-set + addrmap-set mirroring callmap-set's
doctrine; DATAMAP band in layout.f (REGION/32); C-DATA-ADDR sets the bit too
(the unification's engine half); aot-capture reads the bands and
ACAP-SCAN-DATA/ACAP-LIT9? are DELETED (layout.f:770-785 forbids exactly what
they do; capture never read ADDRMAP - the two-authorities defect). Strings:
option A (bake the window's DATA content + NSTR to allot-at-intern) NOW,
option B (inline bodies, PC-relative, retires NSTR) dotted as the long-term
form. [']: option ii (widen EMIT-ADDRS to the site's own Rd, stronger than
today, Reloc.v updated) - the x9-copy route is rejected as a patch. Stages
3-4 are completeness, not cut-critical. +1 insn per DATA site moves the
pinned byte baselines - re-pin in the same change.

SCOPE NOTE 2026-08-11 (seed-closure lane): this leaf's DATA gap is the
IN-WINDOW case only. PRE-WINDOW DATA literals (NSTR arena, NTRAP table -
allotted when the PREFIX loads, below d0) are a THIRD relocation class:
invisible to the window scan AND wrong to rebase by the window delta (their
correct value is fixed by the prefix's own DP, which differs between
metabuild host and bin/hb). Separate dot: habu-aot-pre-window (minted
2026-08-11).

DELIVERED AND MERGED (aotsite lane, 2026-08-11). The record replaced the value
heuristic end to end for the ENGINE's own literals:
 - stages 1-7 of the reloc work, including the per-site record and hir.addr;
 - the chain's publisher records its address sites (addrmap-set);
 - C-DATA-ADDR / C-DATA-ADDR-RAW record theirs, so the capture can FIND a
   chain-compiled DATA address instead of recognising one by its value;
 - the INLINER carries a copied chain's record with the chain
   (SNAP-RELOC:CARRY-SITE). This was the defect the record alone would have
   hidden: on the metabuild window the band recorded 21 DATA chains where 142
   were present, because a `create`d data word's body is short enough that
   C-CALL copies it into every caller and the copy landed at an offset nothing
   had recorded. After the fix, 142 of 142, extra 0;
 - the capture reads the bands and the value scans (ACAP-SCAN-DATA,
   ACAP-LIT9?) are deleted; a recorded site the window cannot place is a named
   refusal instead of silence;
 - the captured window's DATA CONTENT is baked, with declared address cells
   zeroed in the image and re-trapped at boot with the booting engine's own
   defer-unset xt.
Proven end to end, not by unit test: an engine-compiled DATA literal
initialised inside a capture window is read back after capture and boot by a
word compiled in that same window (PTY boot, test/aot-data-span-forge.f), so
the bytes travel AND the address literal rebases.

STILL OPEN, AND NOT REDEFINED DOWNWARD. Two acceptance demonstrations remain:
the chain-compiled s-quote returning its bytes, and the host-vs-seeded binding
mutation pair. Both need the native chain to run inside a capture window, so
both are blocked by habu-seed-the-chain-e98b03d4 (Stage B) and by
habu-aot-has-no-0b01043c, which is a precondition of that work rather than a
follow-up to it: content-keyed interning hands a window word the PRE-window
address of any body the prefix already interned.

