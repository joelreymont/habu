---
title: Native file-relative diagnostic positions during MULTI-ERR load
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T00:06:06.546776+02:00"
---

PROBLEM: The native MULTI-ERR checker load (src/core/checker.f CHECK + check-hook.f HOOK) emits per-def diagnostics with DEF-BUFFER-relative line/column/byte (JLINE/FAILB), because nothing re-points the diagnostic origin DIAGL0/DIAGC0/DIAGB0 (set by DIAG-ORIGIN!, checker.f ~4491; consumed by render.f JABS-LINE/JABS-BSTART) to each definition's FILE position during include. A 2-def file reports the 2nd def as line 1 and byte_start short by the ': ' prefix. This blocks retiring the tools/check-all-errors-core.f re-driver (habu-multi-err-checking-42db26f4), whose whole value-add over the native mode is FILE-relative positions (it feeds them via VERIFY:SOURCE-BUF-AT-IN-SCOPE -> SOURCE-AT! -> DIAG-ORIGIN! per def). Byte-exact golden test/golden/diag-all-errors.err pins file-relative positions (GDX-AE-BAD1 line 3 byte 100, GDX-AE-BAD2 line 4 byte 131). SOLUTION: thread the compiler's per-definition file position (line/col/byte at the ':' token during include/evaluate) into DIAG-ORIGIN! before each def is checked in MULTI-ERR mode, so JABS-LINE/JABS-BSTART become file-relative and match the re-driver byte-for-byte. The def-start position lives in the compiler input tracking (src/habu/habu1.f / habu2.f load path); habu2.f is currently off-limits (sibling worker), so coordinate territory. REGRESSION: a MULTI-ERR include of a multi-def file must produce the same file-relative line/column/byte_start/byte_end as check-all-errors-core.f for the same source; add a checked fixture comparing the two paths' JSON. Then habu-multi-err-checking-42db26f4 can rewire onto the native mode (pending the separate cascade-policy decision recorded there).

## Progress (DONE — checker-side, no habu2 edit needed)

Threaded file-relative diagnostic origin into the native MULTI-ERR load WITHOUT
touching habu2.f. Key finding: the compiler already records the def name-token
source pointer in DATA cell DEF-TKA-CELL ($250); the checker just needed a way to
consume it. checker.f cannot name DEF-TKA-CELL / data-base directly (checker.f
bakes before layout.f in the core prefix), so instead the DRIVER passes the
cell's ABSOLUTE address — the driver setup runs at top level (interpreted,
unchecked), so `data-base DEF-TKA-CELL +` needs no new trusted site.

src/core/checker.f:
- `DIAG-ORIGIN-SPAN! ( base name bl bc bb -- )` — reusable: given the eval-buffer
  base ptr, the name-token ptr, and the buffer start's file line/col/byte, scans
  and sets DIAG-ORIGIN! to the name token's file position (mirrors verify-source
  ABS-ORIGIN, so the native path matches the re-driver's math exactly).
- `MEO-*` state + `MULTI-ERR-ORIGIN! ( base namec bl bc bb -- )` — driver sets the
  eval-buffer base, the absolute addr of the compiler's def name-token cell, and
  the buffer start's file origin (1 1 0 for a whole file). `MEO-APPLY` reads
  `MEO-NAMEC @ @` (the live DEF-TKA) per def and re-points DIAG-ORIGIN!.
  MULTI-ERR-BEGIN/END clear the mode; off by default.
- CHECK now calls MEO-APPLY before rendering each MULTI-ERR diagnostic.

Empirically, native MULTI-ERR on the 4-def all-errors fixture now emits
BAD1 line 3 col 30 byte 100..103 and BAD2 line 4 col 26 byte 131..133 —
byte-for-byte identical to test/golden/diag-all-errors.err (only the `file` field
differs, set by DIAG-FILE!). definition_source, token, token_index, effects, and
repair_class already matched before this change.

REGRESSION (test/engine-suite.f): a MULTI-ERR run of the 4-def fixture with
MULTI-ERR-ORIGIN! set asserts BAD1/BAD2 carry the golden file-relative
line/column/byte_start/byte_end; a paired negative run (a rejected def on file
line 3, NO origin) asserts the diagnostic stays def-relative (line 1, no line 3),
proving the feature is off by default and that origin threading is what makes it
file-relative.

VALIDATION: install --force reached the byte-for-byte compiler fixpoint; full gate
(`bin/hb --load test/run.f`) PASS; typed-local-diff-lint 0; trusted-inventory
ratchet unchanged (TRUSTED 218 / TRUST 348 / SETCHECK 10 / TRUST-BARE 1 /
HOOK-INSTALL 12 — no new boundary); host-lint 0; filemap-lint 0.

REMAINING for habu-multi-err-checking-42db26f4 (CLI rewire, NOT this dot): a CLI
`--all-errors --load file.f` driver that reads the file into a buffer, calls
MULTI-ERR-BEGIN + MULTI-ERR-ORIGIN! (base = buffer, namec = data-base
DEF-TKA-CELL +, 1 1 0) + DIAG-FILE!, evaluates the buffer, and exits on
MULTI-ERR-END; that reader owns the one small engine-cell-read boundary. Still
gated on the separate cascade-policy decision recorded in that dot.
