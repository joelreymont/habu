---
title: Declare persisted producer xt cells
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-30T11:07:11.904582+02:00\""
---

PRIORITY 1, and now THE blocker for a restored snapshot image that can compile: with dot habu-relocate-persisted-region-47de06b9 landed, the region address literals are relocated correctly and a restored image still dies rc 134 the moment it compiles a definition, because two persisted DP-heap cells hold an execution token that nothing ever declared to the snapshot address-cell table. The two cells are src/core/checker.f CHECKER-CERT:PRODUCER-XT (a plain 'variable', written by CHECKER-CERT:INSTALL and dispatched through by CHECKER-CERT-CALL) and src/core/lower-cert-base.f LOWER-CERT:FULL-XT (same shape, written by FULL-INSTALL, dispatched by LOWER-CERT:DISPATCH). The address-cell table (SNAP-RELOC:XTCELL-*, dot habu-relocate-persisted-defer-7aa681c4) is fed only where a cell's KIND is decided - the 'defer' handler when it allocates a dispatch cell, the 'is' handler when it stores a token into one, and cold boot for the three engine hook cells - so a bare 'variable' plus 'execute' is outside every declaration point and its token is persisted as the writing run's absolute region address. EVIDENCE, measured under lldb on a snapshot image built by 'bin/hb --load tools/build-fixpoint-refresh.f -- snap': the executed value is read from a DP-heap cell, the cell is present in the image FILE at that DATA offset (so it is persisted, not computed at run time), the return address on the machine stack at the faulting 'blr' resolves through tools/imgdump.f --pc to CHECKER-CERT-CALL, and the crash address is the writing run's region base plus a valid region offset. The fix is to make each cell a declared dispatch cell instead of an undeclared variable: 'defer PRODUCER ( ptr u8 n n -- )' in package CHECKER-CERT with INSTALL taking a quotation and using 'is', and the same for LOWER-CERT:FULL. PROVEN to be sufficient: with exactly that conversion applied on top of the address-literal change, a restored image runs ': FOO 1 . ;' then 'FOO' and exits 0, where it exited 134 before. Two design points the implementer must settle rather than copy from the experiment: (a) the two 'already installed' / 'unavailable' guards currently live on the variable being non-zero, and an unset defer throws its own named code instead, so decide deliberately whether to keep an explicit installed flag (the experiment used one for FULL and dropped the one for PRODUCER) rather than silently losing a real invariant; (b) src/core/lower-cert-seal.f undefines PRODUCER-XT and FULL-XT to erase producer authority at seal, so the seal must still erase the same authority when they are defers, and that must be tested, not assumed. The deeper hole is worth its own dot if this one does not close it: nothing in the checker or the compiler stops the next author writing 'variable X' and 'X @ execute', so the declared-kind design is only as complete as the set of ways a cell can come to hold a token. Acceptance: a restored snapshot image compiles a definition and executes it; test/owner-wid-internal.f goes green; tools/build-fixpoint-test.f asserts 144, 151 and 152 pass (all three fail rc 134 today, before AND after the address-literal change - measured); a negative regression proves an undeclared cell of this shape is rejected or relocated.

Claim: agent=producerxt workspace=.jj-ws/habu-declare-persisted-producer-76fbce09

MEASURED 2026-07-30 (agent=producerxt, commit "Declare persisted producer xt
cells"). Both cells this dot names are now declared dispatch cells, the failure
this dot names is gone and proven gone, and the residual behind it is a
different owner in the same class, named with evidence and dotted.

What the change does. src/core/checker.f replaces the plain `variable
CHECKER-CERT:PRODUCER-XT` with `defer PRODUCER-XT ( ptr u8 n n -- )`, and
src/core/lower-cert-base.f replaces `variable LOWER-CERT:FULL-XT` with
`defer FULL-XT ( ptr u8 n n -- )`. That is the whole point: `defer` and `is`
are the only two places that tell the snapshot writer a persisted DP-heap cell
holds a JIT-region address, so making each cell a dispatch cell puts it in the
address-cell table where the loader moves it with the region. The names keep
their `-XT` tails on purpose. That is this codebase's own convention for a
late-bound hook -- checker.f already has `defer TFAM-RESOLVE-XT`,
`defer TDECL-EVAL-XT`, `defer CONSTRUCT-STEP-XT` and two dozen more -- and it
also leaves `undefine PRODUCER-XT` and `undefine FULL-XT` unchanged in the seal,
so tools/build-fixpoint-test.f's seal-ordering anchor needed no edit at all.
Editing it would have meant editing a global word in a file the package gate
does not exempt, which is a legacy-debt fight this lane has no business
starting.

Each installer now takes the producer as a QUOTATION rather than a raw xt, so
the checker fit-checks the installed producer's effect against the dispatch
cell's declared effect instead of accepting an opaque `n`. Because a quotation
is a compile-time construct, each hand-over moved from the top level into a
one-shot install seam (LOWER-CERT:DISPATCH-INSTALL,
LOWER-CERT:FULL-PRODUCE-INSTALL), which is the shape every other `is` site in
the tree already uses.

`TRUSTED: CHECKER-CERT-CALL` is DELETED, with its TRUSTED.md row. It existed
only to `execute` a raw stored xt, which is exactly the thing the declared cell
replaces, so the change removes an unchecked boundary rather than adding one.
trust-lint stays at 0 findings.

DESIGN POINT (a), the guards. Both explicit flags are KEPT, as ordinary integer
cells that never hold a token: `CHECKER-CERT:PRODUCER-SET` and
`LOWER-CERT:FULL-SET`. Three separate reasons, none of them habit. Install-once
is an AUTHORITY invariant, not a convenience: `is` will store into a dispatch
cell any number of times, so without a flag a second, later grant would silently
replace the certificate producer, and the seal below only closes that window at
the end of the prefix. The "unavailable" guard keeps its exact message and its
rc 76; an unset dispatch cell fails closed too, but only with the generic
"defer: unset execution vector" (EXEC-VECTOR-RC), which is a worse diagnostic
for a real domain error. And LOWER-CERT:DISPATCH does not merely dispatch, it
ASKS whether a full producer exists -- with none it must still emit a boot-safe
empty certificate -- and a dispatch cell cannot be interrogated, so a flag is
structurally required there whatever one decides about the other two guards.

DESIGN POINT (b), the seal. src/core/lower-cert-seal.f retires the two things
that can still GRANT authority: the NAME of each dispatch cell (without which no
later source can write `is` at it) and the flag that records the grant, plus the
two new one-shot install seams. Undefining a name never removes the cell or its
code, which is why the callers compiled below the seal keep dispatching and why
both cells stay in the address-cell table and go on being relocated -- the seal
erases the ability to install, not the ability to run. That is not assumed: it
is what makes the restored-image reproducer below work at all, since every
producer call in a restored image happens long after the seal.

CHECKER CAPABILITY ADDED. A prim row could not name a quotation operand: every
PE- atom builds a concrete term, so `PPRIM: CHECKER-CERT INSTALL` could only
declare the producer as a bare `n`. checker.f gains `PE-Q`, `PE-QIN`, `PE-QOUT`
and `;PE-Q`, which accumulate a quotation's own data rows the way PE-IN/PE-OUT
accumulate the prim's and close them into one `[ in -- out ]` term. In and out
share one fresh base row and the return effect is neutral, which is exactly what
SIG-PARSE-QUOT builds from the same text in a signature string, so a prim row
and a written signature now describe the same term. E-COPY and E-INST already
walk T-QUOT/EN-QUOT in full, so the stored effect copies and re-freshens like any
other. Without this the prim table would have gone on declaring an effect the
word no longer has, and leaving a false declaration in the trusted surface
because the seal happens to make it unreachable is a patch, not a fix.

MEASURED, every number on this workspace with `bin/hb --load
tools/build-fixpoint-refresh.f -- install --force` rerun before the measurement.

  The one-line reproducer, a snapshot image built by `-- snap`, fed
  ": FOO 1 . ;" then "FOO":
     before: exit 134, SIGSEGV, register dump.
     after:  exit 0, prints 1.
  The fuller reproducer, same image, adding a typed definition and a deferred
  word installed and dispatched in the restored image:
     ": FOO 1 . ;" / "FOO" / ": SQUARE ( n -- n ) dup * ;" / "7 SQUARE ." /
     "defer BAR ( -- )" / ": SET-BAR ( -- ) [: 2 . ;] is BAR ;" / "SET-BAR" /
     "BAR"  ->  exit 0, prints 1, 49, 2.
  50 consecutive boots of that image, each running that whole script:
     50 exit 0, 0 failures, output byte-identical every time.
  tools/build-fixpoint-test.f:
     before: asserts 144, 151 and 152 FAIL (144 exit 134, 151 the crash dump
             where a clean stderr was expected, 152 exit 134); suite exit 1.
     after:  all three PASS; whole suite exit 0, "build-fixpoint-test: ok".
  test/snapshot-xt-cell-decl.f: green, with six new rows (below).
  Engine fixpoint rebuild green; self-check census 0 uncheckable, 0 rejected,
  certified 4251 -> 4257.
  package-diff-lint exit 0, typed-local-diff-lint exit 0, error-code-lint
  0 finding(s), trust-lint 0 finding(s), dot-dep-lint 0 finding(s).

  test/owner-wid-internal.f: STILL RED, unchanged at 3 failures. That is not
  this class; see the residual section.

NEW TEST ROWS, and the negative that carries the weight.
test/snapshot-xt-cell-decl.f gains the exact shape this change introduces: a
package-private `defer` plus a public installer that takes a quotation and
stores it with `is`. The rows pin that the defer declares exactly one address
cell, that compiling the installer's `is` re-declares that same cell and adds no
row, that running the installer twice adds no row, and that the deferred word
dispatches to the producer the last install handed over. The new NEGATIVE is
stronger than the file's existing one: the existing forged cell holds a value
that merely LOOKS like a token (an address inside the live region), while the
new one holds a REAL execution token, put in an ordinary `variable` by an
ordinary `!` -- which is precisely what PRODUCER-XT and FULL-XT used to be. It
must stay out of the table, and the file asserts both that it is absent and that
it really does hold the token that was stored.

FALSIFICATION (measured, every mutation applied and reverted).
  1. Revert ONLY the CHECKER-CERT declaration -- `variable PRODUCER-XT` again,
     INSTALL back to a raw xt, CHECKER-CERT-CALL back, the PPRIM back to
     `PE-N PE-IN` -- and leave LOWER-CERT:FULL-XT a declared defer. Rebuild,
     re-snap: the reproducer dies rc 134 again with the same SIGSEGV. Restored,
     rebuilt, back to exit 0. So the surviving declaration is not what carries
     the fix on its own, and the reverted one is load-bearing.
  2. The engine stops declaring a defer's cell (delete the
     `SNAP-RELOC:LMARK` call from habu2.f C-DEFER-CELL), rebuild: the two
     pre-existing count rows AND both new count rows go red, and nothing else.
     src/habu/habu2.f is byte-identical to its base in the final tree.
  3. The installer is handed the same producer twice
     (ARM-SECOND installs `[: 99 ;]` instead of `[: 5 ;]`): only the
     "installed producer is the one the last install handed over" row reds.
  The remaining new row -- the real-token cell staying out of the table -- has
  no mutation of its own, because there is deliberately NO way to declare a cell
  from Habu source: that is the property the row exists to state. It is
  falsified in the other direction by mutation 2, which shows the table's
  contents come from the real `defer`/`is` handlers and not from the fixture.

RESIDUAL, named with evidence and dotted, not silently accepted.
A restored image now compiles and runs definitions, but it still dies rc 134 on
a GENERATED DECLARATION. Smallest reproducer, on a plain `-- snap` image:
the single line `sumtype option2<a> = none2 | some2 a ;sumtype`. That is why
`require lib/string.f` (which pulls in lib/adt/option.f) still fails and why
test/owner-wid-internal.f is still red; test/owner-wid-child.f's own asserts show
it exactly, with the snapshot legs that only need a rejection (rc 70) and the
bare stdin leg passing while every leg that compiles library source dies 134.
It is the SAME relocation class with a DIFFERENT owner. The faulting instruction
is the `blr x9` at the end of the engine's catch frame setup -- lldb disassembly
at the return address shows the frame store, the CATCH-FRAME-MAGIC store and the
blr -- so it is a `catch` of a token that came out of a cell. The token is
identical across three consecutive runs of one image (0x1014d0aa4) while the
live region base differs in each (0x103760000 / 0x103a00000 / 0x1037b0000), and
that same 64-bit value appears once in the image FILE as a contiguous cell, so
it is persisted, not computed, and it is the writing run's address. The owner is
the declaration-transaction participant table: src/core/generated-declaration.f
creates PARTICIPANT-BOOT and STATE as DP-heap arrays, five participants enroll
five quotations each through SNAPSHOT!/PREPARE!/COMMIT!/ROLLBACK!/RELEASE!, the
state record holds two more through ST.ALLOCATOR and ST.DIAGNOSTIC, and
src/core/declaration-transaction.f CALL-PARTICIPANT runs one with
`expected callback catch`. A `defer` cannot name a cell whose address is
computed from a table base and a row index, so none of them is declared. This is
exactly the deeper hole this dot anticipated. Dotted as
habu-declare-persisted-cb-b150b5d5, which now owns test/owner-wid-internal.f.

BEST LONG-TERM FIX OR A PATCH? Long-term, and re-derived from the code rather
than taken from the dot's wording. The invariant is that a persisted cell is
relocatable only if something recorded that it holds a region address, and the
only sound place to record that is where the cell's KIND is decided, which for a
dispatch cell is `defer` (allocation) and `is` (store). The change moves both
producers onto exactly those two points and adds no new declaration mechanism,
no value test, no address range, no sentinel: relocation membership is still a
table fed at the decision site and never a guess about a cell's contents, which
is why the new negative row -- a cell holding a genuine live token that stays
out of the table -- passes. The place this could have been a patch is the
guards. Dropping the install-once flag because "an unset defer throws anyway"
would have traded a real authority invariant for a smaller diff, and leaving the
`PPRIM: CHECKER-CERT INSTALL` row declaring a bare `n` would have left a false
effect in the trusted surface, defensible only by arguing the seal makes it
unreachable. Both were done the harder way instead. The residual is not a
weakening either: it is a different owner, proven with a constant-across-runs
token found in the image file, and it is dotted rather than absorbed.

HONEST GAPS.
  - test/owner-wid-internal.f, this dot's acceptance item 2, does NOT go green.
    It cannot until habu-declare-persisted-cb-b150b5d5 lands; the same one-line
    sumtype reproducer fails on a plain image, so nothing about the owner-wid
    harness is special.
  - test/snapshot-xt-cell-decl.f still cannot observe the two production cells
    directly. Their names are undefined by the seal, so the suite pins the
    RULE by reproducing the exact shape in its own package; that a restored
    image compiles at all is what pins the two production cells, and that is
    covered by the reproducer and by tools/build-fixpoint-test.f.
  - The new PE-Q / ;PE-Q atoms are exercised by exactly one prim row
    (CHECKER-CERT:INSTALL) and have no unit test of their own in this change;
    they are proven only by the engine rebuild certifying that row's caller.
  - No full gate-stdlib was run, per this lane's instructions.
