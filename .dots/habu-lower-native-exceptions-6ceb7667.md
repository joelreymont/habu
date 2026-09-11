---
title: Lower native exceptions
status: closed
priority: 1
issue-type: task
created-at: "2026-07-26T22:59:20.066896+02:00"
closed-at: "2026-08-13T19:49:37.079971+02:00"
close-reason: "Landed at ed9431ce (three commits): catch lowers as an ordinary bare call to the engine entry - no new IR edge, the machine has one; the checker publishes each site's instantiated window in cells keyed by token ordinal (measured BEFORE the fit-check binds the tail - the recorded trap); throw needed nothing; evaluate struck. catch GONE from the refusal table (was 18, largest): 10 compile, 6 at the calling-quotation-under-locals ceiling 7578eaaa, 1 locals, 1 join. Census compiled 3306 to 3316 on the merge base. Parked-value seam applies by construction; differentials incl. depth-not-contents, two-windows, string-ordinal, parked-across-catch."
---

Full context: design Wave 4 adds throw/catch/evaluate edges to the IR and definition transaction. Acceptance: caught errors are testable in-process where safe; every failure releases modules/objects, restores dictionary/data/code marks, and publishes no half-definition; nested exception differentials pass.

RULED 2026-08-13 (design probe, this leaf): catch is an ORDINARY BARE CALL, no new IR edge. BTHROW resumes at a label inside BCATCH and both paths leave through one ret to one caller - the machine makes no second control transfer, so a two-successor terminator would model an edge that does not exist. throw ALREADY lowers (checker publishes arity 1->0 + CTL-DEAD; five shapes measured rc 0 through NMIGRATE, incl. inside ?do and across locals) - no throw work remains. evaluate is STRUCK from this leaf: checker-unsafe (rc 70 by name), production uses are TRUSTED one-liners, its transaction is the engine's LEVALREC. The transactional acceptance clause is ALREADY OWNED on both sides (engine habu2.f:63 rollback set RSP/CP/NDICT/XDS/DP; chain migrate.f RUN with HELD-RETRACT etc.; measured: refusals at -8651/-8286/-8508 leave marks identical to success; standing front-half test native-migrate.f:1765). Remaining implementation, ONE LANE, three commits: (1) checker exports the caught quotation's instantiated window width in CELLS at a catch site (RSCATCH already holds it; EFFECT-DIN-CELLS precedent) + dict.f reader mirroring SPELL-QUOT-DIN; (2) elaborate.f mints a CALLABLE-ROW at catch (entry = engine catch, dead=0) with the window as OPERANDS AND RESULTS - residency by construction, not by the travelling analysis (Fix Review Gate: the rule, not the lucky liveness consequence); engine catch entry must never acquire an NCLOB row - state it in clobber.f prose; (3) differentials D1-D5 from the probe (mutate-and-throw under=5 not 7; normal; nested; rc-local+rethrow - the 20-of-40 production shape; catch in ?do) - engine answers are the spec: catch restores DEPTH never CONTENTS (type-family.f:23 already says truncates). Every other pass: nothing. Probe corpus /private/tmp/hb-exc/. Exceptions-side interface from the rstack SSA ruling (92993f27): the catch call's return-vector argument is the entry vector on the exceptional path, ROUT on the normal - ordinary block args, NOTHING TO UNWIND under SSA. MEASURED EVIDENCE behind that sentence, from the rstack lane's probes, so this leaf does not rest on a ruling alone: (a) the engine's handler frame saves AND restores both the user return-stack depth and the loop-stack depth (habu1.f:2299/2334 save/restore, habu2.f:7186-7190 with underflow and past-region guards), so on the ENGINE path a caught throw already discards >r litter; (b) in-process, `42 >r [: 99 >r 7 throw ;] catch drop r>` answers 42 and a two-item-litter version answers 1 - the caller's own parked value comes back, not the litter; (c) the checker's RSCATCH unifies RCUR against the quotation's RIN and takes ROUT, and a token after an uncaught throw is refused as dead code ('at r> after throw'); (d) the asymmetry that should decide this leaf's scope: under the SSA lowering the parked values were NEVER in memory, so the chain has nothing to restore and never couples to the handler-frame layout - a memory-stack lowering would have created exactly that coupling. Caution for whoever writes the differentials: test/catch-frame.f's RSP-DRIFT assertions measure the engine's counter and go vacuous once these bodies compile through the chain (dot habu-catch-frame-drift-94f29c07). Expected census result: catch leaves E-HIR-UNMODELED (18 first-refusals, largest single spelling). Blockers removed 2026-08-13: quotations dot (the needed capability - quotation consumed by a call - is landed and measured running), Rocq proofs (scheduling not dependency), parent epic (leaf decomposition).

Claim: agent=exc-impl workspace=.jj-ws/habu-exc-impl
IMPLEMENTED 2026-08-13 (exc-impl lane, three commits in .jj-ws/habu-exc-impl).
(1) The checker publishes the caught window. RSCATCH measures the quotation's
own din and dout in CELLS BEFORE the fit-check and latches them; the exporter
files them against the TOKEN ORDINAL the site stands on, and EFFECT-CATCH-CELLS
( n -- n n ) answers by that ordinal (CELLS-NONE twice for no site; a real width
and CELLS-NONE for a body that never returns). THE MEASUREMENT ORDER IS THE TRAP
AND IS NOW WRITTEN DOWN AT ROW-CELLS: after UNIFY-IN the quotation's din row IS
the live row, because its tail has been bound to the rest of the stack, so
ROW-CELLS asked afterwards reports the whole compile-time depth - the
vector-depth guess wearing a checker's coat. Measured: `[: 1 2 3 throw ;] catch`
over a one-deep stack answers 0 before the check and 1 after. The KEY is the
token ordinal because report ordinal k IS tape ordinal k (feed.f ORDER-CK
enforces one tape row per report, in order, exactly once); a latch answers a
definition's second catch with the first's window, and numbering the catch sites
or the quotations would make two counters for one fact. Ceiling CWIN-MAX = 16
sites per definition, chosen low enough to be REACHABLE through the 128-token
tape so the past-cap refusal is measured rather than argued.
(2) `catch` lowers as an ordinary bare call: one new HIR:ctrl member, one
BDECLARE-CONTROL row, DO-CATCH mirroring DO-EXEC with in = out = window+1.
Falsified: declaring the window on one side only is E-NELAB-ARITY in both
directions. Declaring it on NEITHER side still compiles and answers correctly,
because CALL-OPERANDS+ already hands the whole vector over - so the pair is the
callee's real contract (which cells are the call's OUTPUTS rather than
survivors) and not a residency trick; the prose says so rather than overclaiming.
(3) Differentials in test/compiler/native-catch.f, registered in
test/gate-stdlib-cases.f and test/gate-stdlib-inline-lib.f.

SPLICE, as asked: a body holding `catch` can never be copied into a caller.
SPLICE-STAGING maps `control` to `call`, so REC-BODY? rejects the token and
migrate.f never records that body - there is no staleness hazard for an
ordinal-keyed table, structurally.

THREE CEILINGS, ALL MEASURED, NONE OF THEM CATCH'S OWN:
(a) a caught body that NEVER RETURNS is refused by name (E-NELAB-QUOT).
src/compiler/native/select.f takes ONE routine contract for the whole module, so
a function with no return inside a definition that has one is lowered as though
a return followed it and leaves its last memory order unread (E-A64RAV-ORDER,
measured). The SAME body inside a definition that itself ends in a throw
compiles and runs - which is why the refusal is unconditional rather than
resting on the enclosing word's shape. Owner: habu-compile-a-quotation-7efa798e,
whose 2026-08-11 note already states this exact finding.
(b) a quotation body holding ANY control structure is refused
(E-IR-VERIFY-SUCCARG, -8088). Measured on the PARENT binary through the
pre-catch route - a body handed to a callee that declares a quotation argument -
so it predates this lane. NEEDS A DOT.
(c) a quotation body that CALLS, under a definition holding a locals group, is
refused (E-IR-VERIFY-SCOPE, -8092). Also measured on the parent binary through
the same route. NEEDS A DOT. It is what bounds the production shape: `[: WORD ;]
catch {: rc:n :} rc 0 <> if rc throw then` compiles today only while the caught
body does not call.
