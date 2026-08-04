---
title: "Unify the publication seam's record stores"
status: active
priority: 2
issue-type: task
created-at: "2026-08-03T20:38:10.224600+02:00"
---

Four parallel address-keyed record stores now live at the publication seam with the same key and lifetime: the replacement log (publish.f), NCLOB clobber records, NINL body records, and the word-model callable rows. Each grew independently; each has its own capacity, its own refusal, its own growth dot. Unify into one publication record per address carrying all four facts (old/new spans, clobber sets, recorded body, interface), one capacity policy, one write site - net lines negative, all existing refusals preserved by name, every consumer reading through the same readers. Subsumes habu-grow-the-republication-52ef5df0 and habu-grow-the-recorded-f0e9f5da (close both with this).

Claim: agent=pub-unify workspace=.jj-ws/habu-unify-the-publication-1b2dc04a

MEASURED 2026-08-04 (agent=pub-unify). MEASURED-REJECTED. No code change. The
consolidation this dot asks for cannot be built without changing behaviour, and
the reduced version that can be built is net POSITIVE in lines. Evidence below,
in the order it was established.

THE PREMISE IS WRONG ON TWO OF THE FOUR STORES. The dot says four parallel
address-keyed stores with the same key and the same lifetime. Two of them are
neither.

  - The replacement log in publish.f is keyed by (name bytes, wordlist id), not
    by an address: LOG-ROW? compares a wid and a name, and the start addresses
    it holds are DATA in the row (LOG-OLD-START/LOG-NEW-START), not its key. Its
    readers take `( ptr u8 n n )` and never an address.
  - That log is also never truncated. LOG-N is written in exactly one place,
    LOG+ (publish.f:202), and only upwards. publish.f DOES register a
    CODE-RECLAIM watcher, but its body is RECLAIMED, which lowers the CLAIMED
    high-water mark and touches no row. That is deliberate and argued at
    publish.f:117-121: "The log is evidence and not a cache ... a row can never
    be dropped to make space". So "all truncate on the same watcher notice" is
    false for it.
  - NMIGRATE's callee list is not address-keyed either. It is a positional list
    read by index 0..CALLEE-N (DECLARE-CALLEE1 reads `k cells CALLEE-ADDR + @`);
    there is no lookup by address anywhere. Its capacity is 4, it describes
    OTHER words rather than the one being published, it registers no watcher,
    and it is cleared by every RUN (migrate.f:635). Its lifetime is one
    migration.

So the real overlap is TWO stores, NCLOB and NINL. Four words are duplicated
between them, and three are byte-identical (verified by diff): ROW-OF (6 code
lines), FLOOR-ROW (6), DROP-FROM (5) identical, ORDER-CK (7) differing only in
its die message. 24 code lines in each file.

BLOCKER 1: THE TWO CAPACITY POLICIES ARE OPPOSITE IN KIND, AND THAT IS MEASURED.
One row per address means one table and one ceiling. The two ceilings do
opposite things at the same moment.

  - A FULL CLOBBER TABLE REFUSES THE PUBLICATION. RECORD-CK throws E-NCLOB-CAP
    for a new address when the table is full (clobber.f:202), and publish.f asks
    it at line 378, BEFORE a byte is written. Probed directly: filling NCLOB to
    128 rows and then migrating a definition throws E-NCLOB-CAP, the word is not
    republished, and it still runs the engine's code. Green.
  - A FULL INLINE TABLE PUBLISHES ANYWAY. test/compiler/native-inline.f
    CAP-CASES (1225-1257) fills the table, then requires that the next migration
    IS published (`NPUB:REPUBLISHED? TTRUE`, line 1230), that it runs
    (`3 NINL-FULL` = 96), that the size rule still says yes about it, and that
    DECLINED went up by exactly one. inline.f:186-193 argues why: a full table is
    not a reason to refuse a word the chain compiled.

With one table these are the same table being full, and the first publication
past the ceiling would have to both throw and not throw. Picking throw deletes
NINL's decline and turns CAP-CASES red; picking decline silently drops a clobber
row, which is the one thing clobber.f:36-41 forbids. Keeping two ceilings in one
table is not "one capacity policy" and saves nothing. Either way the suite needs
more than a rename, which this dot's own constraint says is where to stop.

BLOCKER 2: MARK/RELEASE CANNOT COEXIST WITH THE WIDEN RULE IN ONE TABLE. Probed:
NINL:MARK/RELEASE drops rows with NO code reclamation behind them - ROWS goes
back and KNOWN? goes false for an address whose code was never touched. NCLOB
deliberately has no such operation, because a LIVE row is exactly what
E-NCLOB-WIDEN holds a second record against; the same probe shows the widen
refusal firing on a live row and an address with no row accepting any set
silently. Sharing one table hands NINL:RELEASE the power to defeat E-NCLOB-WIDEN
for live published code. The suites depend on this: RETIRE and CAP-PHASE both
release, and CAP-PHASE releases 64 rows written at synthetic addresses
($21000 + k*4) that no code occupies - sound for bodies, and unsound as clobber
rows the moment the tables are one.

THE ARITHMETIC, MEASURED, FOR THE REDUCED VERSION THAT IS BUILDABLE. Sharing
only the four duplicated scan words, both stores keeping their own tables. A
first hypothesis that this cannot be written checked at all was FALSIFIED by
probe: a table base threaded on the stack as `ptr a` under the locals certifies
(a bare `p:ptr` LOCAL does not, which is what docs/forth.md warns about).

  - src/compiler/native/npubrow.f written and certified: 37 code lines, 51 raw.
  - clobber.f rewritten onto it and certified; native-clobber suite green.
    88 -> 72 code lines, 243 -> 229 raw. Saves 16.
  - inline.f rewritten onto it and certified; native-inline suite green.
    219 -> 203 code lines. Saves 16.
  - Net: -16 -16 +37 = +5 production lines. On raw lines, +23.

And it delivers none of the four things this dot asks for: not one record, not
one capacity policy, not one write site, and not one watcher instead of three
(NINL still needs its own registration for the staging clear).

This is the habu-share-the-timing-2eda3703 result again, for the same reason
LESSONS.md records under "The line ledger of a consolidation is not the
duplication it removes": a new checked package costs its header, requires,
package/public/;package and its own statement of the rule before its first word,
and that exceeds 24 duplicated lines per store.

WHAT IS ACTUALLY WORTH DOING, IF ANYTHING. Nothing here. The two stores already
share what can be shared without cost: they cite each other's arguments by file
and line, and the invariant the suffix drop rests on (a publication's slot is
above every slot claimed before it) is held as a REFUSAL in one place,
publish.f's E-NPUB-SLOT, rather than assumed in two. That is the single
authority this dot was reaching for, and it is already there.

Subsumed dots habu-grow-the-republication-52ef5df0 and habu-grow-the-recorded-
f0e9f5da are NOT closed by this: they are about growing two ceilings, which is
untouched by the rejection and still open work.

Gates on the unchanged tree: all 16 native suites green; native-inline twice in
one process green both times; maki 200/200; codegen-compare 0 findings;
codegen-compare-test and codegen-workload-test green; typed-local-diff-lint and
package-diff-lint exit 0; error-code-lint 0 findings; dot-dep-lint 0 findings;
gate-stdlib --pool-slots 3: 185 PASS, 0 FAIL, rc 0.

Is this the best long-term answer or a patch? It is a refusal to change working
code, which is the only long-term-correct answer available: the consolidation
would have had to weaken either the clobber capacity refusal or the inline
decline, and both are landed rules with suites behind them. The reduced sharing
was built and measured rather than argued about, and it lost on its own terms.
