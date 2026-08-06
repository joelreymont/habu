---
title: Attribute the wordlist-isolation red
status: active
priority: 2
issue-type: task
created-at: "2026-08-05T11:40:03.431641+02:00"
---

test/run.f red: 'hb long dictionary wordlist isolation' exit 70 'no authenticated package context', pre-existing on master 10281011 (device-suites lane pristine-parent control). Not codegen-owned on its face — attribute to the owning campaign; if it turns out to be fallout from the codegen engine-lookup work (83ec063f lineage), it comes back to the codegen program. Diagnose enough to route it, then hand off or fix.

Claim: agent=wordlist workspace=.jj-ws/habu-attr-the-wordlist-d7e28f45

ATTRIBUTED AND FIXED 2026-08-06 (agent=wordlist).

ATTRIBUTION: the case never passed under package authentication, and no codegen
change caused it. The rule that outlaws the old shape is src/habu/xref.f
LIVE-PKG: a definition's package context must be EITHER the exact all-zero tuple
(record cell, public WID, private WID and get-current all zero - top level) OR a
live namespace record whose two WIDs are the protected engine cells. `wordlist
constant W  W set-current` leaves the record cell zero while get-current is not,
which is neither branch, so CHECKER-PKG-CONTEXT-REJECT refuses the definition at
DEFINITION time. This is not lookup-side, and the codegen engine-lookup lane
(83ec063f lineage) is not implicated - the refusal predates it.

It is also not news to the tree. src/core/checker.f says at the refusal site that
"whether a bare word list ought to BE a legal definition context is a separate
question", and names the owner: habu-model-bare-wordlists-9e7c3521, open since
2026-07-28. That dot records the same reproducer, a mutation proof of the exact
throw site, and confirms it on a pre-seal baseline engine (86ed2dec), so the
shape has been illegal since package authentication landed rather than since any
recent change. What 9e7c3521 left open was only the SEMANTIC decision; commit
0a5b92d6 had already discharged its diagnostic clause by turning a bare uncaught
7136/rc 67 into the named refusal and rc 70.

THE DECISION TAKEN, AND WHY THE ENGINE IS RIGHT. Every definition has to be
attributable to a package or to top level, because that attribution IS the
checker's scope model - private/public visibility, duplicate rejection, and
scope rollback all key off it. A bare wordlist has no owner to attribute to, so
admitting one would mean certified definitions with no scope: a hole in the
model rather than a convenience. The engine already provides the lawful way to
get an isolated wordlist, which is a package, so nothing is lost by refusing.

WHAT THE TEST DOES NOW. test/gate-dictionary-lib.f WORDLIST-SOURCE exercises the
SAME property - a name defined into one wordlist is not reachable in another -
through a package: it opens `package LONGWL public`, records `get-current` (the
wordlist the definition actually lands in, taken from the engine rather than
assumed), defines the name, closes, and then asks the same two searches. The
expected output is unchanged, `-1` then `0`, because the property is unchanged.
Isolation now holds for the stronger reason: the wordlist has an owner.

AND THE NEGATIVE HALF IS PINNED. A new case, dictionary/wordlist-bare, runs the
old shape through GE-EVAL-FORK-BAD and requires rc 70 with "no authenticated
package context" on stderr, so re-admitting bare wordlists becomes a decision
somebody makes here rather than something a later change does quietly.

FALSIFIED BY MUTATION, not by inspection. Relaxing LIVE-PKG's first branch to
admit any zero record cell makes the old reproducer compile and print exactly the
`-1` / `0` the original test expected (rc 0); restoring it returns the refusal
and rc 70. So the two cases pin both directions of the decision: the positive
case would still pass under the relaxation, and the negative case is what catches
it. Incidentally confirmed on that mutation: the engine re-reads its prefix from
source at boot, so a src/ edit changes bin/hb's behaviour with no rebuild.

GATES, on the tree rebased onto master c8ca14d7. test/run.f: the wordlist red is
gone and the phase that carried it, `native dictionary/checker gate phase`, is
PASS (4606ms). The whole remaining red set is `fork json-read-perf ratchets` and
`test/compiler/checker-model-manifest.f`, plus `GROUP: stdlib/tail-pure` which is
only the roll-up of the latter - both known. The bootstrap-mirror ADT reds that
were present before the rebase are gone too, closed by master's own lane rather
than by anything here. maki 192 PASS / 0 FAIL. Four lints 0 findings.

That the negative case is genuinely SCHEDULED and not merely present is measured
rather than assumed: on its first run it was miswired (it called the
rc-0-asserting GE-EVAL-RUN-STDIN) and the gate reported `FAIL: hb bare wordlist
definition refused`, so the phase executes and evaluates it; it passes now that
it runs through GE-EVAL-FORK-BAD, which is the harness word for a case whose
subject is expected to exit non-zero.

Is this the best long-term answer or a patch? Long-term. The alternative - giving
the checker an anonymous-wordlist state - would have added a definition context
with no owner purely to keep one test's spelling, which is the hole the
authentication exists to close. The property under test is fully covered, and it
is now covered through the shape production code actually uses. What this does
NOT do is close habu-model-bare-wordlists-9e7c3521's own decision record; that
leaf should be closed against this one, since the semantics it was left open for
are now decided and pinned in both directions.
