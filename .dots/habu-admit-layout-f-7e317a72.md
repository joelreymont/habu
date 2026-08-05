---
title: Admit layout.f body edits in package gate
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-29T22:58:09.106535+02:00\""
---

Full context: src/habu/layout.f is engine trunk like habu2.f - 240 pre-package global constants whose packaging is blocked on the stage0 using capability (habu-add-using-to-d815f0ab). The relocation work must change the BODIES of two existing globals (SNAP-FORMAT-VERSION bumped 4 to 5, DATA-START rebased to SNAP-RELOC:XTCELL-END), and tools/package-diff-lint.f correctly flags body-changed globals in an unpackaged file. Extend the ENGINE-BODY-EDIT admission from commit a943eb40 (currently exact-path src/habu/habu2.f, keyed on DEF-TAIL-ADDED 0= so body edits pass while NEW globals still fail) to also cover src/habu/layout.f, and pin the behavior in tools/package-diff-lint-test.f with both directions: a body edit to an existing layout.f global passes, a new unpackaged layout.f global still fails with its name in the finding. Falsify both directions by probe before calling it done. This admission retires when habu-give-layout-f-315df2ca finishes packaging the file.

Claim: agent=snapreloc workspace=.jj-ws/habu-relocate-snapshot-region-752042fe

MEASURED 2026-07-29 (agent=snapreloc). Half done, and the other half needs the
leaf re-cut: the key this dot names cannot clear the two findings it was minted
to clear. Evidence and the reason below.

What was built. tools/package-diff-lint-core.f now treats the engine trunk as two
rows -- src/habu/habu2.f and src/habu/layout.f -- reached through ONE comparison
site (ENGINE-TRUNK-N / ENGINE-TRUNK-AT / ENGINE-TRUNK-PATH?), for the reason the
fixture row table in the same file already gives: a weakening such as a suffix
match or a case fold then has exactly one place to live and changes both rows at
once, so the hostile fixtures kill it on both. ENGINE-BODY-EDIT? asks that
predicate instead of comparing one literal path. The header comment explains why
layout.f is the same kind of file as habu2.f, and the retirement condition now
names both sealing dots (habu-cont-habu2-emitter-493363e7 for habu2.f,
habu-give-layout-f-315df2ca for layout.f).

tools/package-diff-lint-test.f pins the new row in four directions, reusing the
existing path-parameterised fixtures: a comment-only body edit of an existing
layout.f global passes clean; a new unpackaged global in layout.f still reports
one finding with its name; the sibling src/habu/layout-extra.f (path as prefix)
still fails; and lib/layout.f (path as suffix, the shape the real per-target
files src/os/macos/layout.f and src/os/linux/layout.f have) still fails.

Falsified by mutation, both directions, as the dot required:
  - Replacing the layout.f row with a second habu2.f row: the positive case flips
    to "expected 0, got 1" and the suite fails.
  - Replacing `DEF-TAIL-ADDED @ 0=` with `true`: the new-global negatives flip to
    "expected 1, got 0", four failures.
  - Restored core is byte-identical to the pre-probe copy; suite green again
    ("test: ok / package-diff-lint-test: ok").

Why the two real findings are STILL reported, and why that is not a bug in the
work above. The gate on the exact relocation artifact still says:

  E-PACKAGE-OWNERSHIP src/habu/layout.f:41:12: `SNAP-FORMAT-VERSION` ...
  E-PACKAGE-OWNERSHIP src/habu/layout.f:652:32: `DATA-START` ...

The admission is keyed on DEF-TAIL-ADDED being clear, which asks "did this diff
touch the line the definition's opener or name sits on?". For a colon word that
question separates a body edit from a new definition perfectly, because the body
is on other lines. For a `constant` it cannot: the definer, the name and the
value are all one line, so changing the VALUE necessarily marks the head as
added, and `4 constant SNAP-FORMAT-VERSION` becoming `5 constant
SNAP-FORMAT-VERSION` is indistinguishable from a brand-new constant under this
key. layout.f is nothing but constants, which is exactly why the key that works
for habu2.f does not work here.

Do NOT widen the key to make these pass. Dropping the DEF-TAIL-ADDED test would
admit every new global in both trunk files, which is the whole point of the
entry, and any looser value test (line-count shapes, "the name looks familiar")
is a guess where a structural fact is available.

The structural fact that IS available, for whoever re-cuts this leaf: the lint
already reconstructs the complete pre-image of each file into OLD$ and walks its
definitions in SCAN-OLD-BOUNDARIES, which runs before SCAN-DEFINITIONS. A
definition that already existed is one whose NAME is defined in that pre-image;
a genuinely new global is not. So the correct key is "the head line changed AND
this name was defined in the old file", and a rename onto the path is still
caught by WHOLE-CHANGED as it is today. Implementing it needs the old-side scan
to record its definition names in a side arena (the shared lexer is mid-pass over
the new source during SCAN-DEFINITIONS, so the old text cannot simply be
re-lexed then), with the usual allocation-failure paths and fixtures. That is a
separate leaf, not a tail of this one, and it is worth doing because it fixes the
same blind spot for every allowlisted file, not just layout.f.

Is this the best long-term solution or a patch? The part that landed is
long-term: it is one predicate over a row table, narrow in the same way the
habu2.f entry was, pinned in both directions and falsified by mutation. The part
that did not land was stopped precisely because the only way to finish it as
specified would have been a weakening.

MEASURED 2026-07-30 (agent=admitkey). The re-cut leaf is done and both real
findings are gone. What follows is what was built, one deliberate departure from
the design this leaf was handed, the falsification matrix, and the gaps.

What was built. tools/package-diff-lint-core.f now keeps a side table of the
names the file's pre-image defined at global scope. SCAN-OLD-BOUNDARIES already
walked the reconstructed old source before the new-side scan starts, so
OLD-START-DEFINITION copies each global definition's name into that table as it
passes it; the two scans share one lexer, so the old text genuinely cannot be
re-read later. Records are the name bytes followed by one newline, which needs no
escape because a definition name is always a whitespace-delimited word token.
The table is sized to the length of the reconstructed old source, and that is a
proven bound rather than a guess: in the old text every definition contributes
its own name plus at least the one delimiter byte in front of it, and those
stretches cannot overlap, so the total the table needs is at most the text's own
length. The capacity test is still there and it throws E-PKGDIFF-NAMETAB (-4811,
because -4808 to -4810 were taken by the error-code lint while this was being
written); nothing is ever silently dropped. The table gets its own allocation,
its own release in the shared cleanup path, and its own injected-failure fixture,
exactly like the three mappings that were already there.

ENGINE-BODY-EDIT? now asks: is this an engine-trunk path, is this not a
whole-file arrival, and did this file already define this NAME at global scope?
The lookup compares whole names and ignores letter case, because that is what
"the same word" means to the dictionary being described -- bin/hb resolves `foo`
to `FOO`, so respelling an existing global in another case publishes no name the
file did not already have. That is not the case fold the row-table comment warns
about; that warning is about exact FILE PATHS, where the engine has no identity
relation to appeal to.

The departure. This leaf was specified as "the head line was not added OR the
name is in the old table". It landed as the name test alone, and the reason is
measured rather than stylistic. The head-line test admits nothing the name test
rejects except one shape, and it decides that shape wrongly: take a definition
the old file kept inside a `( ... )` comment, have the diff uncomment it and edit
one line of its body, and the definition's own opener line is never touched. The
head-line test therefore reads a word the file has never defined as a body edit
and admits it, while the name test reports it. Keeping a disjunct whose only
distinguishing case is one it gets wrong would have widened the entry for no
gain, so it is gone and the shape is pinned as a fixture. Restoring the disjunct
flips that fixture from one finding to zero, which is the evidence.

Falsification matrix, all through the real lint entry on constructed artifacts
(bin/hb --load tools/package-diff-lint-test.f):

  - existing layout.f constant's value changed, name unchanged -> clean. Same
    fixture on habu2.f -> clean; on the prefix sibling layout-extra.f and the
    suffix collision lib/layout.f -> one finding each, naming the constant.
  - a new constant added beside an existing one, both trunk rows -> one finding
    each, naming SNAP-SMUGGLED-BAND.
  - a constant renamed in place, definition count unchanged, both trunk rows ->
    one finding each, naming the arriving name SNAP-VERSION.
  - a constant that lived inside a package moved out to top level -> one
    finding, naming XTCELL-END. This is the pin on recording only global names.
  - the same constant respelled in lower case -> clean, because that is the
    same word.
  - a commented-out engine word uncommented with one body line edited -> one
    finding, naming EM-UNCOMMENTED. This is the pin that decides the key.
  - the four earlier engine-trunk directions (comment-only body edit passes, new
    global fails, prefix sibling fails, suffix collision fails) all still hold.
  - injected allocation failure for the name table -> E-MEM-SIZE, no mapping
    left behind, peak of three mappings; the reconstructed-old-source failure
    still stops at two and the mark-table failure at one.
  - the name table forced full -> E-PKGDIFF-NAMETAB, no mapping left behind,
    even though the file being read is an ordinary admitted body edit.

Mutations run against those pins, each restored afterwards; the restored core
file hashes byte-identical to the pre-probe copy (sha256
ca0e9130b5fc4e7ba1b35e328d212b8d999b6a73e1f7bdc9e7cc92a7092cca5d):

  - make the old-name lookup always true -> 22 failures: every new-constant,
    rename, promotion and new-global negative flips to zero findings.
  - record package-local names too -> the promotion case flips to zero findings.
  - compare names byte for byte instead of by word identity -> the lower-case
    respelling flips to one finding.
  - drop the overflow refusal and truncate instead -> the forced-full fixture
    stops throwing.
  - restore the head-line disjunct -> the uncommented-word hostile flips to zero
    findings.

Gates, all on the two-commit artifact (jj diff --git -r 2511e0a2..@) with a
bin/hb refreshed to the parent commit's engine: package-diff-lint exit 0 with
zero findings (both original layout.f findings cleared), package-diff-lint-test
exit 0, typed-local-diff-lint exit 0, error-code-lint 0 findings,
suite-coverage-lint 0 findings, host-lint 0 findings, and the parent commit's
test/snapshot-xt-cell-decl.f exit 0.

Gaps left open, none of them blocking this leaf:
  - Registering tools/package-diff-lint-test.f as a suite is not the same as
    scheduling it in every slice. It runs in the full gate and in the resident
    lint-tools group body, but its label is missing from the lint-tools slice
    filter in test/gate-stdlib-lib.f, so `test/gate-stdlib.f -- lint-tools` skips
    it. Pre-existing, and worth its own dot.
  - The overflow refusal can only be reached by the injected bound, because the
    real bound is proven sufficient. That is a guard against a future change to
    the recorder, not against any input.
  - The lookup is a linear walk of the table. It runs only for trunk-path
    definitions the diff actually changed, so a real artifact does a handful of
    walks; a file with thousands of changed definitions would notice.
  - The entry still retires whole when its two sealing dots land
    (habu-cont-habu2-emitter-493363e7, habu-give-layout-f-315df2ca).

Is this the best long-term solution or a patch? Long-term. The admission now
rests on an existence fact the lint reconstructs from the diff itself -- this
file defined this word before -- rather than on a line-position proxy for that
fact, and the departure above moved it further in that direction rather than
less. It fails closed on every failure of the table, it is narrower than what
was specified, and each clause is falsified by a mutation that flips a named
fixture. The one judgement call worth a reviewer's attention is admitting a
delete-and-redefine of an existing name as a body edit: that is correct for these
two files, because what this entry guards is the set of global names the trunk
file publishes, and rewriting a definition of a name the file already published
does not grow that set. A file that should not carry two definitions of one word
has a redefinition problem, which is a different question from who owns the name
and is not one this ownership lint can answer.
