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
