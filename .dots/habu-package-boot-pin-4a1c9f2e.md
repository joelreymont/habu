---
title: Package tools/boot-pin.f so its prefix list can be edited
status: active
priority: 2
issue-type: task
created-at: "2026-08-21T12:00:00.000000+02:00"
---

The tree already wrote this dot's contract, in src/core/generated-declaration.f:403-407:
"What still refuses is tools/boot-pin.f, which has no package at all: adding one
prefix row there reports `BP-EACH` defines a changed module word outside a
package (measured against this tree by dot habu-single-prefix-load-17a8c792,
which priced the same edit and put its own mark at the end of an existing prefix
file instead). boot-pin.f's packaging belongs to its own dot; when it lands, this
package moves out unchanged." No dot existed. This is it.

WHY IT COULD NOT KEEP WAITING. tools/boot-pin.f owns the ONE canonical
boot-prefix path list (BP-EACH), and test/run-engine-set.f keys every gate phase
on whatever that list names, so the list is what "the engine" means to the phase
cache (incident habu-incident-master-red-750d7ee7). Route 3
(habu-route-3-the-64078d43) MOVES the boot-prefix order, which is a change to
BP-EACH's body, which the unpackaged file refuses. The precedent lane could dodge
it by not touching the order; route 3 cannot, because the order IS its task.
Measured on master 81d88a3a before any edit, on a minimal reorder diff:

    E-PACKAGE-OWNERSHIP tools/boot-pin.f:126:3: `BP-EACH` defines a changed
    module word outside a package

SHAPE. `package BOOT-PIN`, eight publics, everything else private. The `BP-`
tails are KEPT (seal-4's keep-the-tails ruling): consumers import once with
`using BOOT-PIN` and every call site stays byte-identical, so the packaging never
reaches a consumer's definition lines and never owes an unpackaged consumer its
own packaging - which is the 80-finding cascade the SCHEMA-REG seal measured and
avoided the same way. The single exception is the CLI entry, renamed
BOOT-PIN-MAIN -> BOOT-PIN:MAIN on the CHECK:MAIN precedent, because
BOOT-PIN:BOOT-PIN-MAIN both repeats its owner and trips E-REDUNDANT-FILE-PREFIX
the moment its definition line is touched.

Publics: BP-EACH, BP-ROOT!, BP-ROOT-U, BP-DIGEST-HEX!, BP-HASH-HEX, BP-MATCH?,
BP-DIAG$, MAIN. Three consumers: test/boot-pin-test.f (seven publics -> `using
BOOT-PIN`, per docs/forth.md's two-or-more MUST-rule), test/run-engine-set.f (one
public -> qualified BOOT-PIN:BP-EACH, and the file is already packaged so the
changed body is lint-clean), tools/boot-pin-main.f (BOOT-PIN:MAIN).

ACCEPTANCE, and the gate key is the one that matters. A packaged BP-EACH the
phase key can no longer call would be the false-proxy class reborn, so the
acceptance is an A/B of the list itself, not of the suite: dump what
ENGINE-SET:FILES folds on master and on the candidate and require the two to be
byte-identical, and require tools/boot-pin.f `print` to answer the same digest on
both sides (it must, because the file hashes its listed sources and is not itself
one of them). Plus test/boot-pin-test.f green, the tail slice green, and both
diff lints at zero.

Closes with the route-3 landing, as its first commit.
