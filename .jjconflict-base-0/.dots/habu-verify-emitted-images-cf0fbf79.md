---
title: Verify emitted images install the check hook
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T22:50:34.801377+02:00"
---

Full context: agent pkglayout found INSTALL-HOOK (formerly SNAP-INSTALL-HOOK, src/habu/snap-lib.f, TRUSTED entry that arms LOWER-CERT-HOOK:INSTALL and set-check with the fail-closed CHECK-HOOK) has NO caller anywhere in the tree, and this predates the packaging change (searched all file types). Either emitted snapshot images are booting WITHOUT the verify-on-definition hook - a real soundness hole where a typed def in a restored image REPL goes unchecked - or the hook is installed through a path that does not name this word and the trusted entry is dead code to remove. Investigate which with evidence (boot a restored image, define a word with a wrong sig, observe whether it is rejected), then either wire the installation on the image boot path or delete the dead trusted entry and its TRUSTED.md row.
