---
title: "Take a word's type from its definition"
status: open
priority: 2
issue-type: task
created-at: "2026-10-08T10:58:28.825097+02:00"
---

Problem: under Joel's ruling below, a kernel word that checked code does not call needs no type, sits in the system package's private section and is stripped from the delivered hb. Such words are still to be found and moved there.
Ruling (Joel, 2026-10-08): the definition is the source of a word's type. The kernel is the words compiled before the type checker runs; it is untyped and implicitly trusted. A kernel word that checked code calls carries a trusted type declared at its definition; every other kernel word needs no type, sits in the system package's private section and is stripped from the delivered hb. No census or certification ceremony; integration tests cover the kernel.
Acceptance: kernel words checked code does not call carry no type and are private to their package, so user source cannot name them. No word user source can call writes the effect store or states a row, so the handling of a row stated after the checker's table goes: PE-CLOSE-SYM's PREFIX-SYM? exit and HIDX forget, PE-ROW-EFF? and its use in CHECKER-CERT-DUP?, and src/core/check-walk.f's TOP-GUEST and TOP-RELEASE with their TG- variables, unless a build shows another declaration needs them.
Files: the prefix files src/habu/boot-files.f FILL lists.
Verify: a user program naming such a word is rejected as undefined; full suite on the built hb.
Depends: none. Ownership: unassigned. Claim: unassigned.
