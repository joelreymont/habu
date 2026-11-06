---
title: Refuse a qualified private type as a qualified private word is refused
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T00:21:40.126103+02:00"
---

Problem: DECISIONS 'type names' (2026-11-06): a bare type name is found as a bare word is. Card section 1: a qualified name selects a package's public wordlist, so HB:HELPER is E-UNDEFINED. But src/core/type-family.f TFAM-QUAL-RESOLVE (:4894) accepts a qualified private family when the qualifier is the open package (its line 'qa qu pa pu CORE-STR=CI IF id RES-TRUE EXIT THEN \ own private rows'): inside package PK with private DEFTYPE VREG, ( pk:vreg -- n ) is accepted (rc 0, /tmp/jrp.jP/src*.f, found by the C2c-a root-cause hunt). Acceptance: a qualified type resolves only to a public family, as a qualified word does; a source signature naming its own package's private type qualified is refused with the unknown-type error; facts mode (exact identities, C2c-a's fix) is unaffected. Files: src/core/type-family.f TFAM-QUAL-RESOLVE; callers in the tree that spell their own private type qualified (rg). Verify: a rejected-program case in the suite's checker tests; candidate build; full suite. Depends: habu-c2c-a-facts-23c32585 (its facts-mode resolver must not route through TFAM-QUAL-RESOLVE). May join habu-drop-the-public-bed8ef8e (the same resolver). Ownership: jerry. Claim: unassigned.
