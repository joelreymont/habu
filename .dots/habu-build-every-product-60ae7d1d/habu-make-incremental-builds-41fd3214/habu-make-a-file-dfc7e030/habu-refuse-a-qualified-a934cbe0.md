---
title: Refuse a qualified private type as a qualified private word is refused
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T00:21:40.126103+02:00"
---

Problem: a bare type name is found as a bare word is, and a qualified name selects a package's public wordlist (card section 1), so HB:HELPER is E-UNDEFINED. But src/core/type-family.f TFAM-QUAL-RESOLVE (:4405) accepts a qualified private family when the qualifier is the open package (:4411, 'qa qu pa pu CORE-STR=CI IF id RES-TRUE EXIT THEN \ own private rows'): inside package PK with private DEFTYPE VREG, ( pk:vreg -- n ) is accepted: 'package PK private DEFTYPE VREG : RD ( pk:vreg -- pk:vreg ) ; ;package' then a line typed after it loads under the installed hb, rc 0. Acceptance: a qualified type resolves only to a public family, as a qualified word does; a source signature naming its own package's private type qualified is refused with the unknown-type error; facts mode (exact identities) is unaffected; callers in the tree that spell their own private type qualified are fixed. Depends: none. Claim: unassigned, owner jerry.
