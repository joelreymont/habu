---
title: Bind a bare tail to the package word the engine runs
status: open
priority: 2
issue-type: task
created-at: "2026-08-04T10:45:10.580784+02:00"
---

Static invariant: a bare token must denote the SAME word when the checker certifies a body and when the engine runs it. The using/global leg of that invariant is now enforced (dot habu-reject-a-bare-1f43a9a6: CHECKER-USED-BIND in src/core/checker.f asks the engine's wordlists through search-wl before a used public may bind). The OPEN-PACKAGE leg is still open when no using is involved.

Reproducer (bin/hb --load, exit 0, prints 41):

  : PVG ( -- n ) 7 ;
  check@
  package PVGP
  0 set-check
  : PVG ( -- n ) 41 ;
  ;package
  set-check
  package PVGP
  public
  : B ( -- n ) PVG ;
  ;package
  PVGP:B FMT:.INT cr

The engine's chain (habu1.f EMIT-FIND: package private, package public, global) binds the package-private PVG and prints 41. The checker's chain missed it - a 0 set-check definition interns no checker symbol - fell through to the global sym and certified B against the global PVG ( -- n ). Certification and execution named different words with no diagnostic.

Why it was not fixed with the using leg: the used-publics leg is reached only after a used public has matched, so one search-wl probe per used-public reference pays for it. The open-package leg would have to probe PKG-PRI-CELL and PKG-PUB-CELL for every token inside a package whose checker package-symbol lookup missed - which is every reference to a global or a primitive from package code. search-wl (habu1.f BSWL) is a linear dictionary scan with no hash index, so that is a hot-path cost the checker cannot absorb as written.

What is needed: a cheap engine-authoritative existence probe. The engine's own LFIND already answers this in O(1) through the HIDX hash index (habu1.f EMIT-FIND FIND-START); exposing that as a primitive (or teaching BSWL to use HIDX with the linear scan as fallback, exactly as LFIND does) makes the probe affordable, and CHECKER-FIND-ACTIVE-SYM can then consult the engine at the package legs too. Engine change, seed-affecting, fixpoint rebuild.

Regression: the case belongs beside the ones added to test/using-test.f under 'an open-package word claims the tail before any used public'.
