---
title: Reject a bare tail that a using and a global both claim
status: open
priority: 2
issue-type: task
created-at: "2026-08-04T09:52:52.907544+02:00"
---

Static invariant: a bare token must denote the SAME word when the checker certifies a body and when the engine runs it. Where a `using`-imported package public and a GLOBAL share a tail, docs/forth.md says the reference site is a hard error (E-USING-SHADOW-GLOBAL, checker code 7141) and that 'the checker resolves a bare tail through the used publics identically to the runtime ... so certification and execution always agree'. Neither holds today.

Reproducer (bin/hb --load, checker active - a plain wrong effect on the same path IS reported):

  require lib/fmt.f
  package SHDW-P
  public
  : FRESH ( n -- n )
     1+ ;
  ;package

  package SHDW-C
  using SHDW-P
  public
  : QUAL ( -- )
     41 SHDW-P:FRESH FMT:.INT cr ;
  : BARE ( -- )
     41 FRESH FMT:.INT cr ;
  ;using
  ;package
  SHDW-C:QUAL
  SHDW-C:BARE

Prints 42 then 3, exit 0, no diagnostic, and leaves one item on the stack. FRESH is a real global (`: FRESH ( -- n )` in src/core/checker.f:1787). What happened: the CHECKER bound the used public SHDW-P:FRESH ( n -- n ) and certified BARE as ( -- ); the ENGINE bound the global FRESH ( -- n ) and executed it. Certification and execution named different words, and E-USING-SHADOW-GLOBAL never fired.

Found while factoring tools/codegen-time.f out of the two codegen harnesses (dot habu-share-the-timing-2eda3703): the shared package first exported a word named FRESH, and every call site through `using CODEGEN-TIME` silently ran the checker's global FRESH instead. The accumulator was never cleared and every measurement was wrong, with no error anywhere. Worked around by renaming to ACC-CLEAR after probing each proposed public tail against the global dictionary by hand (s" NAME" XREF-FIND XREF-FOUND?), which is the manual discipline this rule exists to replace.

Owner: checker semantics - the using-resolution path in src/core/checker.f, plus whatever the engine's bare-tail lookup does under an open using. Fix: make the reference site reject with E-USING-SHADOW-GLOBAL (7141) naming both candidates and their arities, per docs/forth.md. Regression: the reproducer above as a negative fixture, plus a positive one showing the qualified form still certifies and runs.
