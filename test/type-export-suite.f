\ type-export-suite.f — checker-level EXPORT alias suite (CHECKER-EXPORT, dot
\ habu-compiler-pkg-re-688212c1). Run BY THE ENGINE over stdin, exactly like
\ test/type-family-rollback-suite.f (checker words resolve at top-level
\ interpret only):
\     bin/hb < test/type-export-suite.f
\ Covers: cross-package alias fidelity (one scheme, two names, source
\ untouched), private->public promotion, defer + control-flag copy, quotation
\ scheme fidelity, every reject (no package, undefined, private from a closed
\ package, malformed qualification, sealed-system source, primitive source,
\ duplicate/self-export), scope/candidate rollback of the alias rows, and the
\ engine keyword half (real package blocks, dual-name execution, checked
\ callers through both names).
\ A failure prints F<index> + detail; REPORT exits 1 on any fail.

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;
: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want . s" got " type got . cr
   then ;

variable FOUNDF   variable TC
variable P-SYMN   variable P-SYMU   variable P-DEPTH

\ ---------------------------------------------------------------------------
\ 1. cross-package alias fidelity: EXPORT xps:XP-INC into xpd publishes the
\    SAME scheme under xpd:XP-INC; the source record is untouched; a wrong
\    declared sig through the alias still rejects.
\ ---------------------------------------------------------------------------
s" xps" CHECKER-PACKAGE   CHECKER-PUBLIC
s" n -- n" s" XP-INC" CHECKER-USIG-ADD
CHECKER-END-PACKAGE

s" xpd" CHECKER-PACKAGE   CHECKER-PUBLIC
s" xps:XP-INC" CHECKER-EXPORT
CHECKER-END-PACKAGE

s" xpd:XP-INC" CHECKER-FIND-USIG FOUNDF !  FOUNDF @ -1 T=
s" xps:XP-INC" CHECKER-FIND-USIG FOUNDF !  FOUNDF @ -1 T=
s" XPU1 ( n -- n ) xpd:XP-INC" CHECK! -1 T=
s" XPU2 ( n -- n ) xps:XP-INC" CHECK! -1 T=
s" XPU3 ( -- n ) xpd:XP-INC" CHECK! 0 T=
s" XPU4 ( n -- n n ) xpd:XP-INC" CHECK! 0 T=

\ ---------------------------------------------------------------------------
\ 2. private->public promotion: a bare-name source resolves through the open
\    package's private scope and publishes under the public tail.
\ ---------------------------------------------------------------------------
s" xpp" CHECKER-PACKAGE
s" n -- n" s" XP-HID" CHECKER-USIG-ADD
CHECKER-PUBLIC
s" XP-HID" CHECKER-EXPORT
CHECKER-END-PACKAGE
s" xpp:XP-HID" CHECKER-FIND-USIG FOUNDF !  FOUNDF @ -1 T=
s" XPU5 ( n -- n ) xpp:XP-HID" CHECK! -1 T=

\ ---------------------------------------------------------------------------
\ 3. defer flag + control-effect flags ride the alias; a plain alias carries
\    neither.
\ ---------------------------------------------------------------------------
s" xpf" CHECKER-PACKAGE   CHECKER-PUBLIC
s" n -- n" s" XP-DEF" CHECKER-USIG-ADD
s" XP-DEF" CHECKER-DEFER
s" --" s" XP-THR" CHECKER-USIG-ADD
s" XP-THR" CTL-THROW NORET-ADD
CHECKER-END-PACKAGE

s" xpf2" CHECKER-PACKAGE   CHECKER-PUBLIC
s" xpf:XP-DEF" CHECKER-EXPORT
s" xpf:XP-THR" CHECKER-EXPORT
CHECKER-END-PACKAGE
s" xpf2:XP-DEF" CHECKER-FIND-ACTIVE-DEFER FOUNDF !  FOUNDF @ -1 T=
s" xpf2:XP-THR" CTL-FLAGS CTL-THROW T=
s" xpf2:XP-DEF" CTL-FLAGS 0 T=
s" xpf2:XP-THR" CHECKER-FIND-ACTIVE-DEFER FOUNDF !  FOUNDF @ 0 T=

\ ---------------------------------------------------------------------------
\ 4. quotation scheme fidelity: a higher-order sig survives the alias copy;
\    a wrong quotation argument through the alias rejects.
\ ---------------------------------------------------------------------------
s" xpq" CHECKER-PACKAGE   CHECKER-PUBLIC
s" [ n -- n ] n -- n" s" XP-HOF" CHECKER-USIG-ADD
CHECKER-END-PACKAGE
s" xpq2" CHECKER-PACKAGE   CHECKER-PUBLIC
s" xpq:XP-HOF" CHECKER-EXPORT
CHECKER-END-PACKAGE
s" XPU6 ( n -- n ) [: 1 + ;] swap xpq2:XP-HOF" CHECK! -1 T=
s" XPU7 ( n -- n ) [: + ;] swap xpq2:XP-HOF" CHECK! 0 T=

\ ---------------------------------------------------------------------------
\ 5. rejects. Every fail-closed path throws its named code; catch restores
\    the pre-call ( a u ) under the code.
\ ---------------------------------------------------------------------------
\ no open package.
s" XP-INC" ' CHECKER-EXPORT catch TC ! 2drop  TC @ E-EXPORT-NO-PACKAGE T=
s" xpr" CHECKER-PACKAGE   CHECKER-PUBLIC
\ undefined bare + qualified names.
s" XP-NOPE" ' CHECKER-EXPORT catch TC ! 2drop  TC @ E-EXPORT-UNDEFINED T=
s" xps:XP-NOPE" ' CHECKER-EXPORT catch TC ! 2drop  TC @ E-EXPORT-UNDEFINED T=
\ private word from a CLOSED package: qualified lookup is public-only.
s" xps:XP-PRIV" ' CHECKER-EXPORT catch TC ! 2drop  TC @ E-EXPORT-UNDEFINED T=
\ malformed qualification (double colon / edge colon) never resolves.
s" xps:XP:BAD" ' CHECKER-EXPORT catch TC ! 2drop  TC @ E-EXPORT-UNDEFINED T=
s" :XP-INC" ' CHECKER-EXPORT catch TC ! 2drop  TC @ E-EXPORT-UNDEFINED T=
\ re-export FROM a sealed system package (latch is sealed in this process).
s" tfam:list" ' CHECKER-EXPORT catch TC ! 2drop  TC @ E-EXPORT-SEALED T=
s" type:of" ' CHECKER-EXPORT catch TC ! 2drop  TC @ E-EXPORT-SEALED T=
s" match:arm" ' CHECKER-EXPORT catch TC ! 2drop  TC @ E-EXPORT-SEALED T=
\ primitive source: prims may be overloaded; copying one row would narrow.
s" dup" ' CHECKER-EXPORT catch TC ! 2drop  TC @ E-EXPORT-PRIM T=
\ duplicate tail in the current section.
s" xps:XP-INC" CHECKER-EXPORT
s" xps:XP-INC" ' CHECKER-EXPORT catch TC ! 2drop  TC @ $4E T=
CHECKER-END-PACKAGE
\ self-export in the same section is the duplicate case.
s" xpz" CHECKER-PACKAGE   CHECKER-PUBLIC
s" n -- n" s" XP-SELF" CHECKER-USIG-ADD
s" XP-SELF" ' CHECKER-EXPORT catch TC ! 2drop  TC @ $4E T=
CHECKER-END-PACKAGE

\ the private source used by the closed-package reject above really is private:
\ record it AFTER the reject probes so the earlier lookup could not see it, then
\ prove a private record still does not resolve via the public qualifier.
s" xps" CHECKER-PACKAGE
s" n -- n" s" XP-PRIV" CHECKER-USIG-ADD
CHECKER-END-PACKAGE
s" xpr2" CHECKER-PACKAGE   CHECKER-PUBLIC
s" xps:XP-PRIV" ' CHECKER-EXPORT catch TC ! 2drop  TC @ E-EXPORT-UNDEFINED T=
CHECKER-END-PACKAGE

\ ---------------------------------------------------------------------------
\ 6. scope rollback: the alias's sym/effect rows retire with the frame; the
\    watermarks (SYM-N, sym string pool) restore exactly.
\ ---------------------------------------------------------------------------
SYM-N @ P-SYMN !   SYM-STR-U @ P-SYMU !
CHECKER-SCOPE-START
   s" xrb" CHECKER-PACKAGE   CHECKER-PUBLIC
   s" xps:XP-INC" CHECKER-EXPORT
   s" xrb:XP-INC" CHECKER-FIND-USIG FOUNDF !  FOUNDF @ -1 T=
CHECKER-SCOPE-DONE
SYM-N @ P-SYMN @ T=
SYM-STR-U @ P-SYMU @ T=
s" xrb:XP-INC" CHECKER-FIND-USIG FOUNDF !  FOUNDF @ 0 T=

\ ---------------------------------------------------------------------------
\ 7. candidate rollback: alias effect, defer flag, and control flags all
\    retire; the frame depth balances.
\ ---------------------------------------------------------------------------
RBF-DEPTH @ P-DEPTH !
CHECK-CANDIDATE-START
   s" xrb2" CHECKER-PACKAGE   CHECKER-PUBLIC
   s" xpf:XP-DEF" CHECKER-EXPORT
   s" xpf:XP-THR" CHECKER-EXPORT
   s" xrb2:XP-DEF" CHECKER-FIND-ACTIVE-DEFER FOUNDF !  FOUNDF @ -1 T=
   s" xrb2:XP-THR" CTL-FLAGS CTL-THROW T=
0 CHECK-CANDIDATE-DONE drop
RBF-DEPTH @ P-DEPTH @ T=
s" xrb2:XP-DEF" CHECKER-FIND-USIG FOUNDF !  FOUNDF @ 0 T=
s" xrb2:XP-DEF" CHECKER-FIND-ACTIVE-DEFER FOUNDF !  FOUNDF @ 0 T=
s" xrb2:XP-THR" CTL-FLAGS 0 T=

\ ---------------------------------------------------------------------------
\ 8. engine keyword half: real package blocks, EXPORT publishes a callable
\    alias — same xt, dual-name execution, checked callers through each name.
\    (Engine REJECT cases exit the process, so they are pinned as child-run
\    gate fixtures, not here.)
\ ---------------------------------------------------------------------------
package XPE
public
: XPE-DBL ( n -- n ) 2 * ;
;package

package XPF
public
EXPORT XPE:XPE-DBL
;package

7 XPE:XPE-DBL 14 T=
7 XPF:XPE-DBL 14 T=
: XPE-USE1 ( n -- n ) XPF:XPE-DBL ;
: XPE-USE2 ( n -- n ) XPE:XPE-DBL ;
5 XPE-USE1 10 T=
5 XPE-USE2 10 T=

\ private->public promotion through the engine keyword.
package XPG
: XPG-HID ( n -- n ) 3 + ;
public
EXPORT XPG-HID
;package
4 XPG:XPG-HID 7 T=

\ ---------------------------------------------------------------------------
\ report: "ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" type-export-suite: failures" 1 die ;
REPORT
