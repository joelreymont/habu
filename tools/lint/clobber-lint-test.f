\ clobber-lint-test.f - focused regression tests for clobber-lint.

require tools/lint/clobber-lint-core.f

package CLOBBER

: CLT-ASSERT ( bool -- )
   0= if s" clobber-lint-test failed" 1 die then ;

PTR-VARIABLE FIX-A   variable FIX-U

: FIX$ ( -- ptr u8 n )
   FIX-A @ FIX-U @ ;

: FIXTURE ( [ ptr u8 n -- ] -- ) {: visit :}
   FIX$ visit execute ;

: CLT-BAD# ( ptr u8 n -- n )
   FIX-U ! FIX-A !
   [: FIXTURE ;] ANALYZE  BAD @ ;

: CLT-FIXTURE-BAD# ( -- n )
   s" tools/lint/clobber-sys-x8-fixture.f" CLT-BAD# ;

: CLT-SYS-CLOBBERS-X8 ( -- )
   CLT-FIXTURE-BAD# 21 = CLT-ASSERT ;

: CLT-QUALIFIED-LABELS ( -- )
   s" LFLUSH" START-L? CLT-ASSERT
   s" RELOC-EMIT:LCALLS" START-L? CLT-ASSERT
   s" A:B:LTAIL" START-L? CLT-ASSERT
   s" LHEAD:OPEN" START-L? 0= CLT-ASSERT
   s" PROT:OPEN" START-L? 0= CLT-ASSERT
   s" PROT:" START-L? 0= CLT-ASSERT
   s" CP" START-L? 0= CLT-ASSERT ;

: CLT-LABEL-ACCESSORS ( -- )
   s" @" LABEL-ACCESS? 0= CLT-ASSERT
   s" LABEL@" LABEL-ACCESS? CLT-ASSERT
   s" OWNER:LABEL@" LABEL-ACCESS? CLT-ASSERT
   s" LABEL" LABEL-ACCESS? 0= CLT-ASSERT ;

: CLT-WRAP-BAD# ( -- n )
   s" tools/lint/clobber-wrap-fixture.f" CLT-BAD# ;

: CLT-WRAPPED-CALLS ( -- )
   CLT-WRAP-BAD# 3 = CLT-ASSERT               \ two guards and a stale global-lookup input
   CLOBBER-CENSUS:COUNTS {: routines:n calls:n :}
   routines 2 = CLT-ASSERT
   calls 6 = CLT-ASSERT ;

: CLT-WRAP-UNMODELED ( -- )                   \ an unmodeled :CALL shape fails closed
   [: s" FFI-GUARD:CALL" 9 7 CLOBBER-WRAP:MASK drop ;] catch
      E-CLOBBER-WRAP-UNRESOLVED = CLT-ASSERT
   [: s" FFI-GUARD:CALL" 9 7 CLOBBER-WRAP:READS drop ;] catch
      E-CLOBBER-WRAP-UNRESOLVED = CLT-ASSERT
   [: s" FFI-GUARD:CALL" 9 7 CLOBBER-WRAP:RETURNS drop ;] catch
      E-CLOBBER-WRAP-UNRESOLVED = CLT-ASSERT ;

: CLT-LITERAL-BOUNDARIES ( -- )
   s" tools/lint/clobber-literal-fixture.f" CLT-BAD# 1 = CLT-ASSERT
   CLOBBER-CENSUS:COUNTS {: routines:n calls:n :}
   routines 0 = CLT-ASSERT
   calls 2 = CLT-ASSERT ;

: CLT-LABEL-JOINS ( -- )
   s" tools/lint/clobber-join-dead.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-join-exit.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-join-branch.f" CLT-BAD# 1 = CLT-ASSERT
   s" tools/lint/clobber-join-loop.f" CLT-BAD# 1 = CLT-ASSERT ;

\ The host word's file comes first, as a scanned file defining it would.
: HOST-FILES ( [ ptr u8 n -- ] -- ) {: visit :}
   s" tools/lint/clobber-host-def.f" visit execute
   s" tools/lint/clobber-host-call.f" visit execute ;

: EPI1-FILES ( [ ptr u8 n -- ] -- ) {: visit :}
   s" tools/lint/clobber-host-epi1-def.f" visit execute
   s" tools/lint/clobber-host-epi1-call.f" visit execute ;

: CLT-HOST-CALLS ( -- )
   [: HOST-FILES ;] ANALYZE  BAD @ 0 = CLT-ASSERT
   s" tools/lint/clobber-host-run.f" CLT-BAD# 1 = CLT-ASSERT
   s" tools/lint/clobber-host-run2.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-host-shadow.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-host-redef.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-host-epi2.f" CLT-BAD# 0 = CLT-ASSERT
   [: EPI1-FILES ;] ANALYZE  BAD @ 0 = CLT-ASSERT
   s" tools/lint/clobber-host-raw.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-host-casal.f" CLT-BAD# 1 = CLT-ASSERT
   s" tools/lint/clobber-host-arm.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-host-emitw-local.f" CLT-BAD# 1 = CLT-ASSERT
   s" tools/lint/clobber-host-emitw-qualified.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-host-hidden.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-local-keyword.f" CLT-BAD# 1 = CLT-ASSERT
   s" tools/lint/clobber-local-case.f" CLT-BAD# 0 = CLT-ASSERT ;

: CLT-OPERANDS ( -- )
   s" tools/lint/clobber-csetm-routine.f" CLT-BAD# 1 = CLT-ASSERT
   s" tools/lint/clobber-csetm-site.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-local-run.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-local-literal.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-bind-call.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-save.f" CLT-BAD# 1 = CLT-ASSERT ;

: CLT-DATA-AND-BARRIERS ( -- )
   s" tools/lint/clobber-dead-data.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-barrier.f" CLT-BAD# 1 = CLT-ASSERT
   s" tools/lint/clobber-data-fall.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-barrier-fall.f" CLT-BAD# 0 = CLT-ASSERT ;

\ A path end in a host arm ends the path only within that arm's block.
: CLT-PATH-ENDS ( -- )
   s" tools/lint/clobber-arm-if.f" CLT-BAD# 1 = CLT-ASSERT
   s" tools/lint/clobber-arm-else.f" CLT-BAD# 1 = CLT-ASSERT
   s" tools/lint/clobber-arm-loop.f" CLT-BAD# 1 = CLT-ASSERT
   s" tools/lint/clobber-arm-fall.f" CLT-BAD# 1 = CLT-ASSERT
   s" tools/lint/clobber-dead-leak.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-hidden-ret.f" CLT-BAD# 0 = CLT-ASSERT ;

\ Host control words join paths where Forth's control-flow stack resolves them.
: CLT-HOST-FLOW ( -- )
   s" tools/lint/clobber-both.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-doret.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-untilret.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-whilepre.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-endedpre.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-bothfall.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-dofall.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-ifelse.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-fallnext.f" CLT-BAD# 1 = CLT-ASSERT
   s" tools/lint/clobber-preif.f" CLT-BAD# 1 = CLT-ASSERT
   s" tools/lint/clobber-lbllocal.f" CLT-BAD# 1 = CLT-ASSERT
   s" tools/lint/clobber-caseof.f" CLT-BAD# 1 = CLT-ASSERT
   s" tools/lint/clobber-casedef.f" CLT-BAD# 1 = CLT-ASSERT
   s" tools/lint/clobber-leave.f" CLT-BAD# 1 = CLT-ASSERT
   s" tools/lint/clobber-whileret.f" CLT-BAD# 1 = CLT-ASSERT
   s" tools/lint/clobber-nested.f" CLT-BAD# 1 = CLT-ASSERT
   s" tools/lint/clobber-matchelse.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-matchfall.f" CLT-BAD# 1 = CLT-ASSERT
   s" tools/lint/clobber-hidflow.f" CLT-BAD# 1 = CLT-ASSERT
   s" tools/lint/clobber-hostcond.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-xelse2.f" CLT-BAD# 0 = CLT-ASSERT
   s" tools/lint/clobber-xof2.f" CLT-BAD# 0 = CLT-ASSERT ;

CLT-SYS-CLOBBERS-X8
CLT-LABEL-ACCESSORS
CLT-QUALIFIED-LABELS
CLT-WRAP-UNMODELED
CLT-WRAPPED-CALLS
CLT-LITERAL-BOUNDARIES
CLT-LABEL-JOINS
CLT-HOST-CALLS
CLT-OPERANDS
CLT-DATA-AND-BARRIERS
CLT-PATH-ENDS
CLT-HOST-FLOW
s" clobber-lint-test: ok" type NL
;package
