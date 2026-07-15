\ kernel-perf-lint-core.f - kernel codegen changes must carry a profile row.
\ Scans a validated framed jj diff artifact: when the diff touches kernel codegen
\ source (lib/ptx/cg*.f, tools/ptx/*-cg.f, src/arch/ptx/emit.f) it must also
\ add at least one valid profile row (or documented WAIVER row) to
\ tools/ptx/perf-rows.tsv, and every added registry row must validate.
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f,
\ tools/lint/text.f, tools/lint/lib.f, tools/lint/diff-frame.f, and
\ tools/ptx/perf-registry.f.

require tools/lint/diff-frame.f

package KERNEL-PERF-LINT
private

16 constant WATCH-MAX
10 constant LF-C

create ONE 1 allot
create WATCH-PATHS WATCH-MAX FS-PATH-CAP * allot
create WATCH-US WATCH-MAX cells allot

variable BAD#
variable ROWS+
variable CUR-REG
variable WATCH#

: REG-PATH$ ( -- ptr u8 n )
   s" tools/ptx/perf-rows.tsv" ;

: OUT ( ptr u8 n -- ) {: a:ptr u:n :}
   1 a u LINT-OUT-WRITE ;

: C ( n -- ) {: c:n :}
   c ONE c!
   ONE 1 OUT ;

: BAD+ ( -- )
   BAD# @ 1+ BAD# ! ;

: WATCHED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" src/arch/ptx/emit.f" LINT-STR= if LINT-TRUE exit then
   a u s" lib/ptx/cg" LINT-STARTS-WITH?
   a u s" .f" HAS-EXT? and if LINT-TRUE exit then
   a u s" tools/ptx/" LINT-STARTS-WITH?
   a u s" -cg.f" HAS-EXT? and ;

: WATCH$ ( n -- ptr u8 n ) {: idx:n :}
   WATCH-PATHS idx FS-PATH-CAP * +
   WATCH-US idx cells + @ ;

: WATCH-KNOWN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup WATCH# @ < while
      dup WATCH$ a u LINT-STR= if drop LINT-TRUE exit then
      1+
   repeat drop LINT-FALSE ;

: WATCH+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u WATCH-KNOWN? if exit then
   WATCH# @ WATCH-MAX < 0= if E-PERF-CAP throw then
   u FS-PATH-CAP > if E-PERF-CAP throw then
   a WATCH-PATHS WATCH# @ FS-PATH-CAP * + u LINT-BMOVE
   u WATCH-US WATCH# @ cells + !
   WATCH# @ 1+ WATCH# ! ;

: REPORT-BAD-ROW ( ptr u8 n -- ) {: a:ptr u:n :}
   BAD+
   s" E-PERF-BAD-ROW " OUT
   REG-PATH$ OUT
   s" : invalid added registry row: " OUT
   a u OUT
   LF-C C ;

: REPORT-MISSING ( ptr u8 n -- ) {: a:ptr u:n :}
   BAD+
   s" E-PERF-ROW-MISSING " OUT
   a u OUT
   s" : kernel codegen changed without a profile/waiver row in " OUT
   REG-PATH$ OUT
   LF-C C ;

: REG-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u PERF:LINE-DATA? 0= if exit then
   a u PERF:LINE-OK? if
      ROWS+ @ 1+ ROWS+ !
   else
      a u REPORT-BAD-ROW
   then ;

: ADDED-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   CUR-REG @ 0= if exit then
   a u REG-LINE ;

: MARK-PATH ( ptr u8 n -- ) {: a:ptr u:n :}
   a u WATCHED? if a u WATCH+ then ;

: NEW! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u MARK-PATH
   a u REG-PATH$ LINT-STR= CUR-REG ! ;

: DROP-EVENT ( ptr u8 n n -- )
   drop 2drop ;

: FILE-EVENT ( ptr u8 n n -- )
   drop NEW! ;

: ADD-EVENT ( ptr u8 n n -- )
   drop ADDED-LINE ;

: PROCESS-EVENT ( ptr u8 n n DIFF:event -- )
   MATCH DIFF:event
      none    OF DROP-EVENT ENDOF
      file    OF FILE-EVENT ENDOF
      hunk    OF DROP-EVENT ENDOF
      add     OF ADD-EVENT ENDOF
      context OF DROP-EVENT ENDOF
      delete  OF DROP-EVENT ENDOF
   ;MATCH ;

: NEXT? ( -- bool )
   DIFF:NEXT? {: a:ptr u:n value:n kind:DIFF:event present:bool :}
   present 0= if LINT-FALSE exit then
   a u value kind PROCESS-EVENT
   LINT-TRUE ;

: MISSING-CHECK ( -- )
   ROWS+ @ 0 > if exit then
   0 begin dup WATCH# @ < while
      dup WATCH$ REPORT-MISSING
      1+
   repeat drop ;

public

: RESET ( -- )
   0 BAD# !
   0 ROWS+ !
   0 WATCH# !
   LINT-FALSE CUR-REG ! ;

: SOURCE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u DIFF:OPEN
   begin NEXT? while repeat ;

: FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT SOURCE ;

: FINISH ( -- )
   MISSING-CHECK
   BAD# @ 0 > if 1 throw then ;

;package
