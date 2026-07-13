\ kernel-perf-lint-core.f - kernel codegen changes must carry a profile row.
\ Scans a `jj diff --git` artifact: when the diff touches a kernel codegen
\ source (lib/ptx/cg*.f, tools/ptx/*-cg.f, src/arch/ptx/emit.f) it must also
\ add at least one valid profile row (or documented WAIVER row) to
\ tools/ptx/perf-rows.tsv, and every added registry row must validate.
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f,
\ tools/lint/text.f, tools/lint/lib.f, and tools/ptx/perf-registry.f.

require lib/adt/option.f

16 constant KPL-WATCH-MAX
43 constant KPL-PLUS-C
10 constant KPL-LF-C
13 constant KPL-CR-C

create KPL-ONE 1 allot
create KPL-WATCH-PATHS KPL-WATCH-MAX FS-PATH-CAP * allot
create KPL-WATCH-US KPL-WATCH-MAX cells allot

variable KPL-BAD
variable KPL-ROWS+
variable KPL-CUR-REG
variable KPL-WATCH#
variable KPL-SCAN-START

: KPL-REG-PATH$ ( -- ptr u8 n )
   s" tools/ptx/perf-rows.tsv" ;

: KPL-OUT ( ptr u8 n -- ) {: a:ptr u:n :}
   1 a u LINT-OUT-WRITE ;

: KPL-C ( n -- ) {: c:n :}
   c KPL-ONE c!
   KPL-ONE 1 KPL-OUT ;

: KPL-BAD+ ( -- )
   KPL-BAD @ 1+ KPL-BAD ! ;

: KPL-WATCHED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" src/arch/ptx/emit.f" LINT-STR= if LINT-TRUE exit then
   a u s" lib/ptx/cg" LINT-STARTS-WITH?
   a u s" .f" HAS-EXT? and if LINT-TRUE exit then
   a u s" tools/ptx/" LINT-STARTS-WITH?
   a u s" -cg.f" HAS-EXT? and ;

: KPL-WATCH$ ( n -- ptr u8 n ) {: idx:n :}
   KPL-WATCH-PATHS idx FS-PATH-CAP * +
   KPL-WATCH-US idx cells + @ ;

: KPL-WATCH-KNOWN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup KPL-WATCH# @ < while
      dup KPL-WATCH$ a u LINT-STR= if drop LINT-TRUE exit then
      1+
   repeat drop LINT-FALSE ;

: KPL-WATCH+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u KPL-WATCH-KNOWN? if exit then
   KPL-WATCH# @ KPL-WATCH-MAX < 0= if E-PERF-CAP throw then
   u FS-PATH-CAP > if E-PERF-CAP throw then
   a KPL-WATCH-PATHS KPL-WATCH# @ FS-PATH-CAP * + u LINT-BMOVE
   u KPL-WATCH-US KPL-WATCH# @ cells + !
   KPL-WATCH# @ 1+ KPL-WATCH# ! ;

: KPL-REPORT-BAD-ROW ( ptr u8 n -- ) {: a:ptr u:n :}
   KPL-BAD+
   s" E-PERF-BAD-ROW " KPL-OUT
   KPL-REG-PATH$ KPL-OUT
   s" : invalid added registry row: " KPL-OUT
   a u KPL-OUT
   KPL-LF-C KPL-C ;

: KPL-REPORT-MISSING ( ptr u8 n -- ) {: a:ptr u:n :}
   KPL-BAD+
   s" E-PERF-ROW-MISSING " KPL-OUT
   a u KPL-OUT
   s" : kernel codegen changed without a profile/waiver row in " KPL-OUT
   KPL-REG-PATH$ KPL-OUT
   KPL-LF-C KPL-C ;

: KPL-REG-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u PERF:LINE-DATA? 0= if exit then
   a u PERF:LINE-OK? if
      KPL-ROWS+ @ 1+ KPL-ROWS+ !
   else
      a u KPL-REPORT-BAD-ROW
   then ;

: KPL-ADDED-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   KPL-CUR-REG @ 0= if exit then
   a 1+ u 1- KPL-REG-LINE ;

: KPL-MARK-PATH ( ptr u8 n -- ) {: a:ptr u:n :}
   a u KPL-WATCHED? if a u KPL-WATCH+ then ;

: KPL-NEW! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u KPL-MARK-PATH
   a u KPL-REG-PATH$ LINT-STR= KPL-CUR-REG ! ;

: KPL-SET-NEW ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" +++ b/" LINT-STARTS-WITH? if a 6 + u 6 - KPL-NEW! exit then
   a u s" +++ " LINT-STARTS-WITH? if a 4 + u 4 - KPL-NEW! then ;

: KPL-SET-OLD ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" --- a/" LINT-STARTS-WITH? if a 6 + u 6 - KPL-MARK-PATH then ;

: KPL-PROCESS-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" +++ " LINT-STARTS-WITH? if a u KPL-SET-NEW exit then
   a u s" --- " LINT-STARTS-WITH? if a u KPL-SET-OLD exit then
   u 0= if exit then
   a c@ KPL-PLUS-C = if a u KPL-ADDED-LINE then ;

: KPL-LINE-TRIM-CR ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u 0 > if
      a u 1- + c@ KPL-CR-C = if a u 1- exit then
   then
   a u ;

: KPL-PROCESS-SPAN ( ptr u8 n -- )
   KPL-LINE-TRIM-CR KPL-PROCESS-LINE ;

: KERNEL-PERF-LINT-RESET ( -- )
   0 KPL-BAD !
   0 KPL-ROWS+ !
   0 KPL-WATCH# !
   LINT-FALSE KPL-CUR-REG ! ;

: KERNEL-PERF-LINT-SOURCE ( ptr u8 n -- ) {: a:ptr u:n :}
   0 KPL-SCAN-START !
   0 begin dup u < while
      dup a + c@ KPL-LF-C = if
         a KPL-SCAN-START @ + over KPL-SCAN-START @ - KPL-PROCESS-SPAN
         dup 1+ KPL-SCAN-START !
      then
      1+
   repeat drop
   KPL-SCAN-START @ u < if
      a KPL-SCAN-START @ + u KPL-SCAN-START @ - KPL-PROCESS-SPAN
   then ;

: KERNEL-PERF-LINT-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u LINT-SOURCE:LOAD
   LINT-SOURCE:TEXT KERNEL-PERF-LINT-SOURCE ;

: KPL-MISSING-CHECK ( -- )
   KPL-ROWS+ @ 0 > if exit then
   0 begin dup KPL-WATCH# @ < while
      dup KPL-WATCH$ KPL-REPORT-MISSING
      1+
   repeat drop ;

: KERNEL-PERF-LINT-FINISH ( -- )
   KPL-MISSING-CHECK
   KPL-BAD @ 0 > if 1 throw then ;
