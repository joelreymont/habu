\ perf-map-test.f - PERF-MAP:LINE$ against `perf script` output captured verbatim
\ (perf 7.2.5-1, aarch64) from
\   perf record -g -o perf.data -- bin/hb --load <program>; perf script -i perf.data
\ A header's pid and period also read as hex, so only a call-chain line's first
\ column may be mapped. A captured address belongs to another binary, so the
\ resolution case builds its line from a word defined here.
require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require tools/perf-map-core.f

package PERF-MAP-TEST
private

\ The word the resolution case looks for. Package-qualified on purpose: the
\ rewritten line must carry the qualifier, not the bare name.
public
: MARKER ( n -- n ) 1+ ;
private

create HEXBUF 32 allot
variable HV
variable HN
variable ACC

: HEX-CHAR ( n -- n ) {: d:n :}
   d 10 < if [char] 0 d + exit then
   [char] a d 10 - + ;

: HEX+ ( n -- )
   HV !
   0 HN !
   HV @ 0= if [char] 0 SB-APPEND-C exit then
   begin HV @ 0 > while
      HV @ 16 mod HEX-CHAR  HEXBUF HN @ + c!
      HN @ 1+ HN !
      HV @ 16 / HV !
   repeat
   begin HN @ 0 > while
      HN @ 1- HN !
      HEXBUF HN @ + c@ SB-APPEND-C
   repeat ;

: FIND-REC ( ptr u8 n -- n )   \ the last live record with this name, -1 when none
   {: a:ptr u:n :}
   -1 ACC !
   ndict@ 0 ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-RETIRED? 0= if
         rec XREF-NAME$ a u STR= if i ACC ! then
      then
   loop
   ACC @ ;

\ ---- the captured lines -------------------------------------------------------
: HEADER$ ( -- ptr u8 n )
   s" H 1880406 1097390.792320:     284641  apple_blizzard_pmu/cycles/Pu: " ;

: FOREIGN-FRAME$ ( -- ptr u8 n )
   S\" \tfffeffa417e0 __ctype_init+0x0 (/usr/lib/libc.so.6)" ;

: ENGINE-FRAME$ ( -- ptr u8 n )
   S\" \t          401680 [unknown] (/tmp/hz-prof/H)" ;

\ ---- cases --------------------------------------------------------------------
\ An unindented header carries no address column, so it comes back byte for byte
\ - the pid and the period included.
: T-HEADER-UNTOUCHED ( -- )
   HEADER$ PERF-MAP:LINE$ HEADER$ STR= TTRUE ;

\ Every byte of a call-chain line survives, not only its mapped address.
: T-FRAME-KEPT ( -- )
   FOREIGN-FRAME$ PERF-MAP:LINE$ {: a:ptr u:n :}
   a u s" __ctype_init+0x0" CONTAINS? TTRUE
   a u s" (/usr/lib/libc.so.6)" CONTAINS? TTRUE ;

\ An address this dictionary does not own stays hex, and its line keeps its tail.
: T-FOREIGN-STAYS-HEX ( -- )
   FOREIGN-FRAME$ PERF-MAP:LINE$ {: a:ptr u:n :}
   a u s" fffeffa417e0" CONTAINS? TTRUE ;

: T-ENGINE-FRAME-KEPT ( -- )
   ENGINE-FRAME$ PERF-MAP:LINE$ {: a:ptr u:n :}
   a u s" [unknown]" CONTAINS? TTRUE
   a u s" (/tmp/hz-prof/H)" CONTAINS? TTRUE ;

\ A live address resolves to its package-qualified word, and the rest of the
\ line - symbol column and dso - is left alone.
: LIVE-FRAME$ ( -- ptr u8 n )
   s" MARKER" FIND-REC {: idx:n :}
   idx 0 < if s" perf-map-test: MARKER is not in the dictionary" 76 die then
   SB-RESET
   S\" \t          " SB-APPEND
   idx XREF-REC XREF-START HEX+
   s"  [unknown] (bin/hb)" SB-APPEND
   SB$ ;

: T-LIVE-RESOLVES ( -- )
   LIVE-FRAME$ PERF-MAP:LINE$ {: a:ptr u:n :}
   a u s" PERF-MAP-TEST:MARKER" CONTAINS? TTRUE
   a u s" (bin/hb)" CONTAINS? TTRUE ;

\ The header pid is a seven-digit run that is also valid hex.
: T-PID-NOT-AN-ADDRESS ( -- )
   HEADER$ PERF-MAP:LINE$ {: a:ptr u:n :}
   a u s" 1880406" CONTAINS? TTRUE ;

: RUN ( -- )
   PERF-MAP:ARM
   T-HEADER-UNTOUCHED
   T-FRAME-KEPT
   T-FOREIGN-STAYS-HEX
   T-ENGINE-FRAME-KEPT
   T-LIVE-RESOLVES
   T-PID-NOT-AN-ADDRESS
   T-REPORT ;

public
: MAIN ( -- ) RUN ;
;package

PERF-MAP-TEST:MAIN
