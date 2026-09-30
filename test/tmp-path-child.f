\ tmp-path-child.f - one TMP-PATH call, for test/tmp-path-test.f.
\
\ Run: HB_TMP=<root> bin/hb --load test/tmp-path-child.f -- <case>
\ Types the path TMP-PATH answers, or dies as TMP-PATH does. The case is the
\ name's length: `exact` fills PATH-CAP with the root and its slash, `over` is
\ one byte more, `negative` is -1 and `huge` is the maximum cell.

require lib/string.f
require lib/memory.f

package TMP-PATH-CHILD

$61 constant LOWER-A

create NAME PATH-CAP allot

: NAME-FILL ( -- )
   PATH-CAP 0 ?do LOWER-A NAME i + c! loop ;

: ROOM ( -- n )
   PATH-CAP s" HB_TMP" GETENV nip - 1 - ;

: NAME-U ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u s" exact" STR= if ROOM exit then
   a u s" over" STR= if ROOM 1 + exit then
   a u s" negative" STR= if -1 exit then
   a u s" huge" STR= if MEM-MAX-N exit then
   s" tmp-path-child: unknown case" 64 die ;

public

: RUN ( -- )
   NAME-FILL
   NAME 0 SCRIPT-ARGV$ NAME-U TMP-PATH type cr ;

;package

TMP-PATH-CHILD:RUN
