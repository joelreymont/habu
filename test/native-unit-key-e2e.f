\ The package key stops at the actual nested source cursor. Run with
\ bin/hb --load test/native-unit-key-e2e.f; the private tree remains printed.

require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require tools/native-source-view.f

package NATIVE-UNIT-KEY-TEST

create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot
create KEY-CUR 64 allot
create KEY-OLD 64 allot
create KEY-LATE 64 allot
variable KEY-SEEN
variable UNIT-ID

PATH-CAP 16 + constant RAW-PATH-CAP
create RAW-PATH RAW-PATH-CAP allot
variable RAW-U
variable CHECK-ACTION

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;

: PUT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n body:ptr bodyu:n :}
   ROOT$ name nameu PATH JOIN-PATH PATH swap body bodyu WRITE-ALL ;

: UNIT-SOURCE ( -- )
   s" pkg.f"
   S\" s\" dep.f\" included\nNATIVE-UNIT-KEY-TEST:CUT\n0 drop\n" PUT ;

: DEP-SOURCE ( -- )
   s" dep.f" S\" 0 drop\n" PUT ;

: DEP-EDIT ( -- )
   s" dep.f" S\" 1 drop\n" PUT ;

\ The selected unit's own bytes after the cut, as a definition appended to it.
: UNIT-EDIT ( -- )
   s" pkg.f"
   S\" s\" dep.f\" included\nNATIVE-UNIT-KEY-TEST:CUT\n0 drop\n1 drop\n" PUT ;

: BAD-SOURCE ( -- )
   s" bad.f" S\" 1 drop\n" PUT ;

: ORIGINAL ( -- )
   s" entry.f" S\" s\" pkg.f\" included\n1 drop\n" PUT ;

\ The raw action must survive a nested load whole, even when its spelling is
\ longer than PATH-CAP and normalization makes the selected path fit.
: PADDED-ORIGINAL ( -- )
   PATH-CAP 2 / 0 ?do
      46 RAW-PATH i 2 * + c!
      47 RAW-PATH i 2 * 1+ + c!
   loop
   s" pkg.f" {: leaf:ptr size:n :}
   leaf RAW-PATH PATH-CAP + size BYTE-COPY
   PATH-CAP size + RAW-U !
   s" entry.f" S\" s\" " PUT
   ROOT$ s" entry.f" PATH JOIN-PATH {: pathu:n :}
   PATH pathu RAW-PATH RAW-U @ APPEND-FILE
   PATH pathu S\" \" included\n1 drop\n" APPEND-FILE ;

: CHECK-RAW-ACTION ( -- )
   SOURCE-UNIT:PARENT-ACTION$ {: mode:n path:ptr size:n :}
   mode SOURCE-UNIT:LOAD-INCLUDE T=
   path size RAW-PATH RAW-U @ T$= ;

: LATE-EDIT ( -- )
   s" entry.f" S\" s\" pkg.f\" included\n2 drop\n" PUT ;

: EARLY-EDIT ( -- )
   s" entry.f" S\" 0 drop\ns\" pkg.f\" included\n2 drop\n" PUT ;

public

: CUT ( -- )
   UNIT-ID @ SOURCE-VIEW:UNIT-KEY {: key:ptr size:n :}
   size 64 T=
   key KEY-CUR size BYTE-COPY
   1 KEY-SEEN ! ;

private

TYPED-VARIABLE KEY-Q [ n n -- ]
variable KEY-U
: KEY-WHOLE ( -- ) 0 KEY-U @ KEY-Q @ execute ;

: KEY-LOAD ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr sourceu:n q :}
   path pathu root rootu SOURCE-ROOT:RELATIVE s" bad.f" STR= if
      path pathu root rootu source sourceu [: 2drop -2102 throw ;]
      SOURCE-VIEW:LOAD-CALLBACK execute
   else path pathu root rootu SOURCE-ROOT:RELATIVE s" pkg.f" STR= if
      CHECK-ACTION @ 0<> if CHECK-RAW-ACTION then
      path pathu root rootu source SOURCE-VIEW:START-LOAD {: id:n :}
      id UNIT-ID !
      KEY-Q @ {: prior :}  KEY-U @ {: prior-u:n :}
      q KEY-Q !  sourceu KEY-U !
      ['] KEY-WHOLE catch {: rc:n :}
      prior KEY-Q !  prior-u KEY-U !
      id SOURCE-VIEW:FINISH-LOAD
      rc 0<> if rc throw then
   else
      path pathu root rootu source sourceu q SOURCE-VIEW:LOAD-CALLBACK execute
   then then ;

: BAD-LOAD ( -- ) s" bad.f" included ;

: ONE-LOAD ( -- )
   0 KEY-SEEN !
   SOURCE-VIEW:OPEN
   ROOT$ s" entry.f" PATH JOIN-PATH PATH swap SOURCE-VIEW:COLLECT
   ROOT$ s" bad.f" PATH JOIN-PATH PATH swap SOURCE-VIEW:COLLECT
   SOURCE-VIEW:USE
   ['] KEY-LOAD SOURCE-UNIT:USE
   ['] BAD-LOAD catch -2102 T=
   s" entry.f" included
   KEY-SEEN @ 1 T=
   SOURCE-VIEW:CLOSE ;

: CHECK-KEYS ( -- )
   ORIGINAL ONE-LOAD KEY-CUR KEY-OLD 64 BYTE-COPY
   LATE-EDIT ONE-LOAD KEY-CUR 64 KEY-OLD 64 T$=
   KEY-CUR KEY-LATE 64 BYTE-COPY
   EARLY-EDIT ONE-LOAD KEY-CUR 64 KEY-LATE 64 STR= 0= TTRUE
   ORIGINAL DEP-EDIT ONE-LOAD KEY-CUR 64 KEY-OLD 64 STR= 0= TTRUE
   DEP-SOURCE UNIT-EDIT ONE-LOAD KEY-CUR 64 KEY-OLD 64 STR= 0= TTRUE
   1 CHECK-ACTION !
   PADDED-ORIGINAL ONE-LOAD
   0 CHECK-ACTION ! ;

public

: RUN ( -- )
   T-RESET
   s" package key sees its own, a dependency's and an earlier loader edit, not a later one" T-LABEL
   s" native-unit-key-e2e" HB-TMP-MKDIR {: path:ptr size:n :}
   path ROOT size BYTE-COPY size ROOT-U !
   UNIT-SOURCE
   DEP-SOURCE
   BAD-SOURCE
   ROOT$ [: CHECK-KEYS ;] SOURCE-ROOT:WITH
   T-REPORT
   s" native unit key tree: " type ROOT$ type cr ;

;package

NATIVE-UNIT-KEY-TEST:RUN
