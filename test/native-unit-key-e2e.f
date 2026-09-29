\ The package key stops at the actual nested source cursor. Run with
\ bin/hb --load test/native-unit-key-e2e.f; the private tree remains printed.

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require tools/native-source-view.f

package NATIVE-UNIT-KEY-TEST

create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot
create KEY-CUR 64 allot
create KEY-OLD 64 allot
create KEY-LATE 64 allot
variable KEY-SEEN

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;

: PUT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n body:ptr bodyu:n :}
   ROOT$ name nameu PATH JOIN-PATH PATH swap body bodyu WRITE-ALL ;

: UNIT-SOURCE ( -- )
   s" pkg.f"
   S\" package NBR\npublic\n: VALUE ( -- n ) 42 ;\n;package\n" PUT ;

: BAD-SOURCE ( -- )
   s" bad.f" S\" 1 drop\n" PUT ;

: ORIGINAL ( -- )
   s" entry.f" S\" s\" pkg.f\" included\n1 drop\n" PUT ;

: LATE-EDIT ( -- )
   s" entry.f" S\" s\" pkg.f\" included\n2 drop\n" PUT ;

: EARLY-EDIT ( -- )
   s" entry.f" S\" 0 drop\ns\" pkg.f\" included\n2 drop\n" PUT ;

: KEY-LOAD ( ptr u8 n ptr u8 n ptr u8 [ -- ] -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr q :}
   path pathu root rootu SOURCE-ROOT:RELATIVE s" bad.f" STR= if
      path pathu root rootu source [: -2102 throw ;]
      SOURCE-VIEW:LOAD-CALLBACK execute
   else path pathu root rootu SOURCE-ROOT:RELATIVE s" pkg.f" STR= if
      path pathu root rootu source SOURCE-VIEW:START-LOAD {: id:n :}
      SOURCE-VIEW:UNIT-KEY {: key:ptr size:n :}
      size 64 T=
      key KEY-CUR size BYTE-COPY
      1 KEY-SEEN !
      id SOURCE-VIEW:FINISH-LOAD
   else
      path pathu root rootu source q SOURCE-VIEW:LOAD-CALLBACK execute
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
   EARLY-EDIT ONE-LOAD KEY-CUR 64 KEY-LATE 64 STR= 0= TTRUE ;

public

: RUN ( -- )
   T-RESET
   s" package key ignores a later same-file edit and sees an earlier edit" T-LABEL
   s" native-unit-key-e2e" HB-TMP-MKDIR {: path:ptr size:n :}
   path ROOT size BYTE-COPY size ROOT-U !
   UNIT-SOURCE
   BAD-SOURCE
   ROOT$ [: CHECK-KEYS ;] SOURCE-ROOT:WITH
   T-REPORT
   s" native unit key tree: " type ROOT$ type cr ;

;package

NATIVE-UNIT-KEY-TEST:RUN
