\ Run through the outer stdin stream, after require has returned:
\ printf 'require test/checker-unit-capture-e2e.f\nCHECKER-UNIT-CAPTURE-TEST:RUN\n' | bin/hb
\ A prepared unit remains installable across the production capture seam.

require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f

package CHECKER-UNIT-CAPTURE-TEST

CAST: ACTION ( n -- [ -- ] )
CAST: EXPORT-XT ( n -- [ -- ptr u8 n ] )
CAST: PREPARE-XT ( n -- [ ptr u8 n n n -- n ] )
CAST: INSTALL-XT ( n -- [ n -- ] )

: OWNER ( -- ptr u8 ) data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ ;
: MARK ( -- ) OWNER CHECKER-OWNER-ABI:UNIT-MARK-OFF + CELL-VIEW @ ACTION execute ;
: UNIT$ ( -- ptr u8 n )
   OWNER CHECKER-OWNER-ABI:UNIT-EXPORT-OFF + CELL-VIEW @ EXPORT-XT execute ;
: OPEN ( -- )
   OWNER CHECKER-OWNER-ABI:UNIT-PREPARE-BEGIN-OFF + CELL-VIEW @ ACTION execute ;
: PREPARE ( ptr u8 n n n -- n )
   OWNER CHECKER-OWNER-ABI:UNIT-PREPARE-OFF + CELL-VIEW @ PREPARE-XT execute ;
: INSTALL ( n -- )
   OWNER CHECKER-OWNER-ABI:UNIT-INSTALL-OFF + CELL-VIEW @ INSTALL-XT execute ;
: CLOSE ( -- )
   OWNER CHECKER-OWNER-ABI:UNIT-PREPARE-END-OFF + CELL-VIEW @ ACTION execute ;

create PATH FS-PATH-CAP allot
create ROOT FS-PATH-CAP allot
variable ROOT-U

public

: READY ( -- )
   MARK UNIT$ {: a:ptr u:n :}
   s" checker-unit-capture-e2e" HB-TMP-MKDIR {: root:ptr len:n :}
   root ROOT len BYTE-COPY len ROOT-U !
   ROOT ROOT-U @ s" prepared.checker-unit" PATH JOIN-PATH
   PATH swap a u WRITE-ALL
   OPEN
   a u 0 here BYTE-VIEW data-base BYTE-VIEW - PREPARE 0 T= ;

: FINISH ( -- )
   [: 0 INSTALL ;] catch 0 T=
   CLOSE
   OPEN CLOSE
   T-REPORT
   s" checker prepared artifact: " type ROOT ROOT-U @ type
   s" /prepared.checker-unit" type cr ;

: RUN ( -- )
   T-RESET
   s" prepared unit survives capture and releases its plan" T-LABEL
   READY
   NATIVE-RUNTIME:CAPTURE-PREPARE
   FINISH ;

;package
