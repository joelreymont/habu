\ Run through the outer stdin stream, after require has returned:
\ printf 'require test/checker-unit-capture-e2e.f\nCHECKER-UNIT-CAPTURE-TEST:RUN\n' | bin/hb
\ A prepared unit remains installable across the production capture seam.
\ The run writes prepared.checker-unit and its portable prepared.refs section.

require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/codegen.f
require tools/native-file-reference.f

package CAP-WARM
public
NEWTYPE item
;package

package CHECKER-UNIT-CAPTURE-TEST

CAST: ACTION ( n -- [ -- ] )
CAST: EXPORT-XT ( n -- [ -- ptr u8 n ] )
CAST: EXPORT-REFS-XT ( n -- [ [ ptr u8 n -- n ] -- ptr u8 n ] )
CAST: PREPARE-XT ( n -- [ ptr u8 n n n -- n ] )
CAST: PREPARE-REFS-XT ( n -- [ ptr u8 n [ n n -- ptr u8 ] -- n ] )
CAST: INSTALL-XT ( n -- [ n -- ] )
CAST: PUB-USE-XT ( n -- [ [ n -- ] -- ] )
CAST: TFAM-REF-USE-XT
   ( n -- [ [ n -- ptr u8 n bool ] [ ptr u8 n -- n bool ] [ ptr u8 n ptr u8 n ptr u8 n -- bool ] [ n -- ] [ n -- ptr u8 n bool ] [ ptr u8 n -- n bool ] [ n ptr u8 n -- ] -- ] )
CAST: CON-USE-XT
   ( n -- [ [ n -- ptr u8 n bool ] [ n ptr u8 n -- ] -- ] )

: TARGET-XT ( ptr u8 n -- n )
   XREF-FIND dup XREF-FOUND? 0= if E-NUNIT-PROFILE throw then
   XREF-START ;
: BIND-REFS ( -- )
   NFILE-REF:BIND-SEED
   s" test/checker-unit-capture-e2e.f" s" test" 0 NFILE-REF:PUSH-FILE
   NFILE-REF:BEGIN-RANGE
   ['] XREF-REC NFILE-REF:RECORD-AT!
   ['] NFILE-REF:ON-PUBLISHED
      s" CHECKER-REG:PUBLICATION-USE" TARGET-XT PUB-USE-XT execute
   s" TFAM:FILE-IDENTITY$" TARGET-XT
   s" TFAM:FILE-FIND" TARGET-XT NFILE-REF:BIND-FAMILY
   ['] NFILE-REF:FAMILY-KEY$
   ['] NFILE-REF:FAMILY-BY-KEY
   ['] NFILE-REF:FAMILY-KEY-MATCH?
   ['] NFILE-REF:CON-DECLARED
   ['] NFILE-REF:CON-KEY$
   ['] NFILE-REF:CON-BY-KEY
   ['] NFILE-REF:BIND-CON
      s" TFAM:FILE-REF-USE" TARGET-XT TFAM-REF-USE-XT execute
   ['] NFILE-REF:CON-KEY$ ['] NFILE-REF:BIND-CON
      s" CHECKER-REG:FILE-CON-USE" TARGET-XT CON-USE-XT execute
   NUNIT-CAPTURE:BIND-PRODUCER
   NUNIT-CAPTURE:START-PREFIX ;
: CLEAR-REFS ( -- )
   s" CHECKER-REG:PUBLICATION-CLEAR" TARGET-XT ACTION execute
   s" TFAM:FILE-REF-RESET" TARGET-XT ACTION execute
   s" CHECKER-REG:FILE-CON-RESET" TARGET-XT ACTION execute
   NFILE-REF:CLOSE NUNIT-CAPTURE:CLOSE ;

: OWNER ( -- ptr u8 ) data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ ;
: MARK ( -- ) OWNER CHECKER-OWNER-ABI:UNIT-MARK-OFF + CELL-VIEW @ ACTION execute ;
: UNIT$ ( -- ptr u8 n )
   ['] NFILE-REF:REF-ANY
      OWNER CHECKER-OWNER-EXT:UNIT-EXPORT-REFS-OFF + CELL-VIEW @
      EXPORT-REFS-XT execute ;
: RAW-UNIT$ ( -- ptr u8 n )
   OWNER CHECKER-OWNER-ABI:UNIT-EXPORT-OFF + CELL-VIEW @ EXPORT-XT execute ;
: OPEN ( -- )
   OWNER CHECKER-OWNER-ABI:UNIT-PREPARE-BEGIN-OFF + CELL-VIEW @ ACTION execute ;
: PREPARE ( ptr u8 n -- n )
   ['] NFILE-REF:REF>ANY
      OWNER CHECKER-OWNER-EXT:UNIT-PREPARE-REFS-OFF + CELL-VIEW @
      PREPARE-REFS-XT execute ;
: PREPARE-RAW ( ptr u8 n n n -- n )
   OWNER CHECKER-OWNER-ABI:UNIT-PREPARE-OFF + CELL-VIEW @ PREPARE-XT execute ;
: INSTALL ( n -- )
   OWNER CHECKER-OWNER-ABI:UNIT-INSTALL-OFF + CELL-VIEW @ INSTALL-XT execute ;
: CLOSE ( -- )
   OWNER CHECKER-OWNER-ABI:UNIT-PREPARE-END-OFF + CELL-VIEW @ ACTION execute ;

create PATH FS-PATH-CAP allot
create ROOT FS-PATH-CAP allot
variable ROOT-U
$10000 CODEGEN:BUFFER GEN

: SOURCE+ ( ptr u8 n -- ) GEN CODEGEN:APPEND-STRING ;
: BUILD-SOURCE ( -- )
   GEN CODEGEN:RESET
   s" package CAP-WARM public" SOURCE+
   10 GEN CODEGEN:APPEND-BYTE
   700 0 ?do
      s"  : W" SOURCE+ i GEN CODEGEN:APPEND-DECIMAL
      s"  ( -- n ) " SOURCE+ i GEN CODEGEN:APPEND-DECIMAL
      s"  ;" SOURCE+
      10 GEN CODEGEN:APPEND-BYTE
   loop
   s"  : USE-ITEM ( item -- item ) ;" SOURCE+
   10 GEN CODEGEN:APPEND-BYTE
   s"  ;package" SOURCE+
   GEN CODEGEN:CONTENTS evaluate-closed ;

: CAPTURE-SOURCE ( -- ) NUNIT-CAPTURE:START-UNIT MARK BUILD-SOURCE ;

: REF-SECTION ( -- ptr u8 n )
   NUNIT-CAPTURE:FIRST-REC
   NUNIT-CAPTURE:RECORDS$ nip DREC / over +
   NFILE-REF:ENCODE ;

: PREPARE-REFS ( ptr u8 n -- ) {: refs:ptr refu:n :}
   refs refu NUNIT-CAPTURE:FIRST-REC
      NUNIT-CAPTURE:FIRST-CODE NUNIT-CAPTURE:CODE$ nip
      NUNIT-CAPTURE:FIRST-WID NUNIT-CAPTURE:NEW-WIDS
      NUNIT-CAPTURE:FIRST-DATA NUNIT-CAPTURE:DATA$ nip
      NFILE-REF:PREPARE ;

public

: READY ( -- )
   BIND-REFS
   [: CAPTURE-SOURCE ;] NUNIT-CAPTURE:WITH
   NFILE-REF:BEGIN-CAPTURE
   UNIT$ {: a:ptr u:n :}
   REF-SECTION {: refs:ptr refu:n :}
   u $10000 > TTRUE
   s" checker-unit-capture-e2e" HB-TMP-MKDIR {: root:ptr len:n :}
   root ROOT len BYTE-COPY len ROOT-U !
   ROOT ROOT-U @ s" prepared.checker-unit" PATH JOIN-PATH
   PATH swap a u WRITE-ALL
   ROOT ROOT-U @ s" prepared.refs" PATH JOIN-PATH
   PATH swap refs refu WRITE-ALL
   OPEN
   refs refu PREPARE-REFS
   a u PREPARE 0 T=
   CLEAR-REFS ;

: FINISH ( -- )
   [: 0 INSTALL ;] catch 0 T=
   s" CAP-WARM:W0" TEST-EVAL:N 0 T=
   s" CAP-WARM:W699" TEST-EVAL:N 699 T=
   s" CAP-CLIENT ( CAP-WARM:item -- CAP-WARM:item ) CAP-WARM:USE-ITEM"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" CAP-CLIENT-BAD ( n -- CAP-WARM:item ) CAP-WARM:USE-ITEM"
      CHECK-QUIET-CANDIDATE! 0 T=
   CLOSE
   MARK
   s" package CAP-WARM public : TINY ( -- n ) 17 ; ;package" evaluate-closed
   RAW-UNIT$ {: a:ptr u:n :}
   u $10000 < TTRUE
   OPEN
   a u 0 here BYTE-VIEW data-base BYTE-VIEW - PREPARE-RAW INSTALL
   CLOSE
   s" CAP-WARM:TINY" TEST-EVAL:N 17 T=
   T-REPORT
   s" checker prepared artifacts: " type ROOT ROOT-U @ type
   s" /prepared.checker-unit " type ROOT ROOT-U @ type
   s" /prepared.refs" type cr ;

: RUN ( -- )
   T-RESET
   s" prepared unit survives capture and releases its plan" T-LABEL
   READY
   NATIVE-RUNTIME:CAPTURE-PREPARE
   FINISH ;

;package
