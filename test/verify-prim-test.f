\ verify-prim-test.f - verify-source PRIM:/PPRIM: row-closer parity with execution
\ (dot habu-close-verify-src).
\ Run: bin/hb --load test/verify-prim-test.f
\
\ Every case calls production VERIFY:SOURCE-BUF. Hidden definers remain undefined;
\ live row closers expose the following top-level definition. Process-exit cases
\ run in a child because the malformed-source boundary uses die.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-fork.f
require lib/process-argv.f
require lib/process-env.f
require lib/codesign.f
require lib/test/outcome.f
require src/habu/verify-source.f
require tools/build-fixpoint.f

package VERIFY-PRIM-TEST
using BUILD-FIXPOINT

$2000 constant DIAG-MAX
$22 constant QUOTE-C
$5C constant ESCAPE-C
$0A constant LF-C
$20 constant SPACE-C

create DIAG-BUF DIAG-MAX allot

: BUF-CLEAR ( -- )
   SB-RESET ;

: ADD ( ptr u8 n -- )
   SB-APPEND ;

: ADD-C ( n -- )
   SB-APPEND-C ;

: ADD-QUOTE ( -- ) QUOTE-C ADD-C ;
: ADD-LF ( -- ) LF-C ADD-C ;

: ADD-STRING-OPEN ( -- )
   s" s" ADD
   ADD-QUOTE
   SPACE-C ADD-C ;

: ADD-ESC-OPEN ( -- )
   s" s" ADD
   ESCAPE-C ADD-C
   ADD-QUOTE
   SPACE-C ADD-C ;

: ADD-ESC-QUOTE ( -- )
   ESCAPE-C ADD-C
   ADD-QUOTE ;

: BUILD$ ( -- ptr u8 n )
   SB$ ;

: ADD-REF ( ptr u8 n -- )
   ADD-LF
   s" : U ( -- ptr a ) " ADD
   ADD
   s"  ;" ADD ;

: VERIFY-RUN ( -- )
   BUILD$ VERIFY:SOURCE-BUF ;

: VERIFY-BUF ( -- n )
   DIAG-BUF DIAG-MAX DIAG-BUFFER!
   [: VERIFY-RUN ;] catch
   DIAG-BUFFER-OFF ;

: EXPECT-HIDDEN ( ptr u8 n -- )
   ADD-REF
   VERIFY-BUF 70 T= ;

: PRIM-STRING ( -- )
   s" a spaced PRIM; inside a body s-quote string is content, not the closer" T-LABEL
   BUF-CLEAR s" PRIM: FOO " ADD ADD-STRING-OPEN
   s" q PRIM; create SL1 y" ADD ADD-QUOTE s"  2drop PRIM;" ADD
   s" SL1" EXPECT-HIDDEN ;

: PRIM-LINE-COMMENT ( -- )
   s" a spaced PRIM; inside a backslash line comment is content, not the closer" T-LABEL
   BUF-CLEAR s" PRIM: FOO " ADD s" \ q PRIM; create SL2 zz" ADD ADD-LF
   s"  PE-N PRIM;" ADD  s" SL2" EXPECT-HIDDEN ;

: PRIM-MULTI-COMMENT ( -- )
   s" a spaced PRIM; inside a paren comment is content, not the closer" T-LABEL
   BUF-CLEAR s" PRIM: FOO ( q PRIM; create SL3 zz ) PE-N PRIM;" ADD
   s" SL3" EXPECT-HIDDEN ;

: PRIM-ESCAPED ( -- )
   s" a spaced PRIM; inside an escaped string is content, not the closer" T-LABEL
   BUF-CLEAR s" PRIM: FOO " ADD ADD-ESC-OPEN
   s" q " ADD ADD-ESC-QUOTE s"  PRIM; create SL5 z" ADD ADD-QUOTE
   s"  2drop PRIM;" ADD  s" SL5" EXPECT-HIDDEN ;

: TEST-PRIM-CONTENT ( -- )
   PRIM-STRING PRIM-ESCAPED PRIM-LINE-COMMENT PRIM-MULTI-COMMENT ;

: BODY-VARIABLE ( -- )
   s" a variable definer that is body data does not register a top-level cell" T-LABEL
   BUF-CLEAR s" PRIM: FOO " ADD ADD-STRING-OPEN
   s" q PRIM; variable SL4 y" ADD ADD-QUOTE s"  PRIM;" ADD
   s" SL4" EXPECT-HIDDEN ;

: BODY-COLON ( -- )
   s" a colon definer that is body data is consumed, so no top-level def rejects" T-LABEL
   BUF-CLEAR s" PRIM: FOO " ADD ADD-STRING-OPEN
   s" q PRIM; : W NOSUCH ; z" ADD ADD-QUOTE
   s"  PRIM;" ADD  VERIFY-BUF 0 T= ;

: TOP-STRING ( -- )
   s" role markers inside a top-level string are data and are never scanned" T-LABEL
   BUF-CLEAR ADD-STRING-OPEN s" PRIM: FAKE create SL7 " ADD ADD-QUOTE
   s"  drop drop" ADD  s" SL7" EXPECT-HIDDEN ;

: TEST-ROLES ( -- )
   BODY-VARIABLE BODY-COLON TOP-STRING ;

: DUP-CLOSERS ( -- )
   s" duplicate and wrong-kind closer tokens inside a string are all data" T-LABEL
   BUF-CLEAR s" PRIM: FOO " ADD ADD-STRING-OPEN
   s" PRIM; PRIM; PPRIM; create SL8 z" ADD ADD-QUOTE s"  PRIM;" ADD
   s" SL8" EXPECT-HIDDEN ;

: PRIM-ATTACHED ( -- )
   s" an attached-quote token PRIM;-quote is distinct and does not close the row" T-LABEL
   BUF-CLEAR s" PRIM: FOO " ADD ADD-STRING-OPEN s" a PRIM;" ADD ADD-QUOTE
   s"  2drop create SL6 zz PRIM;" ADD  s" SL6" EXPECT-HIDDEN ;

: PPRIM-ATTACHED ( -- )
   s" an attached-quote token PPRIM;-quote is distinct and does not close the row" T-LABEL
   BUF-CLEAR s" PPRIM: PKG FOO " ADD ADD-STRING-OPEN s" a PPRIM;" ADD ADD-QUOTE
   s"  2drop create PSL6 zz PPRIM;" ADD  s" PSL6" EXPECT-HIDDEN ;

: PRIM-RAW-ATTACHED ( -- )
   s" a raw PRIM;X body token does not close before the live PRIM;" T-LABEL
   BUF-CLEAR s" PRIM: FOO PRIM;X create RSL6 zz PRIM;" ADD
   s" RSL6" EXPECT-HIDDEN ;

: PPRIM-RAW-ATTACHED ( -- )
   s" a raw PPRIM;X body token does not close before the live PPRIM;" T-LABEL
   BUF-CLEAR s" PPRIM: PKG FOO PPRIM;X create RPSL6 zz PPRIM;" ADD
   s" RPSL6" EXPECT-HIDDEN ;

: TEST-ATTACHED ( -- )
   DUP-CLOSERS
   PRIM-ATTACHED
   PPRIM-ATTACHED
   PRIM-RAW-ATTACHED
   PPRIM-RAW-ATTACHED ;

: PPRIM-ROUTE ( -- )
   s" a PPRIM: row whose primitive name is a definer-shaped token does not leak" T-LABEL
   BUF-CLEAR s" PPRIM: PKG create SL9 PPRIM;" ADD
   s" SL9" EXPECT-HIDDEN ;

: PPRIM-PLAIN ( -- )
   s" a plain PPRIM: row scans clean" T-LABEL
   BUF-CLEAR s" PPRIM: PKG BAR PE-N PE-IN PE-N PE-OUT PPRIM;" ADD
   VERIFY-BUF 0 T= ;

: PPRIM-STRING ( -- )
   s" a spaced PPRIM; inside a body string is content, not the closer" T-LABEL
   BUF-CLEAR s" PPRIM: PKG FOO " ADD ADD-STRING-OPEN
   s" q PPRIM; create PSL1 y" ADD ADD-QUOTE s"  2drop PPRIM;" ADD
   s" PSL1" EXPECT-HIDDEN ;

: PPRIM-ESCAPED ( -- )
   s" a spaced PPRIM; inside an escaped string is content, not the closer" T-LABEL
   BUF-CLEAR s" PPRIM: PKG FOO " ADD ADD-ESC-OPEN
   s" q " ADD ADD-ESC-QUOTE s"  PPRIM; create PSL2 z" ADD ADD-QUOTE
   s"  2drop PPRIM;" ADD  s" PSL2" EXPECT-HIDDEN ;

: PPRIM-LINE-COMMENT ( -- )
   s" a spaced PPRIM; inside a line comment is content, not the closer" T-LABEL
   BUF-CLEAR s" PPRIM: PKG FOO " ADD
   s" \ q PPRIM; create PSL3 zz" ADD ADD-LF
   s"  PE-N PPRIM;" ADD  s" PSL3" EXPECT-HIDDEN ;

: PPRIM-MULTI-COMMENT ( -- )
   s" a spaced PPRIM; inside a multiline comment is content, not the closer" T-LABEL
   BUF-CLEAR
   s" PPRIM: PKG FOO ( q PPRIM; create PSL4 zz ) PE-N PPRIM;" ADD
   s" PSL4" EXPECT-HIDDEN ;

: TEST-PPRIM-CONTENT ( -- )
   PPRIM-ROUTE
   PPRIM-PLAIN
   PPRIM-STRING
   PPRIM-ESCAPED
   PPRIM-LINE-COMMENT
   PPRIM-MULTI-COMMENT ;

\ A package row has two closers: PPRIM; interns the axiom publicly and
\ CLOSE-PRIVATE interns it into the package private wordlist. Both end the row.
\ A bare PRIM: row has no package to be private in, so there the same token is an
\ ordinary effect token and the row still needs PRIM;.

: PPRIM-PRIVATE-CLOSE ( -- )
   s" CLOSE-PRIVATE closes a PPRIM: row and the next top-level definition stays visible" T-LABEL
   BUF-CLEAR s" PPRIM: PKG FOO PE-N PE-IN CLOSE-PRIVATE" ADD ADD-LF
   s" create PRIVAFTER 0 ," ADD ADD-LF
   s" : UPRIVAFTER ( -- ptr a ) PRIVAFTER ;" ADD  VERIFY-BUF 0 T= ;

: PPRIM-PRIVATE-BODY ( -- )
   s" a definer before CLOSE-PRIVATE is row body, so it registers nothing" T-LABEL
   BUF-CLEAR s" PPRIM: PKG FOO create PSL11 zz CLOSE-PRIVATE" ADD
   s" PSL11" EXPECT-HIDDEN ;

: PPRIM-PRIVATE-LOWERCASE ( -- )
   s" a lowercase close-private closes the row exactly as the engine folds it" T-LABEL
   BUF-CLEAR s" PPRIM: PKG FOO PE-N close-private create PLC2 zz PPRIM;" ADD
   s" PLC2" ADD-REF  VERIFY-BUF 0 T= ;

: PPRIM-PRIVATE-STRING ( -- )
   s" a spaced CLOSE-PRIVATE inside a body string is content, not the closer" T-LABEL
   BUF-CLEAR s" PPRIM: PKG FOO " ADD ADD-STRING-OPEN
   s" q CLOSE-PRIVATE create PSL12 y" ADD ADD-QUOTE s"  2drop PPRIM;" ADD
   s" PSL12" EXPECT-HIDDEN ;

: PPRIM-PRIVATE-ESCAPED ( -- )
   s" a spaced CLOSE-PRIVATE inside an escaped string is content, not the closer" T-LABEL
   BUF-CLEAR s" PPRIM: PKG FOO " ADD ADD-ESC-OPEN
   s" q " ADD ADD-ESC-QUOTE s"  CLOSE-PRIVATE create PSL13 z" ADD ADD-QUOTE
   s"  2drop PPRIM;" ADD  s" PSL13" EXPECT-HIDDEN ;

: PPRIM-PRIVATE-LINE-COMMENT ( -- )
   s" a CLOSE-PRIVATE inside a line comment is content, not the closer" T-LABEL
   BUF-CLEAR s" PPRIM: PKG FOO " ADD
   s" \ q CLOSE-PRIVATE create PSL14 zz" ADD ADD-LF
   s"  PE-N PPRIM;" ADD  s" PSL14" EXPECT-HIDDEN ;

: PPRIM-PRIVATE-MULTI-COMMENT ( -- )
   s" a CLOSE-PRIVATE inside a paren comment is content, not the closer" T-LABEL
   BUF-CLEAR
   s" PPRIM: PKG FOO ( q CLOSE-PRIVATE create PSL15 zz ) PE-N PPRIM;" ADD
   s" PSL15" EXPECT-HIDDEN ;

: PPRIM-PRIVATE-ATTACHED ( -- )
   s" a raw CLOSE-PRIVATEX body token does not close before the live PPRIM;" T-LABEL
   BUF-CLEAR s" PPRIM: PKG FOO CLOSE-PRIVATEX create PSL16 zz PPRIM;" ADD
   s" PSL16" EXPECT-HIDDEN ;

: PRIM-PRIVATE-IS-EFFECT ( -- )
   s" CLOSE-PRIVATE in a bare PRIM: row is an effect token, so the row runs on to PRIM;" T-LABEL
   BUF-CLEAR s" PRIM: FOO PE-N PE-IN CLOSE-PRIVATE create SL17 zz PRIM;" ADD
   s" SL17" EXPECT-HIDDEN ;

: MIXED-CLOSERS ( -- )
   s" a PPRIM; row and a CLOSE-PRIVATE row in sequence each end at their own closer" T-LABEL
   BUF-CLEAR s" PPRIM: PKG A PE-N PE-OUT PPRIM;" ADD ADD-LF
   s" create MIDPRIV 0 ," ADD ADD-LF
   s" PPRIM: PKG B PE-N PE-IN CLOSE-PRIVATE" ADD ADD-LF
   s" : UMIDPRIV ( -- ptr a ) MIDPRIV ;" ADD  VERIFY-BUF 0 T= ;

: PRIVATE-ROWS-REAL ( -- )
   s" the four checker.f declaration-frame rows scan clean and expose the next definition" T-LABEL
   BUF-CLEAR
   s" PPRIM: CHECKER-DECL-FRAME START PE-N PE-IN CLOSE-PRIVATE" ADD ADD-LF
   s" PPRIM: CHECKER-DECL-FRAME PREPARE PE-N PE-IN  PE-F PE-OUT CLOSE-PRIVATE" ADD ADD-LF
   s" PPRIM: CHECKER-DECL-FRAME ROLLBACK PE-N PE-IN CLOSE-PRIVATE" ADD ADD-LF
   s" PPRIM: CHECKER-DECL-FRAME RELEASE CLOSE-PRIVATE" ADD ADD-LF
   s" : VP-AFTER-PRIVATE ( -- n ) 7 ;" ADD  VERIFY-BUF 0 T= ;

: TEST-PRIVATE-CLOSER ( -- )
   PPRIM-PRIVATE-CLOSE
   PPRIM-PRIVATE-BODY
   PPRIM-PRIVATE-LOWERCASE
   PPRIM-PRIVATE-STRING
   PPRIM-PRIVATE-ESCAPED
   PPRIM-PRIVATE-LINE-COMMENT
   PPRIM-PRIVATE-MULTI-COMMENT
   PPRIM-PRIVATE-ATTACHED
   PRIM-PRIVATE-IS-EFFECT
   MIXED-CLOSERS
   PRIVATE-ROWS-REAL ;

: BETWEEN-ROWS ( -- )
   s" a genuine top-level create between two prim rows still registers" T-LABEL
   BUF-CLEAR s" PRIM: A PE-N PRIM; create MID zz PRIM: B PE-N PRIM;" ADD ADD-LF
   s" : UMID ( -- ptr a ) MID ;" ADD  VERIFY-BUF 0 T= ;

: SECOND-ROW ( -- )
   s" a second row's body definer is consumed, not leaked by the first row closer" T-LABEL
   BUF-CLEAR s" PRIM: A PE-N PRIM; create MID zz PRIM: B " ADD ADD-STRING-OPEN
   s" q PRIM; create SL10 y" ADD ADD-QUOTE s"  PRIM;" ADD
   s" SL10" EXPECT-HIDDEN ;

: TEST-ROW-ORDER ( -- )
   BETWEEN-ROWS SECOND-ROW ;

: PRIM-LOWERCASE ( -- )
   s" a lowercase closer prim; closes the row exactly as the engine does" T-LABEL
   BUF-CLEAR s" PRIM: FOO PE-N prim; create LC1 zz PRIM;" ADD
   s" LC1" ADD-REF  VERIFY-BUF 0 T= ;

: PPRIM-LOWERCASE ( -- )
   s" a lowercase closer pprim; closes the row exactly as the engine does" T-LABEL
   BUF-CLEAR s" PPRIM: PKG FOO PE-N pprim; create PLC1 zz PPRIM;" ADD
   s" PLC1" ADD-REF  VERIFY-BUF 0 T= ;

: TRUSTED-STRING ( -- )
   s" a spaced semicolon inside a trusted body string no longer early-closes" T-LABEL
   BUF-CLEAR s" TRUSTED: T ( -- ) " ADD ADD-STRING-OPEN
   s" x ; : EVIL ( -- n ) ; y" ADD ADD-QUOTE
   s"  2drop ;" ADD  VERIFY-BUF 0 T= ;

: TRUSTED-CONTROL ( -- )
   s" a real semicolon after a complete trusted body string still closes it" T-LABEL
   BUF-CLEAR s" TRUSTED: T2 ( -- ) " ADD ADD-STRING-OPEN
   s" a b c" ADD ADD-QUOTE s"  2drop ;" ADD
   VERIFY-BUF 0 T= ;

: TEST-CASE-TRUSTED ( -- )
   PRIM-LOWERCASE
   PPRIM-LOWERCASE
   TRUSTED-STRING
   TRUSTED-CONTROL ;

: PLAIN-ROWS ( -- )
   s" plain PRIM: and PPRIM: rows scan clean" T-LABEL
   BUF-CLEAR s" PRIM: FOO PE-N PRIM; PPRIM: PKG BAR PE-N PPRIM;" ADD
   VERIFY-BUF 0 T= ;

: REAL-ROWS ( -- )
   s" a realistic multi-effect row with a trusted-only marker and a pprim scans clean" T-LABEL
   BUF-CLEAR s" PRIM: rr PE-A PE-IN PE-A PE-OUT PRIM; PRIM-TRUSTED-ONLY!" ADD
   s"  PPRIM: PK FF PE-N PE-IN PE-N PE-OUT PPRIM;" ADD
   VERIFY-BUF 0 T= ;

: FOLLOWING-PRIM ( -- )
   s" a top-level definition after a properly closed row remains visible" T-LABEL
   BUF-CLEAR s" PRIM: FOO PE-N PRIM;" ADD ADD-LF
   s" create AFTERC 0 ," ADD ADD-LF
   s" : UAFTER ( -- ptr a ) AFTERC ;" ADD  VERIFY-BUF 0 T= ;

: FOLLOWING-PPRIM ( -- )
   s" a top-level definition after a closed PPRIM: row remains visible" T-LABEL
   BUF-CLEAR s" PPRIM: PKG FOO PE-N PPRIM;" ADD ADD-LF
   s" create PAFTER 0 ," ADD ADD-LF
   s" : UPAFTER ( -- ptr a ) PAFTER ;" ADD  VERIFY-BUF 0 T= ;

: TEST-POSITIVE ( -- )
   PLAIN-ROWS REAL-ROWS FOLLOWING-PRIM FOLLOWING-PPRIM ;

create PATH-BUF FS-PATH-CAP allot
$1000 constant CHILD-MAX
create CHILD-OUT CHILD-MAX allot
create CHILD-ERR CHILD-MAX allot
5000 constant CHILD-NOMINAL-MS
$1000 constant CORPUS-MAX
create NATIVE-PATH FS-PATH-CAP allot
create VERIFY-PATH FS-PATH-CAP allot
create DRIVER-PATH FS-PATH-CAP allot
create COLD-PATH FS-PATH-CAP allot
create ROOT-PATH FS-PATH-CAP allot
create CORPUS-A CORPUS-MAX allot
create CORPUS-B CORPUS-MAX allot
variable NATIVE-U
variable VERIFY-U
variable DRIVER-U
variable COLD-U
variable ROOT-U

: CHILD-CAP ( -- len ) CHILD-MAX >LEN ;
: CHILD-TIMEOUT ( -- ms ) CHILD-NOMINAL-MS T-BUDGET-MS >MS ;

: BUILD-DRIVER ( ptr u8 n -- ) {: row:ptr rowu:n :}
   BUF-CLEAR
   s" require lib/errors.f" ADD ADD-LF
   s" require lib/string.f" ADD ADD-LF
   s" require lib/fs.f" ADD ADD-LF
   s" require src/habu/verify-source.f" ADD ADD-LF
   s" s" ADD ADD-QUOTE SPACE-C ADD-C row rowu ADD ADD-QUOTE
   s"  VERIFY:SOURCE-BUF" ADD ADD-LF ;

: PREP-CHILD ( ptr u8 n -- ptr u8 len ) {: row:ptr rowu:n :}
   CLEANUP-RESET
   s" habu-verify-prim" TMPDIR-MKDIR {: root:ptr rootu:n :}
   root rootu CLEANUP-DIR+
   root rootu s" miss.f" PATH-BUF JOIN-PATH >LEN {: pathu:len :}
   PATH-BUF pathu LEN>N CLEANUP+
   row rowu BUILD-DRIVER
   PATH-BUF pathu LEN>N BUILD$ WRITE-ALL
   PATH-BUF pathu ;

: CAPTURE> ( result<pcap:captured,pcap:failed> -- len len n )
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 0 ENDOF
     err OF PCAP-FAILED:UNMAKE RC>N ENDOF
   ;MATCH ;

: RUN-CHILD ( ptr u8 len ptr u8 len ptr u8 len ms -- len len n )
   {: path:ptr pathu:len out:ptr outcap:len err:ptr errcap:len timeout:ms :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   path pathu PROC-ARGV+
   s" bin/hb" >LEN out outcap err errcap timeout RUN-ARGV-CAPTURE
   CAPTURE> ;

: RUN-COLD-CHILD ( ptr u8 len ptr u8 len ptr u8 len ms -- len len n )
   {: path:ptr pathu:len out:ptr outcap:len err:ptr errcap:len timeout:ms :}
   PROC-ARGV-RESET
   s" --build" >LEN PROC-ARGV+
   path pathu PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   ROOT-PATH ROOT-U @ >LEN PROC-ARGV+
   s" bin/hb" >LEN out outcap err errcap timeout RUN-ARGV-CAPTURE
   CAPTURE> ;

: ADD-NORMAL-LIT ( -- )
   ADD-STRING-OPEN
   s" x" ADD
   ADD-QUOTE ;

: ADD-ESCAPED-LIT ( -- )
   ADD-ESC-OPEN
   s" x" ADD
   ADD-ESC-QUOTE
   s" y" ADD
   ADD-QUOTE ;

: ADD-PRIM-CORPUS ( -- )
   s" PRIM: VP-ATTACH " ADD
   ADD-NORMAL-LIT
   s" 2drop PE-N PE-OUT PRIM;" ADD ADD-LF
   s" PRIM: VP-ESC " ADD
   ADD-ESCAPED-LIT
   s" 2drop PE-N PE-OUT PRIM;" ADD ADD-LF
   s" PRIM: VP-COMMENT ( false PRIM; ) PE-N PE-OUT PRIM;" ADD ADD-LF
   s" PRIM: VP-LINE \ false PRIM;" ADD ADD-LF
   s"  PE-N PE-OUT PRIM;" ADD ADD-LF
   s" PRIM: VP-LOW PE-N PE-OUT prim;" ADD ADD-LF
   s" PRIM: VP-CURSOR " ADD
   ADD-NORMAL-LIT
   s" PRIM; 2drop" ADD ADD-LF ;

: ADD-PPRIM-CORPUS ( -- )
   s" PPRIM: VP-PKG VP-P-ATTACH " ADD
   ADD-NORMAL-LIT
   s" 2drop PE-N PE-OUT PPRIM;" ADD ADD-LF
   s" PPRIM: VP-PKG VP-P-ESC " ADD
   ADD-ESCAPED-LIT
   s" 2drop PE-N PE-OUT PPRIM;" ADD ADD-LF
   s" PPRIM: VP-PKG VP-P-COMMENT ( false PPRIM; ) PE-N PE-OUT PPRIM;" ADD ADD-LF
   s" PPRIM: VP-PKG VP-P-LINE \ false PPRIM;" ADD ADD-LF
   s"  PE-N PE-OUT PPRIM;" ADD ADD-LF
   s" PPRIM: VP-PKG VP-P-LOW PE-N PE-OUT pprim;" ADD ADD-LF
   s" PPRIM: VP-PKG VP-P-CURSOR " ADD
   ADD-ESCAPED-LIT
   s" PPRIM; 2drop" ADD ADD-LF ;

: BUILD-CORPUS ( -- )
   BUF-CLEAR
   ADD-PRIM-CORPUS
   ADD-PPRIM-CORPUS
   s" : VP-FOLLOW ( -- n ) 7 ;" ADD ADD-LF ;

: BUILD-VERIFY-DRIVER ( ptr u8 len -- )
   {: path:ptr pathu:len :}
   BUF-CLEAR
   s" require lib/errors.f" ADD ADD-LF
   s" require lib/string.f" ADD ADD-LF
   s" require lib/fs.f" ADD ADD-LF
   s" require src/habu/verify-source.f" ADD ADD-LF
   s" $1000 constant VP-CAP" ADD ADD-LF
   s" create VP-SRC VP-CAP allot" ADD ADD-LF
   s" s" ADD ADD-QUOTE SPACE-C ADD-C
   path pathu LEN>N ADD
   ADD-QUOTE
   s"  VP-SRC VP-CAP READ-ALL" ADD ADD-LF
   s" VP-SRC swap VERIFY:SOURCE-BUF" ADD ADD-LF ;

: DIFFERENTIAL-PATHS ( ptr u8 n -- )
   {: root:ptr rootu:n :}
   root ROOT-PATH rootu BYTE-COPY
   rootu ROOT-U !
   root rootu s" native.f" NATIVE-PATH JOIN-PATH NATIVE-U !
   root rootu s" verify.f" VERIFY-PATH JOIN-PATH VERIFY-U !
   root rootu s" driver.f" DRIVER-PATH JOIN-PATH DRIVER-U !
   root rootu s" cold.f" COLD-PATH JOIN-PATH COLD-U !
   NATIVE-PATH NATIVE-U @ CLEANUP+
   VERIFY-PATH VERIFY-U @ CLEANUP+
   DRIVER-PATH DRIVER-U @ CLEANUP+
   COLD-PATH COLD-U @ CLEANUP+ ;

: WRITE-CORPUS ( -- )
   BUILD-CORPUS
   NATIVE-PATH NATIVE-U @ BUILD$ WRITE-ALL
   VERIFY-PATH VERIFY-U @ BUILD$ WRITE-ALL ;

: PROVE-CORPUS-BYTES ( -- )
   NATIVE-PATH NATIVE-U @ CORPUS-A CORPUS-MAX READ-ALL {: au:n :}
   VERIFY-PATH VERIFY-U @ CORPUS-B CORPUS-MAX READ-ALL {: bu:n :}
   CORPUS-A au CORPUS-B bu CORE-STR= TTRUE
   CORPUS-A au
   S\" PRIM: VP-ATTACH s\" x\"2drop PE-N PE-OUT PRIM;"
   CONTAINS? TTRUE
   CORPUS-A au
   S\" PRIM: VP-CURSOR s\" x\"PRIM; 2drop"
   CONTAINS? TTRUE
   BUF-CLEAR
   s" PPRIM: VP-PKG VP-P-CURSOR " ADD
   ADD-ESCAPED-LIT
   s" PPRIM; 2drop" ADD
   CORPUS-A au BUILD$ CONTAINS? TTRUE ;

: WRITE-VERIFY-DRIVER ( -- )
   VERIFY-PATH VERIFY-U @ >LEN BUILD-VERIFY-DRIVER
   DRIVER-PATH DRIVER-U @ BUILD$ WRITE-ALL ;

: WRITE-COLD-PAYLOAD ( -- )
   s" cold.f" BF-RESET-OUT
   s" cold.f" BF-APPEND-RUN-PRELUDE
   s" cold.f" NATIVE-PATH NATIVE-U @ BF-APPEND-SOURCE
   s" cold.f" BF-APPEND-COMMON
   s" cold.f" COMPILER-BUILD:SEAL ;

: EXPECT-CHILD-OK ( ptr u8 len -- )
   CHILD-OUT CHILD-CAP CHILD-ERR CHILD-CAP CHILD-TIMEOUT RUN-CHILD
   {: outn:len errn:len code:n :}
   code 0 T=
   CHILD-OUT outn LEN>N s" " T$=
   CHILD-ERR errn LEN>N s" " T$= ;

: EXPECT-COLD-OK ( ptr u8 len -- )
   CHILD-OUT CHILD-CAP CHILD-ERR CHILD-CAP CHILD-TIMEOUT RUN-COLD-CHILD
   {: outn:len errn:len code:n :}
   code 0 T=
   CHILD-OUT outn LEN>N s" " T$=
   CHILD-ERR errn LEN>N s" " T$= ;

: PRODUCTION-DIFFERENTIAL ( -- )
   s" native cold-load and VERIFY:SOURCE-BUF accept the same exact row corpus" T-LABEL
   CLEANUP-RESET
   s" habu-verify-prim-diff" TMPDIR-MKDIR {: root:ptr rootu:n :}
   root rootu CLEANUP-DIR+
   root rootu DIFFERENTIAL-PATHS
   root rootu BF-TMP!
   WRITE-CORPUS
   PROVE-CORPUS-BYTES
   WRITE-VERIFY-DRIVER
   WRITE-COLD-PAYLOAD
   COLD-PATH COLD-U @ >LEN EXPECT-COLD-OK
   DRIVER-PATH DRIVER-U @ >LEN EXPECT-CHILD-OK
   CLEANUP-RUN ;

: PREP-SOURCE-CHILD ( ptr u8 n -- ptr u8 len )
   {: src:ptr srcu:n :}
   srcu CORPUS-MAX > if E-STR-CAPACITY throw then
   src CORPUS-A srcu BYTE-COPY
   CLEANUP-RESET
   s" habu-verify-prim-source" TMPDIR-MKDIR {: root:ptr rootu:n :}
   root rootu CLEANUP-DIR+
   root rootu s" source.f" NATIVE-PATH JOIN-PATH NATIVE-U !
   root rootu s" driver.f" DRIVER-PATH JOIN-PATH DRIVER-U !
   NATIVE-PATH NATIVE-U @ CLEANUP+
   DRIVER-PATH DRIVER-U @ CLEANUP+
   NATIVE-PATH NATIVE-U @ CORPUS-A srcu WRITE-ALL
   NATIVE-PATH NATIVE-U @ >LEN BUILD-VERIFY-DRIVER
   DRIVER-PATH DRIVER-U @ BUILD$ WRITE-ALL
   DRIVER-PATH DRIVER-U @ >LEN ;

: EXPECT-SOURCE-RC74 ( ptr u8 n -- )
   {: src:ptr srcu:n :}
   src srcu PREP-SOURCE-CHILD
   CHILD-OUT CHILD-CAP CHILD-ERR CHILD-CAP CHILD-TIMEOUT RUN-CHILD
   {: outn:len errn:len code:n :}
   code 74 T=
   CHILD-OUT outn LEN>N s" " T$=
   CHILD-ERR errn LEN>N
   s" verify-source: unterminated string" CONTAINS? TTRUE
   CLEANUP-RUN ;

: MISSING-NORMAL-QUOTE ( -- )
   s" an unterminated normal string in a PRIM: row fails closed with rc 74" T-LABEL
   BUF-CLEAR
   s" PRIM: VP-BAD " ADD ADD-STRING-OPEN s" no-close" ADD
   BUILD$ EXPECT-SOURCE-RC74 ;

: MISSING-ESCAPED-QUOTE ( -- )
   s" an unterminated escaped string in a PPRIM: row fails closed with rc 74" T-LABEL
   BUF-CLEAR
   s" PPRIM: VP-PKG VP-P-BAD " ADD ADD-ESC-OPEN s" no-close" ADD
   BUILD$ EXPECT-SOURCE-RC74 ;

: EXPECT-RC74 ( ptr u8 n ptr u8 n -- )
   {: row:ptr rowu:n msg:ptr msgu:n :}
   row rowu PREP-CHILD
   CHILD-OUT CHILD-CAP CHILD-ERR CHILD-CAP CHILD-TIMEOUT RUN-CHILD
   {: outn:len errn:len code:n :}
   code 74 T=
   outn LEN>N 0 T=
   CHILD-ERR errn LEN>N msg msgu CONTAINS? TTRUE
   CLEANUP-RUN ;

: EXPECT-CLOSER-FAIL ( ptr u8 n -- )
   s" verify-source: missing primitive row closer" EXPECT-RC74 ;

: MISSING-PRIM ( -- )
   s" a PRIM: row with no closer fails closed in a child with rc 74" T-LABEL
   s" PRIM: FOO PE-N PE-OUT" EXPECT-CLOSER-FAIL ;

: MISSING-PPRIM ( -- )
   s" a PPRIM: row with no closer fails closed in a child with rc 74" T-LABEL
   s" PPRIM: PKG FOO PE-N PE-OUT" EXPECT-CLOSER-FAIL ;

: WRONG-PRIM ( -- )
   s" a live PPRIM; cannot close a PRIM: row" T-LABEL
   s" PRIM: FOO PE-N PPRIM;" EXPECT-CLOSER-FAIL ;

: WRONG-PPRIM ( -- )
   s" a live PRIM; cannot close a PPRIM: row" T-LABEL
   s" PPRIM: PKG FOO PE-N PRIM;" EXPECT-CLOSER-FAIL ;

: MISSING-PRIM-PRIVATE ( -- )
   s" a bare PRIM: row closed only with CLOSE-PRIVATE still fails closed with rc 74" T-LABEL
   s" PRIM: FOO PE-N CLOSE-PRIVATE" EXPECT-CLOSER-FAIL ;

: MISSING-PPRIM-PRIVATE ( -- )
   s" a PPRIM: row with neither PPRIM; nor CLOSE-PRIVATE fails closed with rc 74" T-LABEL
   s" PPRIM: PKG FOO PE-N PE-IN CLOSE-PRIVATEX" EXPECT-CLOSER-FAIL ;

: MISSING-PPRIM-PACKAGE ( -- )
   s" a PPRIM: row with no package fails closed in a child with rc 74" T-LABEL
   s" PPRIM:"
   s" verify-source: missing primitive package" EXPECT-RC74 ;

: MISSING-PPRIM-NAME ( -- )
   s" a PPRIM: row with a package but no name fails closed in a child with rc 74" T-LABEL
   s" PPRIM: PKG"
   s" verify-source: missing primitive name" EXPECT-RC74 ;

: TEST-FAIL-CLOSED ( -- )
   MISSING-PRIM
   MISSING-PPRIM
   MISSING-PRIM-PRIVATE
   MISSING-PPRIM-PRIVATE
   WRONG-PRIM
   WRONG-PPRIM
   MISSING-PPRIM-PACKAGE
   MISSING-PPRIM-NAME
   MISSING-NORMAL-QUOTE
   MISSING-ESCAPED-QUOTE ;

: RUN ( -- )
   T-RESET
   TEST-PRIM-CONTENT
   TEST-ROLES
   TEST-ATTACHED
   TEST-PPRIM-CONTENT
   TEST-PRIVATE-CLOSER
   TEST-ROW-ORDER
   TEST-CASE-TRUSTED
   TEST-POSITIVE
   PRODUCTION-DIFFERENTIAL
   TEST-FAIL-CLOSED
   T-REPORT ;

RUN

;package
