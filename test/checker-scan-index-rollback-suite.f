\ checker-scan-index-rollback-suite.f — rollback of records a rejected frame
\ creates, for the checker's symbol-keyed store indexes. The other half of
\ test/checker-scan-index-suite.f, whose header says what is under test: these
\ cases touch no state its section 1 creates, so both rows run side by side, and
\ section numbers are that suite's. A WHITEBOX-SUITE row: under bin/hb it exits
\ 70 (docs/gate.md "How a suite runs, and what that demands of its files").

require test/checker-scan-index-lib.f

\ Reopened, not imported: the cases run inside the fixture's package, where
\ the definitions they make land (test/checker-scan-index-lib.f).
using TFAM

package SCANIDX-TEST

\ 2. DIFFERENTIAL, run first: an index mark a rollback case reads means
\    something only once all five indexes exist, and the differential builds
\    them
\    (SCX-MARKS-EXACT).
SCX-TFAM-N 0 > TTRUE                           \ the family differential is not vacuous
SCX-SYM-N 1 > TTRUE                            \ nor the symbol ones
SCX-DIFF-ALL

\ 3. ROLLBACK. The checker's own rollback frames retire store records; the
\    index has to retire with them, and a name redefined AFTER a rollback must
\    answer with its new effect and not with the rolled-back one. Here the
\    symbol or family is new inside the frame; 3d and 3e, in the other row,
\    roll back records of symbols that outlive it.

\ 3a. a rejected candidate frame: a signature added inside it is visible there
\     and gone after, and the same name then takes a DIFFERENT effect cleanly.
s" SCXR" SCX-SIG-MIN-IN -1 T=
CHECK-CANDIDATE-START
   s" n n -- n" s" SCXR" CHECKER-USIG-ADD
   s" SCXR" SCX-SIG-MIN-IN 2 T=                      \ visible inside the candidate
0 CHECK-CANDIDATE-DONE drop
SCX-MARKS-EXACT                                      \ read FIRST: a lookup would rebuild
s" SCXR" SCX-SIG-MIN-IN -1 T=                        \ retired with the frame
s" n -- n" s" SCXR" CHECKER-USIG-ADD
s" SCXR" SCX-SIG-MIN-IN 1 T=                         \ the new effect answers
SCX-DIFF-ALL

\ 3b. the same shape through the real load path: a definition the checker
\     REJECTS rolls its scope back, and the name then takes a different effect
\     that its callers are held to.
: SCX-BADDEF ( -- ) s" : SCXB ( n -- n ) drop ;" evaluate-closed ;
: SCX-GOODDEF ( -- ) s" : SCXB ( n -- ) drop ;" evaluate-closed ;
: SCX-GOODUSE ( -- ) s" : SCXBU ( n -- ) SCXB ;" evaluate-closed ;
: SCX-BADUSE ( -- ) s" : SCXBU2 ( n -- n ) SCXB ;" evaluate-closed ;

' SCX-BADDEF catch TC !   TC @ 0 <> TTRUE            \ rejected: the body drops its output
s" SCXB" SCX-SIG-MIN-IN -1 T=                        \ ... and left no record behind
' SCX-GOODDEF catch TC !  TC @ 0 T=
s" SCXB" SCX-SIG-MIN-IN 1 T=
' SCX-GOODUSE catch TC !  TC @ 0 T=                  \ a caller certifies against the new effect
' SCX-BADUSE catch TC !   TC @ 0 <> TTRUE            \ ... and against nothing else
SCX-DIFF-ALL

\ 3c. a family declared inside a rejected candidate leaves no row and no chain,
\     and the same (package, tail) can then be declared with a different kind.
\     Its variant leaves no tail chain either (VNX-RETIRE).
variable CAND-FAM                                    \ the candidate family's id
s" scxctor" SCX-NAME! SCX-SYM-INTERN IX !
IX @ SUMV-FROM-CTOR-SYM TFALSE drop
s" scxrb" s" cand" TFAM-FIND-IN TFALSE drop
CHECK-CANDIDATE-START
   s" scxrb" CHECKER-PACKAGE-PRIVATE s" cand" 2 TK-PRODUCT TFAM-DECL CAND-FAM !
   s" scxrb" CHECKER-PACKAGE-PRIVATE s" cand2" 2 TK-PRODUCT TFAM-DECL drop
   s" scxrb" s" cand" TFAM-FIND-IN TTRUE drop
   CAND-FAM @ s" cand" 0 0 0 0 SUMV-ADD IX @ SUMV-CTOR-SYM!
   CAND-FAM @ s" cand" SUMV-FIND TTRUE drop
   IX @ SUMV-FROM-CTOR-SYM TTRUE drop
0 CHECK-CANDIDATE-DONE drop
SCX-MARKS-EXACT                                      \ read FIRST: a lookup would rebuild
s" scxrb" s" cand" TFAM-FIND-IN TFALSE drop
s" scxrb" s" cand2" TFAM-FIND-IN TFALSE drop
CAND-FAM @ s" cand" SUMV-FIND TFALSE drop
IX @ SUMV-FROM-CTOR-SYM TFALSE drop
: SCX-REDECL ( -- ) s" scxrb" CHECKER-PACKAGE-PUBLIC s" cand" 0 TK-CELL TFAM-DECL drop ;
' SCX-REDECL catch 0 T=
s" scxrb" s" cand" TFAM-FIND-IN TTRUE drop
SCX-MARKS-EXACT
SCX-DIFF-ALL

\ 3f. a rejected frame's rows are zeroed, not only uncounted. An image carries
\     every store's slack, so a retired row left in place reaches a later image
\     and makes it differ from one whose run never wrote the row. Each store's
\     high mark is read inside the frame; after the pop every cell and byte
\     between the restored count and that mark reads zero.
variable HI-SYMN   variable HI-SYMU   variable HI-CTN    variable HI-CTU
variable HI-VRECN  variable HI-VRECF  variable HI-VRECND variable HI-VNARG
variable HI-VRECU  variable HI-DFER   variable HI-NORET

: SCX-SCRUB-HI! ( -- )
   SYM-N @ HI-SYMN !  SYM-STR-U @ HI-SYMU !
   CTN @ HI-CTN !  CT-STR-U @ HI-CTU !
   VREC-N @ HI-VRECN !  VREC-FIELD-N @ HI-VRECF !
   VREC-NODE-N @ HI-VRECND !  VNARG-N @ HI-VNARG !
   VREC-STR-U @ HI-VRECU !
   DFER-END @ HI-DFER !  NORET-END @ HI-NORET ! ;

: NONZERO-CELLS ( ptr n n n -- n ) {: base:ptr lo:n hi:n :}
   0 hi lo ?do base i cells + @ 0<> IF 1 + THEN loop ;

: NONZERO-BYTES ( ptr u8 n n -- n ) {: base:ptr lo:n hi:n :}
   0 hi lo ?do base i + c@ 0<> IF 1 + THEN loop ;

: CT-COL ( ptr n -- n ) CTN @ HI-CTN @ NONZERO-CELLS ;
: VREC-COL ( ptr n -- n ) VREC-N @ HI-VRECN @ NONZERO-CELLS ;
: VRN-COL ( ptr n -- n ) VREC-NODE-N @ HI-VRECND @ NONZERO-CELLS ;

: SYM-SCRUBBED ( -- )
   s" the frame's symbol rows and name bytes are zero after the pop" T-LABEL
   HI-SYMN @ SYM-N @ > TTRUE
   HI-SYMU @ SYM-STR-U @ > TTRUE
   SYMS CELL-VIEW  SYM-N @ SYM-REC CELL / *  HI-SYMN @ SYM-REC CELL / *
   NONZERO-CELLS 0 T=
   SYM-STR SYM-STR-U @ HI-SYMU @ NONZERO-BYTES 0 T= ;

: CT-SCRUBBED ( -- )
   s" ... and its type-code rows and their name bytes" T-LABEL
   HI-CTN @ CTN @ > TTRUE
   HI-CTU @ CT-STR-U @ > TTRUE
   CT-NAME-A CT-COL  CT-NAME-U CT-COL +  CT-CLASS CT-COL +  CT-WIDTH CT-COL +
   CT-SIGN CT-COL +  CT-PKG-A CT-COL +  CT-PKG-U CT-COL +  0 T=
   CT-STR CT-STR-U @ HI-CTU @ NONZERO-BYTES 0 T= ;

: VREC-ZERO-ABOVE ( -- )
   VREC-NAME-A VREC-COL  VREC-NAME-U VREC-COL +  VREC-START VREC-COL +
   VREC-COUNT VREC-COL +  VREC-TVN VREC-COL +  VREC-RVN VREC-COL +  0 T=
   VREC-FIELDS VREC-FIELD-N @ HI-VRECF @ NONZERO-CELLS 0 T=
   VRN-TAG VRN-COL  VRN-A VRN-COL +  VRN-B VRN-COL +  VRN-C VRN-COL +
   VRN-D VRN-COL +  VRN-E VRN-COL +  VRN-F VRN-COL +  VRN-G VRN-COL +
   VRN-H VRN-COL +  0 T=
   VNARG VNARG-N @ HI-VNARG @ NONZERO-CELLS 0 T=
   VREC-STR VREC-STR-U @ HI-VRECU @ NONZERO-BYTES 0 T= ;

: VREC-SCRUBBED ( -- )
   s" ... and its value record, fields, nodes, argument runs and bytes" T-LABEL
   HI-VRECN @ VREC-N @ > TTRUE
   HI-VRECND @ VREC-NODE-N @ > TTRUE
   VREC-ZERO-ABOVE ;

: CONTROL-SCRUBBED ( -- )
   s" ... and its defer and control rows" T-LABEL
   HI-DFER @ DFER-END @ > TTRUE
   HI-NORET @ NORET-END @ > TTRUE
   DFERS CELL-VIEW  DFER-END @ CELL /  HI-DFER @ CELL /  NONZERO-CELLS 0 T=
   NORETS NORET-END @ HI-NORET @ NONZERO-BYTES 0 T= ;

CHECK-CANDIDATE-START
   s" scxscrub" SCX-NAME! SCX-SYM-INTERN IX !
   IX @ RES-TRUE DFER-ADD-SYM
   IX @ CTL-DEAD 0 0 NORET-ADD-SYM
   s" scxscrubrec" s" member n" CHECKER-TRY-RECORD 0 T= 2drop
   s" scxscrublin" s" SCANIDX-TEST" CT-ADD-LINEAR
   SCX-SCRUB-HI!
0 CHECK-CANDIDATE-DONE drop
SCX-MARKS-EXACT
SYM-SCRUBBED  CT-SCRUBBED  VREC-SCRUBBED  CONTROL-SCRUBBED
SCX-DIFF-ALL

\ 3g. a refused CHECKER-TRY-RECORD scrubs what it wrote the same way. Each
\     refusal comes after the row was begun and its first field stored: the
\     duplicate is found only against the stored "good", and a bad field type
\     only after its field is stored, so both answer the second field's offset,
\     7. The bound is each store's capacity, read after the attempt.
: VREC-HI-CAP! ( -- )
   VREC-CAP-V @ HI-VRECN !  VREC-FIELD-CAP-V @ HI-VRECF !
   VREC-NODE-CAP-V @ HI-VRECND !  VNARG-CAP-V @ HI-VNARG !
   VREC-STR-CAP-V @ HI-VRECU ! ;

s" a refused record's rows, fields, nodes, runs and bytes are zero" T-LABEL
s" scxtryrefa" s" good n bad nosuch-type-zz" CHECKER-TRY-RECORD
s" checker: bad value-record field type" T$=  7 T=
VREC-HI-CAP!  VREC-ZERO-ABOVE
s" scxtryrefb" s" good n good n" CHECKER-TRY-RECORD
s" checker: duplicate value-record field" T$=  7 T=
VREC-HI-CAP!  VREC-ZERO-ABOVE

T-REPORT

;package

;using
