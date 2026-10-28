\ A cached FILE replays its address cell rows: a pointer cell holding its own
\ DATA, a quotation cell, a pointer cell holding a literal body that shares the
\ file's literal pool with a definition's literal, and an earlier file's pointer
\ cell a later file overwrote. The warm build reuses every file and its product
\ is the cold one byte for byte. So it is when a file declares an earlier
\ file's cell before its own. A file that writes an earlier file's DATA and
\ then loads a child is refused at capture, as is one that stores in a pointer
\ cell the end of its DATA before a child load, where the child's first DATA
\ starts, whether the cell is its own or an earlier file's written after its
\ last child load.
\ bin/hb --load test/native-file-slots-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require test/native-file-app-lib.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-SLOTS-TEST
create APP FS-PATH-CAP allot variable APP-U
create CACHE FS-PATH-CAP allot variable CACHE-U
DYNAMIC-BUFFER BYTES u8
DYNAMIC-BUFFER COLD u8  variable COLD-U
$1000 constant LINE-CAP
create LINE LINE-CAP allot  variable LINE-U

: LIB$ ( -- ptr u8 n )
   S\" package SLOT-LIB\npublic\nPERSISTED-PTR-VARIABLE P\ncreate BUF 16 allot\nBUF P !\n65 BUF c!\nTYPED-VARIABLE Q [ -- n ]\n: SEVEN ( -- n ) 7 ;\n' SEVEN Q xt!\nPERSISTED-PTR-VARIABLE S\ns\" hello\" drop S !\n: GREET ( -- ) s\" world\" type ;\n: PEEK ( -- n ) P @ c@ ;\n: CALL ( -- n ) Q @ execute ;\n: SAY ( -- ) S @ 5 type ;\n;package\n" ;
: SET$ ( -- ptr u8 n )
   S\" require lib.f\ncreate OTHER 8 allot\n66 OTHER c!\nOTHER SLOT-LIB:P !\n" ;
: APP$ ( -- ptr u8 n )
   S\" require lib.f\nrequire set.f\n: MAIN ( -- ) SLOT-LIB:PEEK . SLOT-LIB:CALL . SLOT-LIB:SAY SLOT-LIB:GREET ;\n" ;
: SET2$ ( -- ptr u8 n )
   S\" require lib.f\ncreate OTHER 8 allot\n66 OTHER c!\nOTHER SLOT-LIB:P !\nrequire more.f\n" ;
: MORE$ ( -- ptr u8 n )
   S\" package SLOT-MORE\npublic\n: M ( -- n ) 3 ;\n;package\n" ;
: APP2$ ( -- ptr u8 n )
   S\" require set2.f\n: MAIN ( -- ) SLOT-LIB:PEEK . SLOT-MORE:M . ;\n" ;
: EXT$ ( -- ptr u8 n )
   S\" package SLOT-EXT\npublic\ncreate CELLX 0 ,\n;package\n" ;
: REG$ ( -- ptr u8 n )
   S\" require ext.f\npackage SLOT-REG\npublic\n: NINE ( -- n ) 9 ;\n' NINE SLOT-EXT:CELLX xt!\nTYPED-VARIABLE R [ -- n ]\n' NINE R xt!\n: CALLR ( -- n ) R @ execute ;\n;package\n" ;
: APP4$ ( -- ptr u8 n )
   S\" require reg.f\n: MAIN ( -- ) SLOT-REG:CALLR . ;\n" ;
: BND$ ( -- ptr u8 n )
   S\" create BEFORE 8 allot\nrequire child.f\nPERSISTED-PTR-VARIABLE P\nSLOT-CHILD:C P !\n" ;
: CHILD$ ( -- ptr u8 n )
   S\" package SLOT-CHILD\npublic\ncreate C 8 allot\n;package\n" ;
: APP5$ ( -- ptr u8 n )
   S\" require bnd.f\nrequire par.f\n: MAIN ( -- ) P @ SLOT-CHILD:C = if s\" same\" else s\" moved\" then type SLOT-LIB:P @ SLOT-PAR:BEFORE - . ;\n" ;
: PAR$ ( -- ptr u8 n )
   S\" require lib.f\npackage SLOT-PAR\npublic\ncreate BEFORE 8 allot\n;package\nrequire kid.f\nhere SLOT-LIB:P !\n" ;
: KID$ ( -- ptr u8 n )
   S\" package SLOT-KID\npublic\n: K ( -- n ) 1 ;\n;package\n" ;
: KID8$ ( -- ptr u8 n )
   S\" package SLOT-KID\npublic\n: K ( -- n ) 1 ;\ncreate KB 8 allot\n;package\n" ;
: DFR$ ( -- ptr u8 n ) S\" defer HOOK ( -- n )\n" ;
: INST7$ ( -- ptr u8 n )
   S\" require dfr.f\n: HOOK-SET ( -- ) [: 7 ;] is HOOK ;\nHOOK-SET\n" ;
: INST8$ ( -- ptr u8 n )
   S\" require dfr.f\n: HOOK-SET ( -- ) [: 8 ;] is HOOK ;\nHOOK-SET\n" ;
: APP6$ ( -- ptr u8 n ) S\" require inst.f\n: MAIN ( -- ) HOOK . ;\n" ;

\ The build of name, the root file, with its out root under the tree.
: USE ( ptr u8 n ptr u8 n -- )
   {: app:ptr appu:n cache:ptr cacheu:n :}
   app appu AT {: a:ptr u:n :}
   a APP u BYTE-COPY u APP-U !
   cache cacheu AT {: c:ptr cu:n :}
   c CACHE cu BYTE-COPY cu CACHE-U ! CACHE CACHE-U @ MAKE-DIR ;

\ One build process per step: a warm build reads what the cold one stored,
\ which only a second process can, and every step of one app shares a cache.
: BUILD ( ptr u8 n -- n )
   AT {: product:ptr productu:n :}
   APP APP-U @ product productu CACHE CACHE-U @ NATIVE-FILE-APP:BUILD ;

\ A build that fails prints its builder's streams.
: BUILT ( ptr u8 n -- )
   BUILD NATIVE-FILE-APP:OUT$ NATIVE-FILE-APP:ERR$ STREAMS! SUCCESS ;

: RAN ( ptr u8 n ptr u8 n -- )
   {: product:ptr productu:n want:ptr wantu:n :}
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   product productu AT s" " CAPTURE SUCCESS
   OUT$ want wantu CONTAINS? TTRUE ;

\ The build's report names count files reused, as PRINT-COUNTS prints it.
: REUSED ( ptr u8 n -- ) NATIVE-FILE-APP:OUT$ 2swap CONTAINS? TTRUE ;

: LOAD ( ptr u8 n -- n )
   AT {: path:ptr pathu:n :}
   path pathu FILE-SIZE {: size:n :}
   size BYTES-RESERVE
   path pathu 0 BYTES size READ-ALL size T=
   size ;

: KEEP ( ptr u8 n -- )
   LOAD {: size:n :}
   size COLD-RESERVE
   0 BYTES 0 COLD size BYTE-COPY size COLD-U ! ;

: SAME? ( ptr u8 n -- bool )
   LOAD {: size:n :}
   0 BYTES size 0 COLD COLD-U @ STR= ;

: SLOTS-CASE ( -- )
   s" lib.f" LIB$ PUT
   s" set.f" SET$ PUT
   s" app.f" APP$ PUT
   s" app.f" s" cache" USE
   s" cold build stores every file" T-LABEL
   s" cold-hb" BUILT
   S\" files reused 0\n" REUSED
   s" cold-hb" S\" 66\n7\nhelloworld" RAN
   s" cold-hb" KEEP
   s" warm build installs every file's slot rows" T-LABEL
   s" warm-hb" BUILT
   S\" files reused 3\n" REUSED
   NATIVE-FILE-APP:ERR$ s" reason=unsupported-slots" CONTAINS? TFALSE
   s" warm-hb" S\" 66\n7\nhelloworld" RAN
   s" warm product is the cold one" T-LABEL
   s" warm-hb" SAME? TTRUE ;

\ reg.f declares ext.f's cell with xt! and then its own quotation cell, so
\ the registry holds the earlier file's row first.
: REGISTRY-CASE ( -- )
   s" ext.f" EXT$ PUT
   s" reg.f" REG$ PUT
   s" app4.f" APP4$ PUT
   s" app4.f" s" cache4" USE
   s" cold build stores an earlier cell declared before a local one" T-LABEL
   s" cold4-hb" BUILT
   s" cold4-hb" S\" 9\n" RAN
   s" cold4-hb" KEEP
   s" warm build declares the cells in the cold order" T-LABEL
   s" warm4-hb" BUILT
   S\" files reused 3\n" REUSED
   s" warm4-hb" S\" 9\n" RAN
   s" warm product with an earlier cell declared first is the cold one" T-LABEL
   s" warm4-hb" SAME? TTRUE ;


\ The refusal line for name under the tree, refused for reason.
: REFUSAL$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: name:ptr nameu:n why:ptr whyu:n :}
   0 LINE-U !
   s" native-file: refused path=" LINE LINE-CAP LINE-U BUF-APPEND
   name nameu AT LINE LINE-CAP LINE-U BUF-APPEND
   s"  reason=" LINE LINE-CAP LINE-U BUF-APPEND
   why whyu LINE LINE-CAP LINE-U BUF-APPEND
   s\" \n" LINE LINE-CAP LINE-U BUF-APPEND
   LINE LINE-U @ ;

: WRITE-REFUSAL$ ( -- ptr u8 n )
   s" set2.f" s" earlier-write-before-load" REFUSAL$ ;

: REFUSAL-CASE ( -- )
   s" set2.f" SET2$ PUT
   s" more.f" MORE$ PUT
   s" app2.f" APP2$ PUT
   s" app2.f" s" cache2" USE
   s" a file that writes earlier DATA before a load is refused" T-LABEL
   s" cold2-hb" BUILT
   NATIVE-FILE-APP:ERR$ WRITE-REFUSAL$ CONTAINS? TTRUE
   s" cold2-hb" S\" 66\n3\n" RAN
   s" the warm build reuses every other file" T-LABEL
   s" warm2-hb" BUILT
   S\" files reused 3\n" REUSED
   NATIVE-FILE-APP:ERR$ WRITE-REFUSAL$ CONTAINS? TTRUE
   s" warm2-hb" S\" 66\n3\n" RAN ;

\ C, the child's first DATA, is where bnd.f's DATA before the load ends, and
\ the end of a segment is its own: as a reference into bnd.f's DATA, C would
\ come back as the start of bnd.f's DATA after the load. par.f stores here
\ after a load of kid.f, which allots nothing, with no segment after it: a
\ reference would come back as the end of BEFORE after kid.f allots DATA.
: BOUNDARY-CASE ( -- )
   s" bnd.f" BND$ PUT
   s" child.f" CHILD$ PUT
   s" par.f" PAR$ PUT
   s" kid.f" KID$ PUT
   s" app5.f" APP5$ PUT
   s" app5.f" s" cache5" USE
   s" a pointer to a child's first DATA refuses the parent" T-LABEL
   s" cold5-hb" BUILT
   NATIVE-FILE-APP:ERR$ s" bnd.f" s" slot-boundary" REFUSAL$ CONTAINS? TTRUE
   NATIVE-FILE-APP:ERR$ s" par.f" s" slot-boundary" REFUSAL$ CONTAINS? TTRUE
   s" cold5-hb" S\" same8\n" RAN
   s" the warm build reuses every other file" T-LABEL
   s" warm5-hb" BUILT
   S\" files reused 4\n" REUSED
   NATIVE-FILE-APP:ERR$ s" bnd.f" s" slot-boundary" REFUSAL$ CONTAINS? TTRUE
   NATIVE-FILE-APP:ERR$ s" par.f" s" slot-boundary" REFUSAL$ CONTAINS? TTRUE
   s" warm5-hb" S\" same8\n" RAN
   s" a child that grows moves the parent's pointer past its DATA" T-LABEL
   s" kid.f" KID8$ PUT
   s" grown5-hb" BUILT
   s" grown5-hb" S\" same16\n" RAN ;

\ inst.f's HOOK-SET compiles `is` on dfr.f's defer and runs: the defer's cell
\ is a declared address cell, so the store is a slot row and the quotation it
\ holds an edge.
: REBIND-CASE ( -- )
   s" dfr.f" DFR$ PUT
   s" inst.f" INST7$ PUT
   s" app6.f" APP6$ PUT
   s" app6.f" s" cache6" USE
   s" cold build stores a file that rebinds an earlier defer" T-LABEL
   s" cold6-hb" BUILT
   s" cold6-hb" S\" 7\n" RAN
   s" cold6-hb" KEEP
   s" warm build reuses the rebinding file" T-LABEL
   s" warm6-hb" BUILT
   S\" files reused 3\n" REUSED
   s" warm6-hb" S\" 7\n" RAN
   s" warm product of a rebinding file is the cold one" T-LABEL
   s" warm6-hb" SAME? TTRUE
   s" an edited rebinding file recompiles" T-LABEL
   s" inst.f" INST8$ PUT
   s" edit6-hb" BUILT
   S\" files reused 2\n" REUSED
   s" edit6-hb" S\" 8\n" RAN ;

public
: MAIN ( -- )
   T-RESET
   true NATIVE-FILE-APP:DIAG? !
   s" native-file-slots" TREE
   SLOTS-CASE
   REGISTRY-CASE
   REFUSAL-CASE
   BOUNDARY-CASE
   REBIND-CASE
   T-REPORT
   s" native file slots artifacts: " type ROOT$ type cr ;
;package
;using
NATIVE-FILE-SLOTS-TEST:MAIN
