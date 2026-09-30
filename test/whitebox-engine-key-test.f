\ whitebox-engine-key-test.f - the whitebox host's cache key follows the tree.
\
\ Run: bin/hb --load test/whitebox-engine-key-test.f
\
\ test/whitebox-key.f keys the unsealed engine on the builder's ordered
\ require/include closure, which is the engine's own boot prefix. This proves the
\ consequence the gate depends on: an edit under src/ moves the keyed artifact
\ path - so the cached host is a miss and gets rebuilt - while bin/hb is never
\ written.
\
\ The tree cannot be edited to show that, so the closure is copied into a private
\ root and keyed there through WHITEBOX-KEY:ENTRY-PATH!, the same derivation
\ test/whitebox-engine.f resolves with. Manifested files must keep their real
\ pathname for discovery. Their static require closures stay there too, so those
\ files resolve the same dependencies in the copied and real builder closures.

require lib/test.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/engine-candidate.f
require lib/process-cwd.f
require tools/dynamic-tail-manifest.f
require tools/event-closure-lib.f
require test/whitebox-key.f

using SOURCE-ROOT
package WHITEBOX-KEY-TEST

FS-PATH-CAP 1+ constant CAP
32 constant DG-LEN

create ROOT CAP allot
create ENTRY CAP allot
create SEAL CAP allot
create REAL-SEAL CAP allot
create SPARE CAP allot
create DST CAP allot
create PATH-A CAP allot
create PATH-B CAP allot
create PATH-C CAP allot
create PATH-D CAP allot
create ENG-A DG-LEN allot
create FSHA-CTX SHA256-FILE-CTX-BYTES allot   \ this fixture's file-digest context
create ENG-B DG-LEN allot
create CHILD-OUT $4000 allot
create CHILD-ERR $4000 allot

variable ROOT-U
variable ENTRY-U
variable SEAL-U
variable REAL-SEAL-U
variable SPARE-U
variable DST-U
variable PATH-A-U
variable PATH-B-U
variable PATH-C-U
variable PATH-D-U
variable IDX
variable REAL-N
variable SEAL-SEEN?

: ROOT$ ( -- ptr u8 n )       ROOT ROOT-U @ ;
: ENTRY$ ( -- ptr u8 n )      ENTRY ENTRY-U @ ;
: SEAL$ ( -- ptr u8 n )       SEAL SEAL-U @ ;
: REAL-SEAL$ ( -- ptr u8 n )  REAL-SEAL REAL-SEAL-U @ ;
: SPARE$ ( -- ptr u8 n )      SPARE SPARE-U @ ;
: DST$ ( -- ptr u8 n )        DST DST-U @ ;
: A$ ( -- ptr u8 n )          PATH-A PATH-A-U @ ;
: B$ ( -- ptr u8 n )          PATH-B PATH-B-U @ ;
: C$ ( -- ptr u8 n )          PATH-C PATH-C-U @ ;
: D$ ( -- ptr u8 n )          PATH-D PATH-D-U @ ;

\ The seal pass the whitebox build stands down - a boot-prefix file, and the one
\ this fixture edits.
: SEAL-REL$ ( -- ptr u8 n )  s" src/core/internal-mark.f" ;

\ A file in the copy that no closure member loads.
: SPARE-REL$ ( -- ptr u8 n ) s" src/core/whitebox-key-unloaded.f" ;

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   u CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

: UNDER-ROOT! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   ROOT$ a u dst JOIN-PATH up ! ;

: ENGINE-DIGEST! ( ptr u8 -- ) {: dst:ptr :}
   FSHA-CTX ENGINE-CANDIDATE:PATH$ dst SHA256-FILE-IN dup 0 <> if throw then drop ;

\ One closure member into the copy. Discovery recognizes a manifested file by
\ its real pathname, so it must stay there.
: MEMBER-COPY ( ptr u8 n -- ) {: a:ptr u:n :}
   a u DTM:KNOWN? if exit then
   a u CWD$ BELOW? 0= if E-FS-PATH throw then
   a u CWD$ RELATIVE {: r:ptr ru:n :}
   r ru SEAL-REL$ STR= if 0 0= SEAL-SEEN? ! then
   r ru DST DST-U UNDER-ROOT!
   DST$ DIRNAME MAKE-DIRS
   a u DST$ COPY-FILE-STREAM ;

\ A real-path boundary's static requires also resolve from the real tree.
\ Leave their closure there instead of introducing shadow copies that the
\ copied builder would load before reaching the boundary.
: BOUNDARY-MEMBER-UNCOPY ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CWD$ BELOW? 0= if exit then
   a u CWD$ RELATIVE DST DST-U UNDER-ROOT!
   DST$ FILE? if DST$ REMOVE-FILE then ;

: LEAVE-BOUNDARY-CLOSURES ( -- )
   DTM:COUNT 0 ?do
      CWD$ i DTM:PATH$ JOIN EC:BUILD
      0 IDX !
      begin IDX @ EC:COUNT < while
         IDX @ EC:PATH$ BOUNDARY-MEMBER-UNCOPY
         IDX @ 1+ IDX !
      repeat
   loop ;

: COPY-CLOSURE ( -- )
   0 SEAL-SEEN? !
   WHITEBOX-KEY:BUILDER$ EC:BUILD
   EC:COUNT REAL-N !
   0 IDX !
   begin IDX @ REAL-N @ < while
      IDX @ EC:PATH$ MEMBER-COPY
      IDX @ 1+ IDX !
   repeat
   LEAVE-BOUNDARY-CLOSURES ;

\ The entry sits at the copy's own root, because a dependency resolves under the
\ directory the entry file was found in: at <root>/native-build.f every
\ root-relative require below it names the copy.
: COPY-ENTRY ( -- )
   s" native-build.f" ENTRY ENTRY-U UNDER-ROOT!
   WHITEBOX-KEY:BUILDER$ ENTRY$ COPY-FILE-STREAM ;

: PREP ( -- )
   CLEANUP-RESET
   s" habu-whitebox-key" HB-TMP-MKDIR CANONICAL TTRUE ROOT ROOT-U COPY!
   ROOT$ CLEANUP-TREE+
   ENG-A ENGINE-DIGEST!
   COPY-CLOSURE
   COPY-ENTRY
   SEAL-REL$ SEAL SEAL-U UNDER-ROOT!
   SPARE-REL$ SPARE SPARE-U UNDER-ROOT!
   CWD$ SEAL-REL$ REAL-SEAL JOIN-PATH REAL-SEAL-U ! ;

: TEST-CLOSURE-CARRIES-PREFIX ( -- )
   s" the closure the key folds carries the boot prefix" T-LABEL
   SEAL-SEEN? @ 0 <> TTRUE ;

: TEST-COPY-IS-COMPLETE ( -- )
   s" and its copy has the same source closure" T-LABEL
   ENTRY$ EC:BUILD
   EC:COUNT REAL-N @ T= ;

: TEST-COPY-SHARES-KEY ( -- )
   s" identical source closures in different roots share the artifact" T-LABEL
   ENTRY$ PATH-A PATH-A-U WHITEBOX-KEY:ENTRY-PATH!
   WHITEBOX-KEY:BUILDER$ PATH-B PATH-B-U WHITEBOX-KEY:ENTRY-PATH!
   A$ B$ T$= ;

: TEST-PREFIX-EDIT-MOVES-KEY ( -- )
   s" one edited prefix file moves the keyed artifact" T-LABEL
   ENTRY$ PATH-A PATH-A-U WHITEBOX-KEY:ENTRY-PATH!
   SEAL$ s\" \\ whitebox-engine-key-test\n" APPEND-FILE
   ENTRY$ PATH-B PATH-B-U WHITEBOX-KEY:ENTRY-PATH!
   A$ B$ T$<> ;

: TEST-RESTORE-RESTORES-KEY ( -- )
   s" restoring its bytes brings the same artifact back" T-LABEL
   REAL-SEAL$ SEAL$ COPY-FILE-STREAM
   ENTRY$ PATH-C PATH-C-U WHITEBOX-KEY:ENTRY-PATH!
   A$ C$ T$= ;

: TEST-UNLOADED-FILE-IS-IGNORED ( -- )
   s" a file the build never loads leaves it alone" T-LABEL
   SPARE$ s\" \\ no loader reaches this file\n" WRITE-ALL
   ENTRY$ PATH-D PATH-D-U WHITEBOX-KEY:ENTRY-PATH!
   A$ D$ T$= ;

\ The whole point of keying the closure: the artifact moved without the binary
\ moving, so a tree whose sources changed cannot reuse the host built from the
\ older ones.
: TEST-ENGINE-UNTOUCHED ( -- )
   s" and bin/hb was never written" T-LABEL
   ENG-B ENGINE-DIGEST!
   ENG-A DG-LEN ENG-B DG-LEN STR= TTRUE ;

\ Manifest identity is relative to the child's invocation root. Give it real
\ copies of the two boundary files, one private dependency, and links to the
\ unchanged test/tool libs.
: MANIFEST$ ( -- ptr u8 n ) ROOT$ s" manifest" JOIN ;

: MANIFEST-LINK ( ptr u8 n -- ) {: a:ptr u:n :}
   CWD$ a u JOIN DST DST-U COPY!
   DST$ MANIFEST$ a u JOIN MAKE-SYMLINK ;

: MANIFEST-PREP ( -- )
   MANIFEST$ s" src/habu" JOIN MAKE-DIRS
   MANIFEST$ s" src/core" JOIN MAKE-DIRS
   s" lib" MANIFEST-LINK s" tools" MANIFEST-LINK s" test" MANIFEST-LINK
   s" src/habu/task-abi.f" MANIFEST-LINK
   MANIFEST$ s" whitebox-manifest-private.f" JOIN
   s\" \\ private dependency\n" WRITE-ALL ;

: MANIFEST-RUN ( ptr u8 n -- ) {: path:ptr pathu:n :}
   PROC-ARGV-RESET PROC-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   s" test/whitebox-manifest-child.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   path pathu >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ CANONICAL TTRUE >LEN MANIFEST$ >LEN
   CHILD-OUT $4000 >LEN CHILD-ERR $4000 >LEN 30000 >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE 0 ENDOF
      err OF PCAP-FAILED:UNMAKE RC>N ENDOF
   ;MATCH {: outu:len erru:len rc:n :}
   rc 0 <> if CHILD-OUT outu LEN>N type CHILD-ERR erru LEN>N type then
   rc 0 T= ;

: MANIFEST-CASE ( ptr u8 n -- ) {: a:ptr u:n :}
   MANIFEST$ a u JOIN DST DST-U COPY!
   a u DST$ COPY-FILE-STREAM
   DST$ S\" \nrequire whitebox-manifest-private.f\n" APPEND-FILE
   a u MANIFEST-RUN ;

: TEST-MANIFEST-LOADS ( -- )
   s" a manifested member keys its static private dependency" T-LABEL
   MANIFEST-PREP
   s" src/habu/driver-io.f" MANIFEST-CASE
   s" src/core/include.f" MANIFEST-CASE ;

: RUN ( -- )
   T-RESET
   PREP
   TEST-CLOSURE-CARRIES-PREFIX
   TEST-COPY-IS-COMPLETE
   TEST-COPY-SHARES-KEY
   TEST-PREFIX-EDIT-MOVES-KEY
   TEST-RESTORE-RESTORES-KEY
   TEST-UNLOADED-FILE-IS-IGNORED
   TEST-MANIFEST-LOADS
   TEST-ENGINE-UNTOUCHED
   CLEANUP-RUN
   T-REPORT ;

RUN
;package
;using
