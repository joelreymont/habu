\ A file frame records each dependency the file consumes from earlier files
\ once, and none of the records the file makes itself. Each file that compiles
\ whole also writes one object under a key that names the file's place in the
\ load, not its text, so an edit keeps the key. Its definitions' result
\ digests change with their code and not with a comment or a relocation.
\ bin/hb --load test/native-file-frame-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/native-file-app-lib.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-FRAME-TEST

$8000 constant CAP
create INPUT CAP allot variable INPUT-U
create WANT CAP allot variable WANT-U
64 constant KEY-LEN
create APP-KEY KEY-LEN allot
create LIB-KEY KEY-LEN allot
16 constant RES-LEN
create APP-RES RES-LEN allot
create LIB-RES RES-LEN allot
24 constant AT-CAP
create HITS-AT AT-CAP allot variable HITS-AT-U

: IN+ ( ptr u8 n -- ) INPUT CAP INPUT-U BUF-APPEND ;

: LOAD+ ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   S\" s\" " IN+ name nameu AT IN+ S\" \" s\" repl\" NDEF-CACHE:LOAD\n" IN+ ;

\ app.f's text, which its object's source spans are checked against.
: APP$ ( -- ptr u8 n )
   S\" require base.f\nrequire run.f\nrequire safe.f\nrequire vread.f\n: PRE ( -- n ) FRAME-BASE:ONE ;\nrequire lib.f\n: IMM-USER ( -- n ) FRAME-LIB:MARK 5 ;\n: FRAME-USE ( -- n )\n   FRAME-LIB:SEVEN FRAME-LIB:TWICE FRAME-LIB:TWICE FRAME-LIB:TWICE FRAME-BASE:ONE + FRAME-DEEP:TEN + ;\nrequire exec.f\n: TALLY ( -- ) 1 FRAME-LIB:HITS +! ;\nTALLY\n' FRAME-LIB:QUAD FRAME-LIB:HOOK xt!\n" ;

\ The diagnostic builder session: each file loads through NDEF-CACHE:LOAD with
\ the cache diagnostics on, so every file frame and object prints its line on
\ stderr. diag.f binds DIAGXT, a deferred word whose link the checker handover
\ carries to the engine checker's own kept row (src/core/checker.f
\ TRANSFER-ROW). Then a multi-error run refuses BROKEN's body at tier 0 and
\ keeps its declared row as a recovery fact; broken.f binds BROKEN under the
\ same run. after.f, the first file after the run ends, stores its definition:
\ the run's last recovery binding leaves the checker idle.
: PROBE ( -- n )
   NATIVE-FILE-APP:BUILDER$ {: builder:ptr builderu:n :}
   0 INPUT-U !
   S\" 1 set-tier\nNATIVE-RUNTIME:CAPTURE-PREPARE\n" IN+
   s" app.f" LOAD+
   S\" s\" use=\" type FRAME-USE . cr\n" IN+
   S\" s\" hits=\" type FRAME-LIB:HITS NULL-PTR - . space cr\n" IN+
   s" watermark.f" LOAD+
   s" ret-own.f" LOAD+
   s" ret-parent.f" LOAD+
   s" ret-late.f" LOAD+
   s" incut.f" LOAD+
   s" gp.f" LOAD+
   s" q1.f" LOAD+
   s" diag.f" LOAD+
   S\" 0 set-tier\nMULTI-ERR-BEGIN\n: BROKEN ( -- n ) 1 2 ;\n1 set-tier\n" IN+
   s" broken.f" LOAD+
   S\" s\" refused=\" type MULTI-ERR-END .\n" IN+
   s" after.f" LOAD+
   s" retire.f" LOAD+
   s" raw.f" LOAD+
   s" poke.f" LOAD+
   s" slot.f" LOAD+
   s" later.f" LOAD+
   s" scope.f" LOAD+
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" cache" AT >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   s" HABU_NATIVE_CACHE_DIAG" >LEN s" 1" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   builder builderu INPUT INPUT-U @ CAPTURE ;

\ The stderr line whose path ends in name followed by tag, from that tail to
\ its newline: tag " rows=" finds the file frame, " key=" the file object.
: LINE$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: name:ptr nameu:n tag:ptr tagu:n :}
   0 INPUT-U !
   name nameu IN+ tag tagu IN+
   ERR$ INPUT INPUT-U @ FIND-SUB MATCH option
     none OF T-FAIL s" " ENDOF
     some OF IDX>N {: at:n :}
        ERR$ drop at +  ERR$ nip at -  {: a:ptr u:n :}
        a u 10 INDEX-OF MATCH option
          none OF a u ENDOF
          some OF IDX>N a swap ENDOF
        ;MATCH
     ENDOF
   ;MATCH ;

: FRAME$ ( ptr u8 n -- ptr u8 n ) s"  rows=" LINE$ ;
: OBJECT$ ( ptr u8 n -- ptr u8 n ) s"  key=" LINE$ ;

\ The value of field (" name=") in line, up to the next space or the end.
: FIELD$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: line:ptr lineu:n field:ptr fieldu:n :}
   line lineu field fieldu FIND-SUB MATCH option
     none OF T-FAIL s" " ENDOF
     some OF IDX>N fieldu + {: at:n :}
        line at +  lineu at -  {: a:ptr u:n :}
        a u 32 INDEX-OF MATCH option
          none OF a u ENDOF
          some OF IDX>N a swap ENDOF
        ;MATCH
     ENDOF
   ;MATCH ;

\ A field's decimal value.
: FIELD-N ( ptr u8 n -- n )
   STR>NUMBER? MATCH option
     none OF T-FAIL 0 ENDOF
     some OF ENDOF
   ;MATCH ;

: KEY$ ( ptr u8 n -- ptr u8 n ) OBJECT$ s"  key=" FIELD$ ;
: DEFS$ ( ptr u8 n -- ptr u8 n ) OBJECT$ s"  defs=" FIELD$ ;
: RESULTS$ ( ptr u8 n -- ptr u8 n ) OBJECT$ s"  results=" FIELD$ ;
: RESOLUTIONS$ ( ptr u8 n -- ptr u8 n ) FRAME$ s"  resolution=" FIELD$ ;

\ name's object line holds field with the value expected.
: OBJECT-HAS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n field:ptr fieldu:n want:ptr wantu:n :}
   name nameu OBJECT$ field fieldu FIELD$ want wantu T$= ;

\ name's object holds count input rows.
: INPUTS ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n count:ptr countu:n :}
   name nameu OBJECT$ s"  inputs=" FIELD$ count countu T$= ;

\ The object file the key names exists in the build cache.
: STORED ( ptr u8 n -- )
   {: key:ptr keyu:n :}
   0 INPUT-U !
   s" cache/hb-file-" IN+ key keyu IN+ s" .hfo" IN+
   INPUT INPUT-U @ AT EXISTS? TTRUE ;

: KEEP ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n dest:ptr destu:n :}
   srcu destu T=
   src dest srcu destu min BYTE-COPY ;

: LIB-SOURCE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: head:ptr headu:n twice:ptr twiceu:n mark:ptr marku:n :}
   0 INPUT-U !
   head headu IN+
   S\" require base.f\nrequire deep.f\npackage FRAME-LIB\npublic\n: TWICE ( n -- n ) " IN+
   twice twiceu IN+
   S\"  ;\n: MARK ( -- ) " IN+
   mark marku IN+
   S\"  ; immediate\nvariable HITS\ncreate T 16 allot\nTYPED-VARIABLE HOOK [ n -- n ]\n: QUAD ( n -- n ) TWICE TWICE FRAME-BASE:ONE + ;\nTRUSTED: BUMP ( n -- n ) 1 + ;\n7 constant SEVEN\n' TWICE HOOK xt!\n;package\ns\" FRAME-LIB:MARK\" 0 parse-imm\n" IN+
   s" lib.f" INPUT INPUT-U @ PUT ;

\ A rebuild after an edit to lib.f: the app still runs, both files still
\ write objects with every colon definition, under the keys of the first build.
: REBUILT ( ptr u8 n -- )
   T-LABEL
   PROBE SUCCESS
   OUT$ S\" use=67\n" CONTAINS? TTRUE
   s" app.f" DEFS$ s" 4" T$=
   s" lib.f" DEFS$ s" 5" T$=
   s" app.f" KEY$ APP-KEY KEY-LEN T$=
   s" lib.f" KEY$ LIB-KEY KEY-LEN T$=
   s" app.f" RESULTS$ APP-RES RES-LEN T$= ;

\ name's frame is unordered and holds one run and one data read.
: RAN ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   name nameu FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   name nameu FRAME$ s"  executed=" FIELD$ s" 1" T$=
   name nameu FRAME$ s"  data=" FIELD$ s" 1" T$= ;

\ HITS's address as the probe printed it, up to the space after it.
: HITS$ ( -- ptr u8 n ) OUT$ s" hits=" FIELD$ ;

20 constant DIGITS-CAP
create DIGITS DIGITS-CAP allot variable DIGITS-AT

\ n in decimal, appended to INPUT.
: N+ ( n -- )
   DIGITS-CAP DIGITS-AT !
   begin
      -1 DIGITS-AT +!
      dup 10 mod 48 + DIGITS DIGITS-AT @ + c!
      10 / dup 0=
   until drop
   DIGITS DIGITS-AT @ + DIGITS-CAP DIGITS-AT @ - IN+ ;

\ The span of definition name's name token in app.f, as "at-end", appended
\ to INPUT: what the origin journal records for it.
: SPAN+ ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   INPUT-U @ {: mark:n :}
   s" : " IN+ name nameu IN+ s"  " IN+
   APP$ INPUT mark + INPUT-U @ mark - FIND-SUB MATCH option
     none OF T-FAIL 0 ENDOF
     some OF IDX>N 2 + ENDOF
   ;MATCH {: at:n :}
   mark INPUT-U !
   at N+ s" -" IN+ at nameu + N+ ;

\ The source spans app.f's object holds, one per definition in order.
: APP-SPANS ( -- )
   0 INPUT-U !
   s" PRE" SPAN+ s" ," IN+
   s" IMM-USER" SPAN+ s" ," IN+
   s" FRAME-USE" SPAN+ s" ," IN+
   s" TALLY" SPAN+
   INPUT WANT INPUT-U @ BYTE-COPY INPUT-U @ WANT-U !
   s" app.f" OBJECT$ s"  src=" FIELD$ WANT WANT-U @ T$= ;

\ name's load wrote no object.
: NO-OBJECT ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   0 INPUT-U !
   name nameu IN+ s"  key=" IN+
   ERR$ INPUT INPUT-U @ FIND-SUB MATCH option
     none OF ENDOF
     some OF drop T-FAIL ENDOF
   ;MATCH ;

: FIXTURES ( -- )
   s" base.f" S\" package FRAME-BASE\npublic\n: ONE ( -- n ) 1 ;\n;package\n" PUT
   s" deep.f" S\" package FRAME-DEEP\npublic\n: TEN ( -- n ) 10 ;\n;package\n" PUT
   s" " s" dup +" s" 3 TWICE drop" LIB-SOURCE
   s" diag.f" S\" : NOCONTRACT-DIAG ( -- ) DIAGXT ;\n" PUT
   s" broken.f" S\" : NOCONTRACT-BROKEN ( -- n ) BROKEN ;\n" PUT
   s" after.f" S\" : AFTER-ONE ( -- n ) 1 ;\n" PUT
   s" exec.f" S\" require lib.f\nFRAME-LIB:SEVEN constant EXEC-SEVEN\n' FRAME-LIB:TWICE drop\nusing FRAME-LIB\nSEVEN constant EXEC-BARE-SEVEN\n;using\n" PUT
   \ peek.f's TRUSTED: PEEK reads its own data; run.f's top level runs it,
   \ safe.f's runs the checked SAFE-PEEK that calls it, and vread.f's reads
   \ the data word STORE itself.
   s" peek.f" S\" package FRAME-PEEK\npublic\nvariable STORE\nTRUSTED: PEEK ( -- n ) STORE @ ;\n: SAFE-PEEK ( -- n ) PEEK ;\n;package\n" PUT
   s" run.f" S\" require peek.f\nFRAME-PEEK:PEEK constant RUN-SEEN\n" PUT
   s" safe.f" S\" require peek.f\nFRAME-PEEK:SAFE-PEEK constant SAFE-SEEN\n" PUT
   s" vread.f" S\" require peek.f\nFRAME-PEEK:STORE @ constant STORE-SEEN\n" PUT
   \ watermark.f moves the user-slot watermark of the TASK the builder loaded at
   \ load time through the cell TASK hands its claimants. TASK:+USER moves it too
   \ and refuses its file for task-slots the same way.
   \ scope.f leaves its package open, the last load: a using it left open
   \ would not count, as usings end with their file (interpret.f CLEAN-END).
   s" watermark.f" S\" : SLOT-BUMP ( ptr u8 -- ) CELL-VIEW CELL swap +! ;\n' SLOT-BUMP TASK:OWNED-USER-NEXT\n" PUT
   s" scope.f" S\" package FRAME-SCOPE\n" PUT
   \ retire.f retires a record lib.f made.
   s" retire.f" S\" undefine FRAME-LIB:BUMP\n" PUT
   \ ret-own.f retires its own record in the segment that made it.
   \ ret-kid.f retires the record its parent ret-parent.f made before loading
   \ it. ret-late.f retires its own record after a child that uses it.
   s" ret-own.f" S\" : RET-OWN ( -- n ) 1 ;\nundefine RET-OWN\n" PUT
   s" ret-kid.f" S\" undefine RET-KID\n" PUT
   s" ret-parent.f" S\" : RET-KID ( -- n ) 2 ;\nrequire ret-kid.f\n" PUT
   s" ret-use.f" S\" : RET-USER ( -- n ) RET-LATE ;\n" PUT
   s" ret-late.f" S\" : RET-LATE ( -- n ) 3 ;\nrequire ret-use.f\nundefine RET-LATE\n" PUT
   \ incut.f cuts back to its own second definition, within its open segment.
   \ gc2.f cuts back to the first definition of gc1.f, which gp.f loaded
   \ before it: below gc2.f's open segment, through gp.f's load of gc1.f.
   \ k1.f cuts back to its own first definition, the ordinal of q1.f's load
   \ of it.
   s" incut.f" S\" : CA ( -- n ) 1 ;\n: CB ( -- n ) 2 ;\ns\" CB\" FORGET-DEFS-FROM\n" PUT
   s" gc1.f" S\" : G1 ( -- n ) 1 ;\n" PUT
   s" gc2.f" S\" s\" G1\" FORGET-DEFS-FROM\n" PUT
   s" gp.f" S\" require gc1.f\nrequire gc2.f\n" PUT
   s" k1.f" S\" : KA ( -- n ) 2 ;\ns\" KA\" FORGET-DEFS-FROM\n: KX ( -- n ) 3 ;\n" PUT
   s" q1.f" S\" : Q1 ( -- n ) 1 ;\nrequire k1.f\n: Q2 ( -- n ) KX ;\n" PUT
   \ raw.f's TRUSTED: RAW-SET stores a code address into the plain variable
   \ RAW-CELL, which declares no xt cell.
   s" raw.f" S\" variable RAW-CELL\n: RAW-TARGET ( -- ) ;\nTRUSTED: RAW-SET ( -- ) ['] RAW-TARGET RAW-CELL ! ;\nRAW-SET\n" PUT
   \ poke.f's TRUSTED: POKE stores a code address 8 bytes into board.f's
   \ 16-byte BOARD, an earlier cell no address row declares: the update check
   \ refuses it there. board.f loads in poke.f's session, above its image floor.
   s" board.f" S\" create BOARD 16 allot\n: BOARD-GO ( -- ) ;\n" PUT
   s" poke.f" S\" require board.f\nTRUSTED: POKE ( -- ) ['] BOARD-GO BOARD 8 + ! ;\nPOKE\n" PUT
   \ slot.f's declared xt cell holds deep.f's TEN, which nothing else in it
   \ compiles.
   s" slot.f" S\" TYPED-VARIABLE SLOT-HOOK [ -- n ]\n' FRAME-DEEP:TEN SLOT-HOOK xt!\n" PUT
   \ later.f's declared pointer cell holds an address 8 bytes into earlier.f's
   \ T, reached from U, so no run reads T; T-MID compiles a binding of T.
   s" earlier.f" S\" package FRAME-EARLY\npublic\ncreate T 16 allot\ncreate U 8 allot\n;package\n" PUT
   s" later.f" S\" require earlier.f\nPERSISTED-PTR-VARIABLE LATE-P\n: T-MID ( -- ptr u8 ) FRAME-EARLY:T 8 + ;\nFRAME-EARLY:U 8 - LATE-P !\n" PUT
   \ IMM-USER runs lib.f's immediate MARK while it compiles. app.f loads
   \ exec.f between its own definitions, and its top level runs TALLY, which
   \ adds 1 to lib.f's HITS, then stores lib.f's QUAD into lib.f's declared
   \ xt cell HOOK.
   s" app.f" APP$ PUT ;

: FRAME-CHECKS ( -- )
   OUT$ S\" use=67\n" CONTAINS? TTRUE
   OUT$ s" refused=1" CONTAINS? TTRUE
   s" diag.f binds DIAGXT through its handed-over contract" T-LABEL
   s" diag.f" FRAME$ s"  ordered=0 binding=1 value=0 resolution=2 executed=0 data=0" ENDS-WITH? TTRUE
   s" broken.f binds BROKEN's recovery record and stays ordered" T-LABEL
   s" broken.f" FRAME$
      s"  ordered=1 binding=0 value=0 resolution=2 executed=0 data=0 reason=binding-contract-unknown word=BROKEN"
      ENDS-WITH? TTRUE
   s" after.f, the first file after the run, stores its first definition" T-LABEL
   s" after.f" DEFS$ s" 1" T$=
   s" app.f consumes ONE, TWICE, TEN, SEVEN, HITS and MARK once each, SEVEN's value, MARK's run, HITS's data through TALLY's run and HOOK's run and data" T-LABEL
   \ The xt! line adds 5 resolution rows (FRAME-LIB:QUAD and FRAME-LIB:HOOK 2
   \ each, namespace and public, xt! 1 global), HOOK's run and its generated
   \ body's, and HOOK's data.
   s" app.f" FRAME$ s"  ordered=0 binding=6 value=1 resolution=34 executed=3 data=2" ENDS-WITH? TTRUE
   s" lib.f consumes ONE only, none of its own or deep.f's records" T-LABEL
   \ HOOK adds 12 resolution rows: TYPED-VARIABLE and xt! 3 each (global hit,
   \ FRAME-LIB public and private misses), the generated body's HOOK#base 4 (global
   \ and private misses, as spelled and folded) and the run of HOOK 2 (global and
   \ private misses). Each public hit is lib.f's own record and adds none.
   s" lib.f" FRAME$ s"  ordered=0 binding=1 value=0 resolution=40 executed=0 data=0" ENDS-WITH? TTRUE
   \ exec.f compiles nothing, so every resolution row is a run's: require and
   \ drop 1 each (global), FRAME-LIB:SEVEN and the ticked FRAME-LIB:TWICE 2 each
   \ (namespace, public), SEVEN under using 3 (namespace, public, global miss).
   \ A run that recorded only the leg holding its record would leave 5.
   s" exec.f only runs: SEVEN's value, TWICE's run and every leg of each run's resolution" T-LABEL
   s" exec.f" FRAME$ s"  ordered=0 binding=0 value=1 resolution=9 executed=1 data=0" ENDS-WITH? TTRUE
   s" run.f runs the trusted PEEK, which reads STORE: one run, one data read" T-LABEL
   s" run.f" RAN
   s" safe.f runs the checked SAFE-PEEK, which reaches PEEK and STORE" T-LABEL
   s" safe.f" RAN
   s" vread.f reads the data word STORE at top level" T-LABEL
   s" vread.f" FRAME$ s"  ordered=" FIELD$ s" 0" T$=
   s" vread.f" FRAME$ s"  executed=" FIELD$ s" 0" T$=
   s" vread.f" FRAME$ s"  data=" FIELD$ s" 1" T$= ;

: OBJECT-CHECKS ( -- )
   s" each file object holds its colon definitions, TRUSTED: BUMP included" T-LABEL
   s" base.f" DEFS$ s" 1" T$=
   s" deep.f" DEFS$ s" 1" T$=
   s" app.f" DEFS$ s" 4" T$=
   s" lib.f" DEFS$ s" 5" T$=
   s" each file object holds exactly its own input rows" T-LABEL
   s" base.f" s" 1" INPUTS
   s" deep.f" s" 1" INPUTS
   s" lib.f" s" 4" INPUTS
   s" app.f" s" 13" INPUTS
   s" each object is stored under its key" T-LABEL
   s" app.f" KEY$ APP-KEY KEY-LEN KEEP
   s" lib.f" KEY$ LIB-KEY KEY-LEN KEEP
   APP-KEY KEY-LEN STORED
   s" lib.f's object holds its records, HITS, T and HOOK's data and deep.f's load" T-LABEL
   s" lib.f" s"  records=" s" 10" OBJECT-HAS
   \ DATA: HITS 8 bytes, T 16, HOOK 8, and the 14 bytes of "FRAME-LIB:MARK"
   \ that its interpreted s" copies to HERE.
   s" lib.f" s"  data=" s" 46" OBJECT-HAS
   s" lib.f" s"  requires=" s" 1" OBJECT-HAS
   s" lib.f" s"  loads=" s" 1" OBJECT-HAS
   s" lib.f" s"  segments=" s" 2" OBJECT-HAS
   s" app.f's object holds six loads, the last between its own definitions" T-LABEL
   s" app.f" s"  records=" s" 4" OBJECT-HAS
   s" app.f" s"  requires=" s" 6" OBJECT-HAS
   s" app.f" s"  loads=" s" 6" OBJECT-HAS
   s" app.f" s"  segments=" s" 7" OBJECT-HAS
   s" base.f" s"  loads=" s" 0" OBJECT-HAS
   s" base.f" s"  segments=" s" 1" OBJECT-HAS
   s" lib.f's declared xt cell HOOK is its one slot" T-LABEL
   s" lib.f" s"  slots=" s" 1" OBJECT-HAS
   s" app.f's store into lib.f's HOOK is its one slot, binding HOOK and QUAD beyond its frame's rows" T-LABEL
   s" app.f" s"  slots=" s" 1" OBJECT-HAS
   s" app.f" OBJECT$ s"  rows=" FIELD$ FIELD-N
   s" app.f" FRAME$ s"  rows=" FIELD$ FIELD-N 2 + T=
   s" app.f's run of TALLY changes one byte of lib.f's HITS; lib.f writes no earlier DATA" T-LABEL
   s" app.f" s"  updates=" s" 1" OBJECT-HAS
   s" lib.f" s"  updates=" s" 0" OBJECT-HAS
   s" raw.f's undeclared code address refuses its object, naming the cell" T-LABEL
   s" raw.f" s"  reason=" LINE$
      s" raw.f reason=raw-cell owner=RAW-CELL offset=0 value=0x" STARTS-WITH? TTRUE
   ERR$ s" raw.f key=" CONTAINS? TFALSE
   s" poke.f's code address in earlier DATA refuses its object, naming the cell" T-LABEL
   s" poke.f" s"  reason=" LINE$
      s" poke.f reason=raw-cell owner=BOARD offset=8 value=0x" STARTS-WITH? TTRUE
   ERR$ s" poke.f key=" CONTAINS? TFALSE
   s" slot.f's slot names TEN, so its object binds TEN beyond its frame's rows" T-LABEL
   s" slot.f" s"  slots=" s" 1" OBJECT-HAS
   s" slot.f" FRAME$ s"  binding=" FIELD$ s" 0" T$=
   s" slot.f" OBJECT$ s"  rows=" FIELD$ FIELD-N
   s" slot.f" FRAME$ s"  rows=" FIELD$ FIELD-N 1+ T=
   \ The frame binds T and reads U's DATA. The slot adds T's DATA row, whose
   \ digest covers T's extent, so an edit that moves T's end is stale; a
   \ binding of T alone would add no row.
   s" later.f's slot points into T, so its object reads T's DATA beyond its frame's rows" T-LABEL
   s" later.f" s"  slots=" s" 1" OBJECT-HAS
   s" later.f" FRAME$ s"  binding=" FIELD$ s" 1" T$=
   s" later.f" FRAME$ s"  data=" FIELD$ s" 1" T$=
   s" later.f" OBJECT$ s"  rows=" FIELD$ FIELD-N
   s" later.f" FRAME$ s"  rows=" FIELD$ FIELD-N 1+ T=
   s" retire.f refuses its object for retiring lib.f's BUMP" T-LABEL
   s" retire.f" s"  reason=" LINE$ s" retire.f reason=retires-earlier word=BUMP" T$=
   s" a file that retires its own record in the segment that made it stores it" T-LABEL
   s" ret-own.f" KEY$ STORED
   s" a child that retires its parent's record refuses both files" T-LABEL
   s" ret-kid.f" s"  reason=" LINE$ s" ret-kid.f reason=retires-earlier word=RET-KID" T$=
   s" ret-kid.f" NO-OBJECT
   s" ret-parent.f" s"  reason=" LINE$ s" ret-parent.f reason=retires-earlier word=RET-KID" T$=
   s" ret-parent.f" NO-OBJECT
   s" a file that retires its record after a child that uses it refuses" T-LABEL
   s" ret-late.f" s"  reason=" LINE$ s" ret-late.f reason=retires-earlier word=RET-LATE" T$=
   s" ret-late.f" NO-OBJECT
   s" a file that cuts within its open segment stores its object" T-LABEL
   s" incut.f" KEY$ STORED
   s" a cut below the innermost file's open segment refuses it and the file that loaded the cut records" T-LABEL
   s" gc2.f" s"  reason=" LINE$ s" gc2.f reason=cut-below-segment" T$=
   s" gc2.f" NO-OBJECT
   s" gp.f" s"  reason=" LINE$ s" gp.f reason=cut-below-segment" T$=
   s" gp.f" NO-OBJECT
   s" a cut at a child's first ordinal refuses the child and the parent whose load it drops" T-LABEL
   s" k1.f" s"  reason=" LINE$ s" k1.f reason=cut-below-segment" T$=
   s" k1.f" NO-OBJECT
   s" q1.f" s"  reason=" LINE$ s" q1.f reason=cut-below-segment" T$=
   s" q1.f" NO-OBJECT
   s" a file that ends in another scope than it began in has no object" T-LABEL
   s" scope.f" s"  reason=" LINE$ s" scope.f reason=scope-exit" T$=
   s" scope.f" NO-OBJECT
   s" a file that takes a TASK user slot has no object" T-LABEL
   s" watermark.f" s"  reason=" LINE$ s" watermark.f reason=task-slots" T$=
   s" watermark.f" NO-OBJECT
   s" app.f's object holds each definition's name span in app.f" T-LABEL
   APP-SPANS ;

public
: MAIN ( -- )
   T-RESET
   s" native-file-frame" TREE
   s" cache" AT MAKE-DIR
   FIXTURES
   \ Each build below is its own builder process: a key that stays stable
   \ across separate builds is the boundary under test.
   PROBE {: rc:n :}
   s" probe-out.log" OUT$ PUT
   s" probe-err.log" ERR$ PUT
   rc SUCCESS
   FRAME-CHECKS
   OBJECT-CHECKS
   LIB-KEY KEY-LEN STORED
   s" app.f" RESULTS$ APP-RES RES-LEN KEEP
   s" lib.f" RESULTS$ LIB-RES RES-LEN KEEP
   HITS$ {: at:ptr atu:n :}
   atu AT-CAP > if T-FAIL else at HITS-AT atu BYTE-COPY atu HITS-AT-U ! then
   \ A comment changes no definition's result, inside TWICE's body or as a
   \ line that moves every later definition. base.f's new variable moves HITS
   \ from here on.
   s" base.f" S\" package FRAME-BASE\nvariable GAP\npublic\n: ONE ( -- n ) 1 ;\n;package\n" PUT
   S\" \\ edited\n" s" ( note ) dup +" s" 3 ( note ) TWICE drop" LIB-SOURCE
   s" comments in lib.f keep both keys and both files' results" REBUILT
   s" lib.f" RESULTS$ LIB-RES RES-LEN T$=
   \ TWICE's new code changes lib.f's results. An empty cache makes app.f
   \ compile again against lib.f's moved code and HITS's moved address, so
   \ new branch words land in its call sites and TALLY captures the new
   \ address; masked, they keep app.f's results. A warm app.f would hand
   \ back its stored bytes and prove nothing about the mask.
   s" cache" AT REMOVE-TREE s" cache" AT MAKE-DIR
   s" " s" 2 *" s" 3 TWICE drop" LIB-SOURCE
   s" a changed TWICE body keeps both keys and app.f's results" REBUILT
   s" lib.f" RESULTS$ LIB-RES RES-LEN STR= TFALSE
   s" base.f's new variable moved HITS" T-LABEL
   HITS$ HITS-AT HITS-AT-U @ STR= TFALSE
   T-REPORT
   s" file frame artifacts: " type ROOT$ type cr ;

;package
;using

NATIVE-FILE-FRAME-TEST:MAIN
