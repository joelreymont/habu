\ Definition reuse preserves nominal, linear and deferred declarations.
\ bin/hb --load test/native-file-types-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require src/core/sha256.f
require test/native-file-app-lib.f
require test/native-file-e2e.f
require tools/native-unit-object.f

using NATIVE-FILE-E2E
package NATIVE-FILE-TYPES-TEST

create HASH-CTX SHA256-FILE-CTX-BYTES allot
create WARM-HASH 32 allot
create COLD-HASH 32 allot
create SOURCE-PATH FS-PATH-CAP allot
create PRODUCT-PATH FS-PATH-CAP allot
create PARTIAL-REFS 1024 allot
create LIVE-KEY 256 allot
create PENDING-KEY 256 allot
variable LIVE-KEY-U
variable PENDING-KEY-U
variable LIVE-ORD
variable REFS-U
create PROBE-IN 1024 allot
variable PROBE-IN-U
create EXPECT 512 allot
variable EXPECT-U

: REFS-RESET ( -- )
   1024 0 ?do 0 PARTIAL-REFS i + c! loop
   1 PARTIAL-REFS 16 + LE:U64!
   24 REFS-U ! ;

: REF-DECL ( ptr u8 n n n -- )
   {: key:ptr keyu:n idx:n contractu:n :}
   PARTIAL-REFS REFS-U @ + {: row:ptr :}
   idx row LE:U64! keyu row 8 + LE:U64!
   contractu row 16 + LE:U64!
   key row 24 + keyu BYTE-COPY
   24 keyu + REFS-U +! ;

: PREPARE-REFS ( -- )
   PARTIAL-REFS REFS-U @ ndict@ cp@ 0 NFILE-REF:PREPARE ;

: LIVE-BOUND ( -- )
   LIVE-KEY LIVE-KEY-U @ NFILE-REF:KEY-RECORD nip LIVE-ORD @ T= ;

: PENDING-REFS ( -- )
   NFILE-REF:CLOSE
   ['] XREF-REC NFILE-REF:RECORD-AT!
   s" partial-projection" NFILE-REF:FILE-INSTANCE!
   s" CELL" XREF-FIND-INDEX LIVE-ORD !
   LIVE-ORD @ NFILE-REF:ON-PUBLISHED
   LIVE-ORD @ NFILE-REF:DECL-KEY$ {: live:ptr liveu:n :}
   live LIVE-KEY liveu BYTE-COPY liveu LIVE-KEY-U !
   NFILE-REF:PENDING-KEY$ {: pending:ptr pendingu:n :}
   pending PENDING-KEY pendingu BYTE-COPY pendingu PENDING-KEY-U !
   REFS-RESET ;

\ Malformed section 13 must neither replace a live key nor change a pending
\ declaration's identity across retries. E2E family edits refuse after parsing.
: PARTIAL-PROJECTION ( -- )
   s" malformed definition projections preserve live and pending keys" T-LABEL
   ['] XREF-REC NFILE-REF:RECORD-AT!
   NUNIT-CAPTURE:BIND-PRODUCER
   REFS-RESET
   \ The first row has no key; no record table has grown to ndict@ yet.
   48 REFS-U !
   ['] PREPARE-REFS E-NUNIT-PROFILE TTHROWSQ
   NUNIT-OBJECT:DISCARD-DEFINITION
   PENDING-REFS
   0 PARTIAL-REFS 16 + LE:U64!
   ['] PREPARE-REFS E-NUNIT-PROFILE TTHROWSQ
   NUNIT-OBJECT:DISCARD-DEFINITION
   LIVE-BOUND
   PENDING-REFS
   2 PARTIAL-REFS 16 + LE:U64!
   PENDING-KEY PENDING-KEY-U @ 0 0 REF-DECL
   LIVE-KEY LIVE-KEY-U @ 1 0 REF-DECL
   ['] PREPARE-REFS E-NUNIT-PROFILE TTHROWSQ
   NUNIT-OBJECT:DISCARD-DEFINITION
   LIVE-BOUND
   PENDING-REFS
   \ Bind the one admitted row, then refuse its absent contract bytes.
   PENDING-KEY PENDING-KEY-U @ 0 1 REF-DECL
   ['] PREPARE-REFS E-NUNIT-PROFILE TTHROWSQ
   NUNIT-OBJECT:DISCARD-DEFINITION
   LIVE-BOUND
   REFS-RESET
   LIVE-KEY LIVE-KEY-U @ 0 0 REF-DECL
   ['] PREPARE-REFS E-NUNIT-PROFILE TTHROWSQ
   NUNIT-OBJECT:DISCARD-DEFINITION
   LIVE-BOUND
   REFS-RESET
   PENDING-KEY PENDING-KEY-U @ 0 0 REF-DECL
   PREPARE-REFS
   ndict@ NFILE-REF:DECL-KEY$ PENDING-KEY PENDING-KEY-U @ T$=
   NUNIT-OBJECT:DISCARD-DEFINITION
   LIVE-BOUND
   NUNIT-OBJECT:CLOSE ;

: SHAPE-SOURCE ( bool -- )
   {: wide:bool :}
   s" types.f"
      S\" package FILE-SHAPE public\nSTRUCTURE scratch OPAQUE DERIVE addr\nFIELD a n\n"
      PUT
   wide if s" types.f" AT S\" FIELD walk n\n" APPEND-FILE then
   s" types.f" AT
      S\" ;STRUCTURE\nSTRUCTURE context OPAQUE DERIVE addr\nFIELD scratch scratch\n;STRUCTURE\nSTRUCTURE client OPAQUE DERIVE addr\nFIELD ctx ptr context\n;STRUCTURE\n: SIZE ( -- n ) CONTEXT-BYTES ;\n: VALUE ( ptr context -- n ) CONTEXT-SCRATCH "
      APPEND-FILE
   s" types.f" AT
      wide if s" SCRATCH-WALK" else s" SCRATCH-A" then APPEND-FILE
   s" types.f" AT
      S\"  dup 42 swap ! @ ;\nPRODUCT request FIELD first n\n" APPEND-FILE
   wide if s" types.f" AT S\" FIELD second n\n" APPEND-FILE then
   s" types.f" AT S\" ;PRODUCT\n: REQUEST ( -- request ) 1 " APPEND-FILE
   wide if s" types.f" AT s" 2 " APPEND-FILE then
   s" types.f" AT S\" FILE--SHAPE-REQUEST:MAKE ;\n;package\n" APPEND-FILE ;

: HASH-APP ( ptr u8 -- )
   {: digest:ptr :}
   HASH-CTX s" app-hb" AT digest SHA256-FILE-IN 0 T= ;

: SAVE-APP ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   ROOT$ s" app-hb" SOURCE-PATH JOIN-PATH SOURCE-PATH swap
   name nameu AT 0 NATIVE-FILE-APP:COPY-SKIP-LINES ;

: BUILD-FRESH ( ptr u8 n -- n )
   {: cache:ptr cacheu:n :}
   cache cacheu AT MAKE-DIR
   ROOT$ s" app.f" SOURCE-PATH JOIN-PATH SOURCE-PATH swap
   ROOT$ s" app-hb" PRODUCT-PATH JOIN-PATH PRODUCT-PATH swap
   cache cacheu AT NATIVE-FILE-APP:BUILD
   NATIVE-FILE-APP:OUT$ NATIVE-FILE-APP:ERR$ STREAMS! ;

: BUILD ( ptr u8 n -- n )
   {: product:ptr productu:n :}
   ROOT$ s" app.f" product productu NATIVE-FILE-APP:BUILD-AT
   NATIVE-FILE-APP:OUT$ NATIVE-FILE-APP:ERR$ STREAMS! ;

: PREPARE ( -- )
   s" cache" AT MAKE-DIR
   s" test/fixtures/native-file-state-prefix.f" s" prefix.f" AT 1
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" test/fixtures/native-file-types-unit.f" s" unit.f" AT 1
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" unit.f" AT S\" s\" types-unit-source-ran\" type cr\n" APPEND-FILE
   s" test/fixtures/native-file-types-downstream.f" s" downstream.f" AT 1
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" test/fixtures/native-file-types-rejected.f" s" bad.f" AT 0
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" app.f" S\" require prefix.f\nrequire unit.f\nrequire downstream.f\n: MAIN ( -- ) FILE-TYPES-CLIENT:RESULT . FILE-TYPES:SAVED-VALUE . ;\n" PUT ;

: FRESH-DOWNSTREAM ( -- )
   s" test/fixtures/native-file-types-downstream.f" s" fresh.f" AT 1
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" app.f" S\" require prefix.f\nrequire unit.f\nrequire fresh.f\n: MAIN ( -- ) FILE-TYPES-CLIENT:RESULT . FILE-TYPES:SAVED-VALUE . ;\n" PUT ;

: SHIFT-PREFIX ( -- )
   s" prefix.f" AT
   S\" package FILE-SHIFT\npublic\nNEWTYPE unrelated\n;package\n" APPEND-FILE ;

: RUN-IMPORTED ( -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" warm-hb" AT s" " CAPTURE SUCCESS
   OUT$ S\" 9\n456\n" CONTAINS? TTRUE ;

: IN+ ( ptr u8 n -- ) PROBE-IN 1024 PROBE-IN-U BUF-APPEND ;

\ A REPL line that loads the tree's named file through the cache.
: LOAD-LINE ( ptr u8 n -- )
   S\" s\" " IN+ AT IN+ S\" \" s\" repl\" NDEF-CACHE:LOAD\n" IN+ ;

\ An app is a snapshot of its builder, which ran a cache session, and boots
\ to a REPL; the boot is the boundary under test, so each run is a process.
: PROBE-RUN ( ptr u8 n -- )
   {: exe:ptr exeu:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" probe-cache" AT >LEN PROC-ENV+
   s" HABU_NATIVE_CACHE_DIAG" >LEN s" 1" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   exe exeu AT PROBE-IN PROBE-IN-U @ CAPTURE SUCCESS ;

\ Each boot takes its own image floor, but running a new definition that
\ reaches an unclassified checked image seed refuses storage: the named file
\ is refused at word, no earlier DATA copied, no object stored.
: SEED-REFUSED ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n word:ptr wordu:n :}
   0 EXPECT-U !
   s" native-file: refused path=" EXPECT 512 EXPECT-U BUF-APPEND
   name nameu AT EXPECT 512 EXPECT-U BUF-APPEND
   s"  reason=run-reaches-kernel-word word=" EXPECT 512 EXPECT-U BUF-APPEND
   word wordu EXPECT 512 EXPECT-U BUF-APPEND
   S\" \n" EXPECT 512 EXPECT-U BUF-APPEND
   ERR$ EXPECT EXPECT-U @ CONTAINS? TTRUE
   ERR$ S\"  copied=0\n" CONTAINS? TTRUE
   0 EXPECT-U !
   s" native-file: object path=" EXPECT 512 EXPECT-U BUF-APPEND
   name nameu AT EXPECT 512 EXPECT-U BUF-APPEND
   s"  key=" EXPECT 512 EXPECT-U BUF-APPEND
   ERR$ EXPECT EXPECT-U @ CONTAINS? TFALSE ;

\ The app's session runs probe.f, then the app saves itself; the second image
\ boots and runs probe2.f, which calls probe.f's word. Each boot takes its own
\ image floor; both runs reach a checked seed of the booted image.
: BOOTED-SESSION ( -- )
   s" a booted app's session refuses a file that reaches an image seed" T-LABEL
   s" probe.f"
      S\" package FILE-PROBE\npublic\nvariable CELLV\n: VALUE ( -- n ) FILE-SHAPE:SIZE 61 + ;\n;package\nFILE-PROBE:VALUE . cr\n"
      PUT
   s" probe2.f"
      S\" package FILE-PROBE2\npublic\n: V2 ( -- n ) FILE-PROBE:VALUE 1+ ;\n;package\nFILE-PROBE2:V2 . cr\n"
      PUT
   0 PROBE-IN-U !
   S\" 1 set-tier\n" IN+
   s" probe.f" LOAD-LINE
   S\" s\" " IN+ s" app2" AT IN+ S\" \" APP-IMAGE:SAVE\n" IN+
   s" app-hb" PROBE-RUN
   OUT$ S\" 77\n" CONTAINS? TTRUE
   s" probe.f" s" SIZE" SEED-REFUSED
   0 PROBE-IN-U !
   S\" 1 set-tier\n" IN+
   s" probe2.f" LOAD-LINE
   s" app2" PROBE-RUN
   OUT$ S\" 78\n" CONTAINS? TTRUE
   s" probe2.f" s" VALUE" SEED-REFUSED ;

\ A capture whose one-shot hook throws returns to the REPL with the process's
\ seed and image floor intact: probe3.f writes probe.f's cell, an earlier
\ session's DATA, so it is refused and its image copies that DATA. A seed and
\ floor lost to the failed capture would take probe.f as seed and store it.
: FAILED-CAPTURE ( -- )
   s" a capture that throws keeps the process's seed and image floor" T-LABEL
   s" probe3.f"
      S\" package FILE-PROBE3\npublic\n99 FILE-PROBE:CELLV !\n: V3 ( -- n ) FILE-PROBE:CELLV @ ;\n;package\nFILE-PROBE3:V3 . cr\n"
      PUT
   0 PROBE-IN-U !
   S\" 1 set-tier\n: BOOM ( -- ) 5 throw ;\n' BOOM IMAGE-LIFECYCLE:REGISTER\n" IN+
   s" probe.f" LOAD-LINE
   S\" : SAVE3 ( -- ) [: s\" " IN+ s" app3" AT IN+
   S\" \" APP-IMAGE:SAVE ;] catch 5 = if s\" save-refused\" type cr then ;\nSAVE3\n" IN+
   s" probe3.f" LOAD-LINE
   s" app-hb" PROBE-RUN
   OUT$ S\" save-refused\n" CONTAINS? TTRUE
   s" app3" AT EXISTS? TFALSE
   OUT$ S\" 99\n" CONTAINS? TTRUE
   0 EXPECT-U !
   s" native-file: refused path=" EXPECT 512 EXPECT-U BUF-APPEND
   s" probe3.f" AT EXPECT 512 EXPECT-U BUF-APPEND
   ERR$ EXPECT EXPECT-U @ CONTAINS? TTRUE
   0 EXPECT-U !
   s" native-file: image path=" EXPECT 512 EXPECT-U BUF-APPEND
   s" probe3.f" AT EXPECT 512 EXPECT-U BUF-APPEND
   S\"  copied=0\n" EXPECT 512 EXPECT-U BUF-APPEND
   ERR$ EXPECT EXPECT-U @ CONTAINS? TFALSE ;

: REJECT-TYPED-CLIENT ( -- )
   s" app.f" S\" require prefix.f\nrequire unit.f\nrequire bad.f\n: MAIN ( -- ) ;\n" PUT
   s" bad-hb" BUILD 70 T=
   OUT$ s" types-unit-source-ran" CONTAINS? TTRUE
   ERR$ s" expected: n actual: file-types:item" CONTAINS? TTRUE ;

: SHAPE-CASE ( -- )
   s" a nested family edit invalidates generated storage definitions" T-LABEL
   s" native-file-types-shape" TREE
   s" cache" AT MAKE-DIR
   false SHAPE-SOURCE
   \ Width frames, checked deferred binding and logical-immediate encoding must
   \ leave the same captured bytes when their bodies are reused from the cache.
   s" storage.f"
      S\" require types.f\npackage FILE-STORAGE public\nSTRUCTURE pair<a,b> FIELD first a FIELD second b ;STRUCTURE\n2 TYPED-BUFFER PAIRS pair<n,FILE-SHAPE:context>\n2 TYPED-BUFFER CONTEXTS FILE-SHAPE:context\n2 TYPED-BUFFER CLIENTS FILE-SHAPE:client\ndefer FILTER ( n -- n )\n: BIND ( -- ) [: $FF and ;] is FILTER ;\n: RESULT ( -- n ) BIND 1 PAIRS drop 1 CLIENTS drop 1 CONTEXTS FILE-SHAPE:VALUE FILTER ;\n: ASK ( FILE-SHAPE:request [ FILE-SHAPE:request -- n ] -- n ) execute ;\n: ANSWER ( FILE-SHAPE:request -- n ) {: request :} 43 ;\n: CALLBACK-VALUE ( -- n ) FILE-SHAPE:REQUEST ['] ANSWER ASK ;\n;package\n"
      PUT
   s" app.f" S\" require storage.f\n: MAIN ( -- ) FILE-STORAGE:RESULT . FILE-SHAPE:SIZE . FILE-STORAGE:CALLBACK-VALUE . ;\n" PUT
   s" app-hb" BUILD SUCCESS
   RUN-APP SUCCESS OUT$ S\" 42\n8\n43\n" CONTAINS? TTRUE
   true SHAPE-SOURCE
   s" app-hb" BUILD SUCCESS
   OUT$ S\" files reused 0\n" CONTAINS? TFALSE
   RUN-APP SUCCESS OUT$ S\" 42\n16\n43\n" CONTAINS? TTRUE
   COLD-HASH HASH-APP
   s" cold-app-hb" SAVE-APP
   s" app-hb" BUILD SUCCESS
   OUT$ s" files reused " CONTAINS? TTRUE
   OUT$ S\" files reused 0\n" CONTAINS? TFALSE
   RUN-APP SUCCESS OUT$ S\" 42\n16\n43\n" CONTAINS? TTRUE
   WARM-HASH HASH-APP
   COLD-HASH 32 WARM-HASH 32 T$=
   s" warm-app-hb" SAVE-APP
   s" fresh-cache" BUILD-FRESH SUCCESS
   RUN-APP SUCCESS OUT$ S\" 42\n16\n43\n" CONTAINS? TTRUE
   COLD-HASH HASH-APP
   WARM-HASH 32 COLD-HASH 32 T$= ;

\ b.f declares empty ranges after a.f, whose field and schema stores take k
\ rows: a fieldless STRUCTURE's field range (TFAM-FLD-RANGE!; it has no MAKE
\ or UNMAKE), a nullary variant's schema range (SUMV-ADD), the four empty
\ sides of `[ -- ]` (SCHEMA-ROW) and the empty argument range of `mark<>`
\ (SCHEMA-APP). Each is stored as start 0 count 0; the sums are private, so
\ they publish no constructor word and b.f stays reusable. Editing a.f shifts
\ those stores under b.f's reused object, down and then up from its capture:
\ each warm build reuses b.f without a throw and its app prints what a
\ fresh-cache build's app prints. An empty range stored at the store's
\ high-water instead makes the warm build throw 76.
: WIDTH-SOURCE ( n -- ) {: k:n :}
   s" a.f" S\" package FILE-WIDE public\nSTRUCTURE wide OPAQUE DERIVE addr FIELD a n\n" PUT
   k 1 > if s" a.f" AT S\" FIELD b n\n" APPEND-FILE then
   k 2 > if s" a.f" AT S\" FIELD c n\n" APPEND-FILE then
   s" a.f" AT S\" ;STRUCTURE\n: SIZE ( -- n ) WIDE-BYTES ;\n;package\n" APPEND-FILE ;

: EMPTY-STEP ( n ptr u8 n ptr u8 n -- )
   {: k:n out:ptr outu:n fresh:ptr freshu:n :}
   k WIDTH-SOURCE
   s" app-hb" BUILD SUCCESS
   OUT$ S\" files reused 1\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ out outu CONTAINS? TTRUE
   fresh freshu BUILD-FRESH SUCCESS
   RUN-APP SUCCESS OUT$ out outu CONTAINS? TTRUE ;

: EMPTY-RANGE-CASE ( -- )
   s" a reused empty family range survives store shifts in an earlier file" T-LABEL
   s" native-file-types-empty" TREE
   s" cache" AT MAKE-DIR
   s" b.f"
      S\" package FILE-TAGS\nSUMTYPE mark VARIANT none ;VARIANT ;SUMTYPE\nSUMTYPE hook VARIANT call [ -- ] ;VARIANT VARIANT wrap mark<> ;VARIANT ;SUMTYPE\npublic\nSTRUCTURE empty ;STRUCTURE\n: KEEP ( ptr empty -- ptr empty ) ;\n: CODE ( -- n ) 32 ;\n;package\n"
      PUT
   s" app.f" S\" require a.f\nrequire b.f\n: MAIN ( -- ) FILE-WIDE:SIZE . FILE-TAGS:CODE . ;\n" PUT
   2 WIDTH-SOURCE
   s" app-hb" BUILD SUCCESS
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ S\" 16\n32\n" CONTAINS? TTRUE
   1 S\" 8\n32\n" s" fresh-narrow" EMPTY-STEP
   3 S\" 24\n32\n" s" fresh-wide" EMPTY-STEP ;

public
: MAIN ( -- )
   T-RESET
   PARTIAL-PROJECTION
   s" FILE round trip restores nominal, linear and deferred checker state" T-LABEL
   s" native-file-types" TREE
   PREPARE
   s" cold-hb" BUILD SUCCESS
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   OUT$ s" types-unit-source-ran" CONTAINS? TTRUE
   s" warm-hb" BUILD SUCCESS
   OUT$ s" files reused " CONTAINS? TTRUE
   OUT$ S\" files reused 0\n" CONTAINS? TFALSE
   OUT$ s" types-unit-source-ran" CONTAINS? TTRUE
   RUN-IMPORTED
   FRESH-DOWNSTREAM
   s" fresh-hb" BUILD SUCCESS
   OUT$ s" types-unit-source-ran" CONTAINS? TTRUE
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" fresh-hb" AT s" " CAPTURE SUCCESS
   OUT$ S\" 9\n456\n" CONTAINS? TTRUE
   SHIFT-PREFIX
   s" shifted-hb" BUILD SUCCESS
   OUT$ s" types-unit-source-ran" CONTAINS? TTRUE
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" shifted-hb" AT s" " CAPTURE SUCCESS
   OUT$ S\" 9\n456\n" CONTAINS? TTRUE
   REJECT-TYPED-CLIENT
   SHAPE-CASE
   BOOTED-SESSION
   FAILED-CAPTURE
   EMPTY-RANGE-CASE
   T-REPORT
   s" native file unit artifacts: " type ROOT$ type cr ;
;package
;using

NATIVE-FILE-TYPES-TEST:MAIN
