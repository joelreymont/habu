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
require tools/native-definition-cache.f
require tools/native-unit-object.f

using NATIVE-FILE-E2E
package NATIVE-FILE-TYPES-TEST

create HASH-CTX SHA256-FILE-CTX-BYTES allot
create WARM-HASH 32 allot
create COLD-HASH 32 allot
create SOURCE-PATH PATH-CAP allot
create PRODUCT-PATH PATH-CAP allot
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
   wide if s" types.f" AT S\" FIELD walk n\n" FS:APPEND-FILE then
   s" types.f" AT
      S\" ;STRUCTURE\nSTRUCTURE context OPAQUE DERIVE addr\nFIELD scratch scratch\n;STRUCTURE\nSTRUCTURE client OPAQUE DERIVE addr\nFIELD ctx ptr context\n;STRUCTURE\n: SIZE ( -- n ) CONTEXT-BYTES ;\n: VALUE ( ptr context -- n ) CONTEXT-SCRATCH "
      FS:APPEND-FILE
   s" types.f" AT
      wide if s" SCRATCH-WALK" else s" SCRATCH-A" then FS:APPEND-FILE
   s" types.f" AT
      S\"  dup 42 swap ! @ ;\nPRODUCT request FIELD first n\n" FS:APPEND-FILE
   wide if s" types.f" AT S\" FIELD second n\n" FS:APPEND-FILE then
   s" types.f" AT S\" ;PRODUCT\n: REQUEST ( -- request ) 1 " FS:APPEND-FILE
   wide if s" types.f" AT s" 2 " FS:APPEND-FILE then
   s" types.f" AT S\" FILE--SHAPE-REQUEST:MAKE ;\n;package\n" FS:APPEND-FILE ;

: HASH-APP ( ptr u8 -- )
   {: digest:ptr :}
   HASH-CTX s" app-hb" AT digest SHA256-FILE-IN 0 T= ;

: SAVE-APP ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   ROOT$ s" app-hb" SOURCE-PATH FS:JOIN-PATH SOURCE-PATH swap
   name nameu AT 0 NATIVE-FILE-APP:COPY-SKIP-LINES ;

: BUILD-FRESH ( ptr u8 n -- n )
   {: cache:ptr cacheu:n :}
   cache cacheu AT FS:MAKE-DIR
   ROOT$ s" app.f" SOURCE-PATH FS:JOIN-PATH SOURCE-PATH swap
   ROOT$ s" app-hb" PRODUCT-PATH FS:JOIN-PATH PRODUCT-PATH swap
   cache cacheu AT NATIVE-FILE-APP:BUILD
   NATIVE-FILE-APP:OUT$ NATIVE-FILE-APP:ERR$ STREAMS! ;

: BUILD ( ptr u8 n -- n )
   {: product:ptr productu:n :}
   ROOT$ s" app.f" product productu NATIVE-FILE-APP:BUILD-AT
   NATIVE-FILE-APP:OUT$ NATIVE-FILE-APP:ERR$ STREAMS! ;

: PREPARE ( -- )
   s" cache" AT FS:MAKE-DIR
   s" test/fixtures/native-file-state-prefix.f" s" prefix.f" AT 1
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" test/fixtures/native-file-types-unit.f" s" unit.f" AT 1
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" unit.f" AT S\" s\" types-unit-source-ran\" type cr\n" FS:APPEND-FILE
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
   S\" package FILE-SHIFT\npublic\nNEWTYPE unrelated\n;package\n" FS:APPEND-FILE ;

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
   s" HABU_OUT" >LEN s" probe-cache" AT >LEN PROC-ENV+
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
   s" app3" AT FS:EXISTS? TFALSE
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
   s" cache" AT FS:MAKE-DIR
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
\ each warm build reuses b.f without a throw (and replays app.f, which loads
\ a.f and b.f, around them), its app prints what a
\ fresh-cache build's app prints, and its image holds the same bytes: an
\ image depends only on persistent program state. An empty range stored at
\ the store's high-water instead makes the warm build throw 76.
: WIDTH-SOURCE ( n -- ) {: k:n :}
   s" a.f" S\" package FILE-WIDE public\nSTRUCTURE wide OPAQUE DERIVE addr FIELD a n\n" PUT
   k 1 > if s" a.f" AT S\" FIELD b n\n" FS:APPEND-FILE then
   k 2 > if s" a.f" AT S\" FIELD c n\n" FS:APPEND-FILE then
   s" a.f" AT S\" ;STRUCTURE\n: SIZE ( -- n ) WIDE-BYTES ;\n;package\n" FS:APPEND-FILE ;

: EMPTY-STEP ( n ptr u8 n ptr u8 n -- )
   {: k:n out:ptr outu:n fresh:ptr freshu:n :}
   k WIDTH-SOURCE
   s" app-hb" BUILD SUCCESS
   OUT$ S\" files reused 2\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ out outu CONTAINS? TTRUE
   WARM-HASH HASH-APP
   s" shift-warm-hb" SAVE-APP
   fresh freshu BUILD-FRESH SUCCESS
   RUN-APP SUCCESS OUT$ out outu CONTAINS? TTRUE
   COLD-HASH HASH-APP
   s" shift-fresh-hb" SAVE-APP
   WARM-HASH 32 COLD-HASH 32 T$= ;

: EMPTY-RANGE-CASE ( -- )
   s" a reused empty family range survives store shifts in an earlier file" T-LABEL
   s" native-file-types-empty" TREE
   s" cache" AT FS:MAKE-DIR
   s" b.f"
      S\" package FILE-TAGS\nSUMTYPE mark VARIANT none ;VARIANT ;SUMTYPE\nSUMTYPE hook VARIANT call [ -- ] ;VARIANT VARIANT wrap mark<> ;VARIANT ;SUMTYPE\npublic\nSTRUCTURE empty ;STRUCTURE\n: KEEP ( ptr empty -- ptr empty ) ;\n: CODE ( -- n ) 32 ;\n;package\n"
      PUT
   s" app.f" S\" require a.f\nrequire b.f\n: MAIN ( -- ) FILE-WIDE:SIZE . FILE-TAGS:CODE . ;\n" PUT
   2 WIDTH-SOURCE
   s" app-hb" BUILD SUCCESS
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ S\" 16\n32\n" CONTAINS? TTRUE
   \ The first build ran on an empty cache, so it is the fresh-cache build
   \ the build that reuses every file, app.f, a.f and b.f, is compared against.
   COLD-HASH HASH-APP
   s" hit-fresh-hb" SAVE-APP
   s" app-hb" BUILD SUCCESS
   OUT$ S\" files reused 3\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ S\" 16\n32\n" CONTAINS? TTRUE
   WARM-HASH HASH-APP
   s" hit-warm-hb" SAVE-APP
   COLD-HASH 32 WARM-HASH 32 T$=
   1 S\" 8\n32\n" s" fresh-narrow" EMPTY-STEP
   3 S\" 24\n32\n" s" fresh-wide" EMPTY-STEP ;

\ A reused file that declares private families in a package (an ENUM and a
\ derived STRUCTURE) leaves the state its cold load left: the warm build's
\ image holds the bytes of a fresh-cache build of the same tree. The first
\ build runs on an empty cache, so it is that fresh-cache build.
: FAMILY-HIT-CASE ( -- )
   s" a reused file of private families leaves the cold image" T-LABEL
   s" native-file-types-family" TREE
   s" cache" AT FS:MAKE-DIR
   s" decl.f"
      S\" package PZ private\nENUM shade VARIANT red ;VARIANT VARIANT blue ;VARIANT ;ENUM\nSTRUCTURE sdfail DERIVE addr FIELD a n ;STRUCTURE\n;package\n"
      PUT
   s" app.f" S\" require decl.f\n: MAIN ( -- ) 41 . ;\n" PUT
   s" app-hb" BUILD SUCCESS
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   COLD-HASH HASH-APP
   s" family-fresh-hb" SAVE-APP
   s" app-hb" BUILD SUCCESS
   OUT$ S\" files reused 2\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ S\" 41\n" CONTAINS? TTRUE
   WARM-HASH HASH-APP
   s" family-warm-hb" SAVE-APP
   WARM-HASH 32 COLD-HASH 32 T$= ;

\ A decimal count the builder printed.
: COUNT-N ( ptr u8 n -- n )
   TRIM STR>NUMBER? MATCH option
     none OF T-FAIL 0 ENDOF
     some OF ENDOF
   ;MATCH ;


\ The builder's family and variant row counts. The builder runs as its own
\ process because it, not this test engine, loads decl.f: the stores its
\ registry baked are the ones the file outgrows.
: BUILDER-ROWS ( -- n n )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   NATIVE-FILE-APP:BUILDER$ S\" using TFAM TFAM-N@ . SUMV-N@ .\n" CAPTURE SUCCESS
   OUT$ {: out:ptr outu:n :}
   out outu 10 0 SPLIT-NEXT drop {: rest:n :} COUNT-N
   out rest + outu rest - COUNT-N ;


\ The rows that take a store holding n past twice the capacity the builder
\ baked it at: its grow doubles from a capacity of 4 (TFAM TF-GROW, SUMV-GROW),
\ so n rows fill the least 4*2^k that holds them, and 2*cap - n + 1 more rows
\ need more than the 2*cap one grow gives.
: ROWS-PAST-DOUBLE ( n -- n ) {: n:n :}
   4 begin dup n < while 2 * repeat 2 * n - 1 + ;

create ENUM-NAME 16 allot
create ENUM-LINE 48 allot

\ Spell n in base 26 with lowercase letters into ENUM-NAME, as many letters as
\ it takes, no leading `a` but for 0; the letter count.
: ENUM-NAME! ( n -- n ) {: n:n :}
   1 n begin dup 26 >= while 26 / swap 1+ swap repeat drop {: w:n :}
   n w 0 ?do dup 26 mod 97 + ENUM-NAME w 1- i - + c! 26 / loop drop
   w ;

\ decl.f's line i, a one-variant family `ENUM eN xN ;ENUM` with N spelling i
\ in base 26; its length.
: ENUM-LINE! ( n -- n ) ENUM-NAME! {: w:n :}
   s" ENUM e" drop ENUM-LINE 6 BYTE-COPY
   ENUM-NAME ENUM-LINE 6 + w BYTE-COPY
   s"  x" drop ENUM-LINE 6 + w + 2 BYTE-COPY
   ENUM-NAME ENUM-LINE 8 + w + w BYTE-COPY
   S\"  ;ENUM\n" drop ENUM-LINE 8 + w 2 * + 7 BYTE-COPY
   15 w 2 * + ;


\ A reused file whose families take the family and variant stores the builder
\ baked past twice their capacity leaves the cold image. It covers both ways a
\ hit could differ: a cold load writes rows into each baked DATA store before
\ the store moves, while a hit makes room first, so the abandoned DATA must not
\ depend on that order; and a hit needing more than twice the baked capacity
\ must reach the capacity the cold load's doubling reaches, not its need. Each
\ one-variant ENUM adds a row to both stores, so the file declares the rows the
\ store farther from its doubled capacity takes. The first build runs on an
\ empty cache, so it is the fresh-cache build.
: GROW-HIT-CASE ( -- )
   s" a reused file that moves a registry store leaves the cold image" T-LABEL
   BUILDER-ROWS ROWS-PAST-DOUBLE swap ROWS-PAST-DOUBLE max {: rows:n :}
   s" native-file-types-grow" TREE
   s" cache" AT FS:MAKE-DIR
   s" decl.f" S\" package PZ private\n" PUT
   rows 0 ?do
      s" decl.f" AT ENUM-LINE i ENUM-LINE! FS:APPEND-FILE
   loop
   s" decl.f" AT S\" ;package\n" FS:APPEND-FILE
   s" app.f" S\" require decl.f\n: MAIN ( -- ) 41 . ;\n" PUT
   s" app-hb" BUILD SUCCESS
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   COLD-HASH HASH-APP
   s" grow-fresh-hb" SAVE-APP
   s" app-hb" BUILD SUCCESS
   OUT$ S\" files reused 2\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ S\" 41\n" CONTAINS? TTRUE
   WARM-HASH HASH-APP
   s" grow-warm-hb" SAVE-APP
   WARM-HASH 32 COLD-HASH 32 T$= ;

\ A reused file whose cold load spent compile history outside its recorded
\ definitions (a STRUCTURE whose name the package SDFAIL takes, rolled back by
\ catch after its MAKE and UNMAKE compiled) leaves the compiler's
\ identity counters where that cold load left them: the warm build's image
\ holds the bytes of the fresh-cache build. The first build runs on an empty
\ cache, so it is that fresh-cache build.
: ROLL-HIT-CASE ( -- )
   s" a reused file of rolled-back compiles leaves the cold image" T-LABEL
   s" native-file-types-roll" TREE
   s" cache" AT FS:MAKE-DIR
   s" decl.f"
      S\" package SDFAIL public\n: A ( -- n ) 7 ;\n;package\n: ROLLED ( -- n ) [: s\" STRUCTURE sdfail DERIVE addr FIELD a n ;STRUCTURE\" evaluate-closed ;] catch ;\nROLLED constant ROLL-RC\n"
      PUT
   s" app.f" S\" require decl.f\n: MAIN ( -- ) ROLL-RC 0<> if 1 else 0 then . SDFAIL:A . ;\n" PUT
   s" app-hb" BUILD SUCCESS
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ S\" 1\n7\n" CONTAINS? TTRUE
   COLD-HASH HASH-APP
   s" roll-fresh-hb" SAVE-APP
   s" app-hb" BUILD SUCCESS
   OUT$ S\" files reused 2\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ S\" 1\n7\n" CONTAINS? TTRUE
   WARM-HASH HASH-APP
   s" roll-warm-hb" SAVE-APP
   WARM-HASH 32 COLD-HASH 32 T$= ;

create SLICE-KEY 64 allot
DYNAMIC-BUFFER ENTRY u8

\ BUILD with the cache diagnostics on: each file's object, reuse and miss
\ prints its line on stderr.
: DIAG-BUILD ( ptr u8 n -- n )
   {: product:ptr productu:n :}
   NATIVE-FILE-APP:BUILDER$ {: builder:ptr builderu:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_OUT" >LEN s" cache" AT >LEN PROC-ENV+
   s" HABU_NATIVE_CACHE_DIAG" >LEN s" 1" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG s" app.f" AT ARG product productu AT ARG
   builder builderu S\" 1 set-tier\nAPP-BUILD:RUN-SAVED-REPORT\n" CAPTURE ;


\ c.f's object key, from the object line its cold build printed.
: KEEP-KEY ( -- )
   ERR$ s" /c.f key=" FIND-SUB MATCH option
     none OF T-FAIL ENDOF
     some OF IDX>N 9 + {: at:n :}
        ERR$ nip at - 64 >= dup TTRUE
        if ERR$ drop at + SLICE-KEY 64 BYTE-COPY then
     ENDOF
   ;MATCH ;


\ c.f's object path, in PROBE-IN.
: OBJECT-PATH ( -- ptr u8 n )
   0 PROBE-IN-U !
   s" cache" AT NATIVE-FILE-APP:OBJ-DIR$ IN+ s" /" IN+ SLICE-KEY 64 IN+ s" .hfo" IN+
   PROBE-IN PROBE-IN-U @ ;


\ Read c.f's cached object into ENTRY and answer its checker slice 0's unit.
\ The checker section (slot 5) holds the slice count, the reference count, the
\ references, then slice 0's length and its unit (native-unit-object.f
\ IMPORT-RUN).
: OPEN-UNIT ( -- ptr u8 n )
   OBJECT-PATH {: path:ptr pathu:n :}
   path pathu FS:FILE-SIZE {: size:n :}
   size ENTRY-RESERVE
   0 ENTRY BYTE-VIEW {: e:ptr :}
   path pathu e size FS:READ-ALL size <> if E-FS-IO throw then
   e 16 + size 16 - NUNIT-FILE:ARCH-AARCH64 s" FILE" SLICE-KEY 64
      NUNIT-FILE:READ-VIEW
   5 NUNIT-FILE:SECTION$ drop {: sec:ptr :}
   sec 8 + LE:U64@ 3 + 8 * sec + {: unit:ptr :}
   NUNIT-FILE:RESET-KEEP
   unit  unit 8 - LE:U64@ ;


\ Reseal the entry OPEN-UNIT read, so the entry and the object's framing still
\ read, and write it back over c.f's object.
: RESEAL-UNIT ( -- )
   OBJECT-PATH {: path:ptr pathu:n :}
   path pathu FS:FILE-SIZE {: size:n :}
   0 ENTRY BYTE-VIEW {: e:ptr :}
   e 16 + size 16 - NDEF-CACHE:CHECKSUM e 8 + LE:U64!
   path pathu e size FS:WRITE-ALL ;


\ Flip the low bit of the unit magic, the unit's first cell, of c.f's checker
\ slice 0 and reseal the entry; only the checker's unit header read refuses it.
: CORRUPT-SLICE ( -- )
   OPEN-UNIT drop {: magic:ptr :}
   magic c@ 1 xor magic c!
   RESEAL-UNIT ;


\ Empty the last link row of c.f's checker slice 0 and reseal the entry: its
\ effect becomes tag 0 and reference 0 beside a grid already tag 0. The link
\ rows close the unit, ten cells each: the record reference, the tag and
\ reference of its effect, control and grid, the deferred flag, the fixed kind
\ and value (checker.f UNIT-EXPORT-LINK). They follow ordinal order, so the
\ last row links a record c.f defines.
: EMPTY-LINK ( -- )
   OPEN-UNIT {: unit:ptr len:n :}
   unit len + 80 - {: row:ptr :}
   row 8 + LE:U64@ 0 <> TTRUE
   row 40 + LE:U64@ 0= TTRUE
   0 row 8 + LE:U64!
   0 row 16 + LE:U64!
   RESEAL-UNIT ;


\ The reason the diagnostics name for c.f's miss; empty when none is named.
: MISS-REASON$ ( -- ptr u8 n )
   0 PROBE-IN-U !
   s" native-file: miss path=" IN+ s" c.f" AT IN+ s"  reason=" IN+
   ERR$ PROBE-IN PROBE-IN-U @ FIND-SUB MATCH option
     none OF s" " ENDOF
     some OF IDX>N PROBE-IN-U @ + {: at:n :}
        ERR$ drop at +  ERR$ nip at -  {: a:ptr u:n :}
        a u 10 INDEX-OF MATCH option
          none OF a u ENDOF
          some OF IDX>N a swap ENDOF
        ;MATCH
     ENDOF
   ;MATCH ;

\ app.f loads c.f, so a warm build would replay its object around c.f's. A
\ case whose app.f must compile from source against what c.f left edits a
\ comment into it first: the new bytes give app.f a key no object holds.
: EDIT-APP ( -- )
   s" app.f" AT S\" \\ edited\n" FS:APPEND-FILE ;

\ c.f declares private products, whose MAKE and UNMAKE are words of package
\ FCTOR. app.f, compiled from source in every build (each
\ warm build edits it first), reopens FCTOR and gives the
\ generic box the two-cell pair. Only the typed construct step admits that
\ call (TFAM-CTOR-STEP?: a direct layout argument), and the step finds the
\ variant through the constructor word's symbol. A reused c.f must leave each
\ variant naming its live constructor word as compiling c.f does; one naming
\ none refuses SOME2 at BOX-MAKE (expected: a actual: pair).
: CTOR-CASE ( -- )
   s" a reused product keeps its constructor for a later file's typed call" T-LABEL
   s" native-file-types-ctor" TREE
   s" cache" AT FS:MAKE-DIR
   s" c.f"
      S\" package FCTOR\nPRODUCT pair FIELD x n FIELD y n ;PRODUCT\nPRODUCT box<a> FIELD v a ;PRODUCT\npublic\n: CODE ( -- n ) 5 ;\n;package\n"
      PUT
   s" app.f"
      S\" require c.f\npackage FCTOR\n: SOME2 ( n n -- box<pair> ) PAIR-MAKE BOX-MAKE ;\n: GET ( box<pair> -- n ) BOX-UNMAKE PAIR-UNMAKE + ;\npublic\n: RUN ( -- n ) 3 4 SOME2 GET ;\n;package\n: MAIN ( -- ) FCTOR:RUN . FCTOR:CODE . ;\n"
      PUT
   \ The diagnostics name c.f's object key for LINK-CASE and SLICE-CASE.
   s" app-hb" DIAG-BUILD SUCCESS
   KEEP-KEY
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ S\" 7\n5\n" CONTAINS? TTRUE
   EDIT-APP
   s" app-hb" BUILD SUCCESS
   OUT$ S\" files reused 1\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ S\" 7\n5\n" CONTAINS? TTRUE ;


\ a.f interns FCTOR's private PAIR-MAKE and undefines it; the symbol stays, so
\ c.f's PRODUCT pair takes that earlier symbol for its constructor word. c.f's
\ family rows then name a constructor the file does not own, which the capture
\ cannot carry: the checker export refuses c.f in both builds. a.f's object is
\ stored, but its undefine leaves it unsupported for reuse, so the warm build
\ misses it too; with app.f edited first it reuses no file, loads app.f, a.f
\ and c.f from source, and the app
\ runs as the cold build's does. A capture refusal that throws instead fails
\ the cold build (rc 76).
: CTOR-PREFIX-CASE ( -- )
   s" a constructor word interned before its file refuses that file's capture" T-LABEL
   s" native-file-types-ctor-prefix" TREE
   s" cache" AT FS:MAKE-DIR
   s" a.f" S\" package FCTOR\n: PAIR-MAKE ( -- ) ;\nundefine PAIR-MAKE\n;package\n" PUT
   s" c.f"
      S\" package FCTOR\nPRODUCT pair FIELD x n FIELD y n ;PRODUCT\npublic\n: RUN ( -- n ) 3 4 PAIR-MAKE PAIR-UNMAKE + ;\n;package\n"
      PUT
   s" app.f" S\" require a.f\nrequire c.f\n: MAIN ( -- ) FCTOR:RUN . ;\n" PUT
   true NATIVE-FILE-APP:DIAG? !
   s" app-hb" BUILD SUCCESS
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ S\" 7\n" CONTAINS? TTRUE
   EDIT-APP
   s" app-hb" BUILD SUCCESS
   false NATIVE-FILE-APP:DIAG? !
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   0 EXPECT-U !
   s" native-file: refused path=" EXPECT 512 EXPECT-U BUF-APPEND
   s" c.f" AT EXPECT 512 EXPECT-U BUF-APPEND
   S\"  reason=checker-export\n" EXPECT 512 EXPECT-U BUF-APPEND
   ERR$ EXPECT EXPECT-U @ CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ S\" 7\n" CONTAINS? TTRUE ;

\ A cache hit never throws. LINK-CASE's miss rewrote c.f's object from source;
\ with its checker slice corrupted where only the checker's unit read refuses it,
\ the object is a miss decided before any install side effect, the miss names
\ its reason, c.f compiles from source and the app prints what the first
\ build's app printed. No entry refusal: the reseal holds.
: SLICE-CASE ( -- )
   s" a cached checker slice the engine refuses is a named miss" T-LABEL
   CORRUPT-SLICE
   EDIT-APP
   s" app-hb" DIAG-BUILD SUCCESS
   ERR$ s" native-cache: entry refused" CONTAINS? TFALSE
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   MISS-REASON$ nip 0 > TTRUE
   MISS-REASON$ s" checker-prepare" T$=
   RUN-APP SUCCESS OUT$ S\" 7\n5\n" CONTAINS? TTRUE ;


\ A cache hit never throws. CTOR-CASE's warm build reused c.f's object; a link
\ row with neither effect nor grid, which the writer never exports, would
\ leave the installed record unlinked. Preparation refuses the row, so the
\ object is a named miss decided before any install side effect, c.f compiles
\ from source and the app prints what the first build's app printed.
: LINK-CASE ( -- )
   s" a cached link with neither effect nor grid is a named miss" T-LABEL
   EMPTY-LINK
   EDIT-APP
   s" app-hb" DIAG-BUILD SUCCESS
   ERR$ s" native-cache: entry refused" CONTAINS? TFALSE
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   MISS-REASON$ s" checker-prepare" T$=
   RUN-APP SUCCESS OUT$ S\" 7\n5\n" CONTAINS? TTRUE ;

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
   FAMILY-HIT-CASE
   GROW-HIT-CASE
   ROLL-HIT-CASE
   CTOR-CASE
   LINK-CASE
   SLICE-CASE
   CTOR-PREFIX-CASE
   T-REPORT
   s" native file unit artifacts: " type ROOT$ type cr ;
;package
;using

NATIVE-FILE-TYPES-TEST:MAIN
