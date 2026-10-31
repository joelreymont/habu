\ Compiled words can load multiple source files during an ordinary APP-BUILD.

require src/lib/test.f
require src/lib/string.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/native-file-e2e.f
require test/native-file-app-lib.f

using NATIVE-FILE-E2E
package NATIVE-FILE-DYNAMIC-LOAD-TEST

: RUN-BUILD ( ptr u8 n -- n )
   {: builder:ptr builderu:n :}
   s" HABU_OUT" >LEN s" cache" AT >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG s" app.f" AT ARG s" app-hb" AT ARG
   builder builderu S\" 1 set-tier\nAPP-BUILD:RUN-SAVED-REPORT\n" CAPTURE ;

: BUILD ( -- n )
   NATIVE-FILE-APP:BUILDER$ PROC-ARGV-ENV-RESET RUN-BUILD ;

: ENV-BUILD ( ptr u8 n -- n )
   {: root:ptr rootu:n :}
   NATIVE-FILE-APP:BUILDER$
   PROC-ARGV-ENV-RESET
   s" SHARED_ROOT" >LEN root rootu >LEN PROC-ENV+
   RUN-BUILD ;

: EMPTY-BUILD ( bool -- n )
   {: present:bool :}
   NATIVE-FILE-APP:BUILDER$
   PROC-ARGV-ENV-RESET
   present if
      s" HABU_FILE_EMPTY_INPUT" >LEN s" " >LEN PROC-ENV+
   then
   RUN-BUILD ;

64 constant OUT-CAP
create COLD-OUT OUT-CAP allot  variable COLD-OUT-U

\ The line of ERR$ from tag to its newline.
: LINE$ ( ptr u8 n -- ptr u8 n )
   {: tag:ptr tagu:n :}
   ERR$ tag tagu FIND-SUB MATCH option
     none OF T-FAIL s" " ENDOF
     some OF IDX>N {: at:n :}
        ERR$ drop at +  ERR$ nip at -  {: a:ptr u:n :}
        a u 10 INDEX-OF MATCH option
          none OF a u ENDOF
          some OF IDX>N a swap ENDOF
        ;MATCH
     ENDOF
   ;MATCH ;

: DIAG-BUILD ( -- n )
   NATIVE-FILE-APP:BUILDER$
   PROC-ARGV-ENV-RESET
   s" HABU_NATIVE_CACHE_DIAG" >LEN s" 1" >LEN PROC-ENV+
   RUN-BUILD ;

DYNAMIC-BUFFER BYTES u8
DYNAMIC-BUFFER IMAGE u8  variable IMAGE-U
create EXPECT 512 allot  variable EXPECT-U

\ The app image's bytes in BYTES; their count.
: LOAD-IMAGE ( -- n )
   s" app-hb" AT {: path:ptr pathu:n :}
   path pathu FS:FILE-SIZE {: size:n :}
   size BYTES-RESERVE
   path pathu 0 BYTES size FS:READ-ALL size T=
   size ;

: KEEP-IMAGE ( -- )
   LOAD-IMAGE {: size:n :}
   size IMAGE-RESERVE
   0 BYTES 0 IMAGE size BYTE-COPY size IMAGE-U ! ;

: SAME-IMAGE? ( -- bool )
   LOAD-IMAGE {: size:n :}
   0 BYTES size 0 IMAGE IMAGE-U @ STR= ;

\ The diagnostic head tag followed by file name's path.
: FILE-LINE$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: tag:ptr tagu:n name:ptr nameu:n :}
   0 EXPECT-U !
   tag tagu EXPECT 512 EXPECT-U BUF-APPEND
   name nameu AT EXPECT 512 EXPECT-U BUF-APPEND
   EXPECT EXPECT-U @ ;

\ store.f's top level maps memory into SP, its own PERSISTED-PTR-VARIABLE,
\ fills it through fill.f's FILL and reads it into SEVEN. The mapping is the
\ file's own storage, so store.f stores an object; its top level runs on every
\ load, so an edit to FILL's body reaches SEVEN through the reused object.
: MAPPED-OWN ( -- )
   s" a file's top level reads a mapping held by its own record and is reused" T-LABEL
   s" native-file-mapped-own" TREE
   s" cache" AT FS:MAKE-DIR
   s" fill.f" S\" package FILL public : FILL ( ptr u8 -- ) 7 swap CELL-VIEW ! ; ;package\n" PUT
   s" store.f" S\" require src/lib/memory.f\nrequire fill.f\npackage STORE\nPERSISTED-PTR-VARIABLE SP\n: BASE ( -- ptr u8 ) SP @ ;\n: SP! ( -- ) MEM-ALLOC-64K {: p:ptr bytes:n :} p SP ! bytes drop ;\nSP!\nBASE FILL:FILL\n: PEEK ( -- n ) BASE CELL-VIEW @ ;\npublic PEEK constant SEVEN\nNULL-PTR SP !\n;package\n" PUT
   s" app.f" S\" require fill.f\nrequire store.f\n: MAIN ( -- ) STORE:SEVEN . ;\n" PUT
   DIAG-BUILD SUCCESS
   RUN-APP SUCCESS OUT$ s" 7" CONTAINS? TTRUE
   KEEP-IMAGE
   DIAG-BUILD SUCCESS
   ERR$ s" native-file: reused path=" s" store.f" FILE-LINE$ CONTAINS? TTRUE
   ERR$ s" native-file: refused path=" s" store.f" FILE-LINE$ CONTAINS? TFALSE
   OUT$ S\" files reused 3\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 7" CONTAINS? TTRUE
   SAME-IMAGE? TTRUE
   s" fill.f" S\" package FILL public : FILL ( ptr u8 -- ) 8 swap CELL-VIEW ! ; ;package\n" PUT
   DIAG-BUILD SUCCESS
   ERR$ s" native-file: reused path=" s" store.f" FILE-LINE$ CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 8" CONTAINS? TTRUE ;

\ mapped.f maps memory at top level after the builder's first map read, which
\ early.f's immediate EARLY-MARK makes by reading a variable at compile time,
\ and app.f's SPOT-USE reads SPOT, which holds that memory's address, at
\ compile time. The read is a refusal, so app.f's frame records no DATA row
\ for the address. RUN-BUILD's fresh builder process is the boundary under
\ test: its first map read precedes the mapping.
: MAPPED-LATE ( -- )
   s" a compile-time read of memory mapped after the first map read is refused" T-LABEL
   s" native-file-mapped-late" TREE
   s" cache" AT FS:MAKE-DIR
   s" early.f" S\" variable EARLY\n5 EARLY !\n: EARLY-MARK ( -- ) EARLY @ drop ; immediate\ns\" EARLY-MARK\" 0 parse-imm\n: EARLY-USE ( -- n ) EARLY-MARK 1 ;\n" PUT
   s" mapped.f" S\" require src/lib/memory.f\nvariable SPOT\n: SPOT! ( -- ) MEM-ALLOC-64K {: p:ptr bytes:n :} p BYTE-VIEW NULL-PTR BYTE-VIEW - SPOT ! bytes drop ;\nSPOT!\n: SPOT-MARK ( -- ) SPOT @ drop ; immediate\ns\" SPOT-MARK\" 0 parse-imm\n" PUT
   s" app.f" S\" require early.f\nrequire mapped.f\n: SPOT-USE ( -- n ) SPOT-MARK 2 ;\n: MAIN ( -- ) SPOT-USE . EARLY-USE . ;\n" PUT
   DIAG-BUILD SUCCESS
   s" reason=mapped-compile-time-read holder=SPOT via=SPOT-MARK run=SPOT-MARK path=" LINE$
   s" /app.f" ENDS-WITH? TTRUE
   s" /app.f rows=" LINE$ {: frame:ptr frameu:n :}
   frame frameu s"  ordered=1 " CONTAINS? TTRUE
   frame frameu s"  data=0 reason=mapped-compile-time-read word=SPOT" ENDS-WITH? TTRUE
   RUN-APP SUCCESS
   OUT$ nip OUT-CAP > if T-FAIL exit then
   OUT$ {: out:ptr outu:n :} out COLD-OUT outu BYTE-COPY outu COLD-OUT-U !
   DIAG-BUILD SUCCESS
   RUN-APP SUCCESS OUT$ COLD-OUT COLD-OUT-U @ T$= ;

: REUSED ( -- )
   OUT$ s" files reused " CONTAINS? TTRUE
   OUT$ S\" files reused 0\n" CONTAINS? TFALSE ;

: ENVIRONMENT ( -- )
   s" cached definitions preserve environment-selected loads" T-LABEL
   s" native-file-env-load" TREE
   s" cache" AT FS:MAKE-DIR s" a" AT FS:MAKE-DIR s" b" AT FS:MAKE-DIR
   s" a/type.f" S\" package SHARED public NEWTYPE rownum\n: ANSWER ( -- n ) 42 ; ;package\n" PUT
   s" b/type.f" S\" package SHARED public NEWTYPE rownum\n: ANSWER ( -- n ) 99 ; ;package\n" PUT
   s" loader.f" S\" package LOADER\n: LOAD ( -- ) s\" type.f\" required ;\n: RUN ( -- ) s\" SHARED_ROOT\" GETENV [: LOAD ;] SOURCE-ROOT:WITH ;\n' RUN ;package execute\n" PUT
   s" app.f" S\" require loader.f\n: MAIN ( -- ) SHARED:ANSWER . ;\n" PUT
   s" a" AT ENV-BUILD SUCCESS
   RUN-APP SUCCESS OUT$ S\" 42\n" T$=
   \ a/type.f and b/type.f are one instance, type.f under the root loader.f
   \ loads it from, so they share one object key. Under b, that object's input
   \ rows, read against root b, find other bytes: b/type.f compiles from
   \ source and its object replaces a's. app.f's object holds, since MAIN
   \ depends on the declaration of SHARED:ANSWER, which b's ANSWER shares, and
   \ calls the live word. loader.f's top level reads SHARED_ROOT, an input
   \ row of its object, so this build reuses app.f alone. loader.f's load
   \ runs from a word its top level runs and is its object's LOAD row, so the
   \ next build reuses all three files.
   s" b" AT ENV-BUILD SUCCESS
   OUT$ S\" files reused 1\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ S\" 99\n" T$=
   s" b" AT ENV-BUILD SUCCESS
   OUT$ S\" files reused 3\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ S\" 99\n" T$=
   s" cached source distinguishes unset and empty environment values" T-LABEL
   s" app.f" S\" : EMPTY-QUERY ( -- ) s\" \" GETENV 2drop ;\nEMPTY-QUERY\n: PRESENT? ( -- bool ) s\" HABU_FILE_EMPTY_INPUT\" GETENV drop NULL-PTR <> ;\nPRESENT? constant PRESENT\n: MAIN ( -- ) PRESENT . ;\n" PUT
   false EMPTY-BUILD SUCCESS
   RUN-APP SUCCESS OUT$ S\" 0\n" T$=
   true EMPTY-BUILD SUCCESS
   RUN-APP SUCCESS OUT$ S\" -1\n" T$= ;

\ parent.f's word loads child.f only when app.f has not loaded it first, so
\ parent.f's object holds a LOAD row in one order and none in the other, and
\ N and TOTAL, made after the load, stand past child.f's records in one order
\ only. Each build reads the object the other order stored, which misses
\ from its first unmatched load row; the product reads N through TOTAL.
: ORDER ( -- )
   s" an object built in one load order misses in the other" T-LABEL
   s" native-file-load-order" TREE
   s" cache" AT FS:MAKE-DIR
   s" child.f" S\" package CHILD public : VALUE ( -- n ) 5 ; ;package\n" PUT
   s" parent.f" S\" : LOAD-CHILD ( -- ) s\" child.f\" required ;\nLOAD-CHILD\nvariable N\n: BUMP ( -- ) CHILD:VALUE N +! ;\nBUMP BUMP\n: TOTAL ( -- n ) N @ ;\n" PUT
   s" app.f" S\" require child.f\nrequire parent.f\n: MAIN ( -- ) TOTAL . ;\n" PUT
   BUILD SUCCESS
   RUN-APP SUCCESS OUT$ S\" 10\n" T$=
   s" app.f" S\" require parent.f\n: MAIN ( -- ) TOTAL . ;\n" PUT
   BUILD SUCCESS
   RUN-APP SUCCESS OUT$ S\" 10\n" T$=
   s" app.f" S\" require child.f\nrequire parent.f\n: MAIN ( -- ) TOTAL . ;\n" PUT
   BUILD SUCCESS
   RUN-APP SUCCESS OUT$ S\" 10\n" T$= ;

\ ORDER's files and build orders, except that parent.f makes TOTAL by
\ evaluating its text past the load. The last build makes no load and reads
\ the object the middle one stored, whose TOTAL row lies in the segment after
\ its LOAD row: the run misses at that head instead of installing TOTAL
\ against a segment it never mapped, which throws.
: EVAL-ORDER ( -- )
   s" an evaluated head past a load the run skipped misses" T-LABEL
   s" native-file-eval-order" TREE
   s" cache" AT FS:MAKE-DIR
   s" child.f" S\" package CHILD public : VALUE ( -- n ) 5 ; ;package\n" PUT
   s" parent.f" S\" : LOAD-CHILD ( -- ) s\" child.f\" required ;\nLOAD-CHILD\nvariable N\n: BUMP ( -- ) CHILD:VALUE N +! ;\nBUMP BUMP\ns\" : TOTAL ( -- n ) N @ ;\" evaluate\n" PUT
   s" app.f" S\" require child.f\nrequire parent.f\n: MAIN ( -- ) TOTAL . ;\n" PUT
   BUILD SUCCESS
   RUN-APP SUCCESS OUT$ S\" 10\n" T$=
   s" app.f" S\" require parent.f\n: MAIN ( -- ) TOTAL . ;\n" PUT
   BUILD SUCCESS
   RUN-APP SUCCESS OUT$ S\" 10\n" T$=
   s" app.f" S\" require child.f\nrequire parent.f\n: MAIN ( -- ) TOTAL . ;\n" PUT
   BUILD SUCCESS
   RUN-APP SUCCESS OUT$ S\" 10\n" T$= ;

\ parent.f carries a value across its include of child.f, so the load does
\ not end balanced and parent.f's object holds no LOAD row for it. The warm
\ build stops installing parent.f at that load: G and F compile live, and F
\ calls this build's G.
: UNBALANCED ( -- )
   s" a load that ends unbalanced stops its file's installs" T-LABEL
   s" native-file-unbalanced-load" TREE
   s" cache" AT FS:MAKE-DIR
   s" child.f" S\" : KID ( -- n ) 2 ;\n" PUT
   s" parent.f" S\" 1 include child.f drop\n: G ( -- n ) 3 ;\n: F ( -- n ) G 1+ ;\n" PUT
   s" app.f" S\" require parent.f\n: MAIN ( -- ) F . ;\n" PUT
   BUILD SUCCESS
   RUN-APP SUCCESS OUT$ S\" 4\n" T$=
   BUILD SUCCESS
   RUN-APP SUCCESS OUT$ S\" 4\n" T$= ;

\ A build with the cache diagnostics on when diag, and HABU_TEST_SINK set
\ when switch.
: SINK-BUILD ( bool bool -- n )
   {: diag:bool switch:bool :}
   NATIVE-FILE-APP:BUILDER$
   PROC-ARGV-ENV-RESET
   diag if s" HABU_NATIVE_CACHE_DIAG" >LEN s" 1" >LEN PROC-ENV+ then
   switch if s" HABU_TEST_SINK" >LEN s" 1" >LEN PROC-ENV+ then
   RUN-BUILD ;

\ The full origin journal's rows a build's p.f load recorded, printed by
\ app.f as oj=, then each row's name and name token span: OJ-SHOW prints the
\ rows past the serial OJ-SER took (test/native-file-frame-e2e.f FRAME-OJ-PROBE).
: PROBE ( -- )
   s" probe.f" S\" package OJ-PROBE\nCAST: OJ-MARK ( n -- [ -- n n bool ] )\nCAST: OJ-ROWS ( n -- [ n [ n n n n ptr u8 n ptr u8 n n n -- ] -- ] )\n: OJ-ROW ( n n n n ptr u8 n ptr u8 n n n -- )\n   {: serial:n phase:n xt:n sym:n path:ptr pathu:n name:ptr nameu:n at:n end:n :}\n   space name nameu type s\" :\" type at . end . ;\npublic\n: OJ-SER ( -- n ) s\" ENGINE-ORIGIN-EARLY:FILE-MARK\" NUNIT-ORIGIN:JOURNAL-XT OJ-MARK execute drop drop ;\n: OJ-SHOW ( n -- ) s\" oj=\" type [: OJ-ROW ;] s\" ENGINE-ORIGIN:FILE-ROWS\" NUNIT-ORIGIN:JOURNAL-XT OJ-ROWS execute s\" ;oj\" type cr ;\n;package\n" PUT ;

\ app.f around p.f: armed when armed, so p.f opens under the full journal.
: SINK-APP ( bool -- )
   {: armed:bool :}
   s" app.f" armed if
      S\" require probe.f\nrequire x.f\nvariable OJ-AT\nENGINE-ORIGIN:ARM\nOJ-PROBE:OJ-SER OJ-AT !\nrequire p.f\nOJ-AT @ OJ-PROBE:OJ-SHOW\n: MAIN ( -- ) P:W . ;\n"
   else
      S\" require probe.f\nvariable OJ-AT\nOJ-PROBE:OJ-SER OJ-AT !\nrequire p.f\nOJ-AT @ OJ-PROBE:OJ-SHOW\n: MAIN ( -- ) P:W . ;\n"
   then PUT ;

\ Each file segment records the origin sink it opened under, and a warm
\ build installs a definition only under the sink its segment recorded. The
\ app build loads with the full journal disarmed (no sink).
: ORIGIN-SINKS ( -- )
   s" native-file-origin-sink" TREE
   s" cache" AT FS:MAKE-DIR
   PROBE
   s" a sink switch between two loads leaves the next segment's rows whole" T-LABEL
   s" a.f" S\" package A public : VALUE ( -- n ) 1 ; ;package\n" PUT
   s" b.f" S\" package B public : VALUE ( -- n ) 2 ; ;package\n" PUT
   s" p.f" S\" require a.f\nENGINE-ORIGIN:ARM\nrequire b.f\npackage P public : W ( -- n ) 7 ; ;package\n" PUT
   false SINK-APP
   false false SINK-BUILD SUCCESS
   OUT$ s"  W:" CONTAINS? TTRUE
   true false SINK-BUILD SUCCESS
   ERR$ s" /p.f reason=" CONTAINS? TFALSE
   ERR$ s" native-file: origin-sink" CONTAINS? TFALSE
   OUT$ s"  W:" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 7" CONTAINS? TTRUE
   s" a definition whose segment opened under another sink compiles live" T-LABEL
   s" a.f" S\" package A public : SWITCH ( -- ) s\" HABU_TEST_SINK\" GETENV drop NULL-PTR <> if ENGINE-ORIGIN:ARM then ; ;package\nA:SWITCH\n" PUT
   s" p.f" S\" require a.f\npackage P public : W ( -- n ) 8 ; ;package\n" PUT
   false false SINK-BUILD SUCCESS
   OUT$ s"  W:" CONTAINS? TFALSE
   true true SINK-BUILD SUCCESS
   ERR$ s" native-file: origin-sink name=W" CONTAINS? TTRUE
   OUT$ s"  W:" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 8" CONTAINS? TTRUE
   true true SINK-BUILD SUCCESS
   ERR$ s" native-file: origin-sink" CONTAINS? TFALSE
   ERR$ s" /p.f reason=" CONTAINS? TFALSE
   OUT$ s"  W:" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 8" CONTAINS? TTRUE
   s" a load that brings in no child closes the harvest segment" T-LABEL
   s" x.f" S\" package X public : VALUE ( -- n ) 3 ; ;package\n" PUT
   s" p.f" S\" ENGINE-ORIGIN:DISARM\ns\" x.f\" required\nENGINE-ORIGIN:ARM\npackage P public : W ( -- n ) 9 ; ;package\n" PUT
   true SINK-APP
   \ W's segment opened with no sink and recorded W's row under the full
   \ journal, so the harvest cannot attribute it.
   true false SINK-BUILD SUCCESS
   ERR$ s" /p.f reason=origin-sink changed" CONTAINS? TTRUE
   OUT$ s"  W:" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 9" CONTAINS? TTRUE
   true false SINK-BUILD SUCCESS
   OUT$ s"  W:" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 9" CONTAINS? TTRUE ;

public
: MAIN ( -- )
   T-RESET
   s" compiled required actions run with cached definitions" T-LABEL
   s" native-file-dynamic-load" TREE
   s" cache" AT FS:MAKE-DIR
   s" a.f" S\" package A public : VALUE ( -- n ) 41 ; ;package\n" PUT
   s" b.f" S\" package B public : VALUE ( -- n ) 1 ; ;package\n" PUT
   s" app.f" S\" : LOAD-BOTH ( -- ) s\" a.f\" required s\" b.f\" required ;\nLOAD-BOTH\n: MAIN ( -- ) A:VALUE B:VALUE + . ;\n" PUT
   BUILD SUCCESS
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 42" CONTAINS? TTRUE
   BUILD SUCCESS
   REUSED
   RUN-APP SUCCESS OUT$ s" 42" CONTAINS? TTRUE
   s" b.f" S\" package B public : VALUE ( -- n ) 3 ; ;package\n" PUT
   BUILD SUCCESS
   REUSED
   RUN-APP SUCCESS OUT$ s" 44" CONTAINS? TTRUE
   s" bump.f" S\" 1 N +!\n" PUT
   s" app.f" S\" variable N\ninclude bump.f\ninclude bump.f\n: MAIN ( -- ) N @ . ;\n" PUT
   BUILD SUCCESS
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 2" CONTAINS? TTRUE
   BUILD SUCCESS
   REUSED
   RUN-APP SUCCESS OUT$ s" 2" CONTAINS? TTRUE
   ENVIRONMENT
   ORDER
   EVAL-ORDER
   UNBALANCED
   MAPPED-OWN
   MAPPED-LATE
   ORIGIN-SINKS
   T-REPORT ;
;package
;using

NATIVE-FILE-DYNAMIC-LOAD-TEST:MAIN
