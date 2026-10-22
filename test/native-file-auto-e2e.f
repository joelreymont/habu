\ Automatic definition reuse through the ordinary hb-build application path.
\ bin/hb --load test/native-file-auto-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/whitebox-child.f
require test/native-file-e2e.f
require test/native-file-app-lib.f

using NATIVE-FILE-E2E
package NATIVE-FILE-AUTO-TEST

$8000 constant CAP
create INPUT CAP allot  variable INPUT-U
create TRACE CAP allot
variable HISTORY-N
DYNAMIC-BUFFER LITERAL-IMAGE u8
DYNAMIC-BUFFER LITERAL-COLD u8
variable LITERAL-IMAGE-U
variable DIAG?
variable DIAG-LINE-N
variable DIAG-START

: STAGE-BUILD ( ptr u8 n ptr u8 n -- )
   {: cache:ptr cacheu:n engine:ptr engineu:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN cache cacheu >LEN PROC-ENV+
   s" HABU_UNDER_TEST" >LEN engine engineu >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN engine engineu >LEN PROC-ENV+
   s" HABU_NATIVE_CACHE_DIAG" >LEN
      DIAG? @ if s" 1" else s" 0" then >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG s" app.f" AT ARG s" app-hb" AT ARG ;

: RUN-DIRECT-SOURCE ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: cache:ptr cacheu:n engine:ptr engineu:n source:ptr sourceu:n :}
   cache cacheu engine engineu STAGE-BUILD
   engine engineu source sourceu CAPTURE ;

: RUN-DIRECT-IN ( ptr u8 n ptr u8 n -- n )
   S\" require tools/app-build.f\nAPP-BUILD:RUN\n" RUN-DIRECT-SOURCE ;

: RUN-SAVED-SOURCE ( ptr u8 n ptr u8 n -- n )
   {: cache:ptr cacheu:n source:ptr sourceu:n :}
   NATIVE-FILE-APP:BUILDER$ {: builder:ptr builderu:n :}
   cache cacheu ENGINE-CANDIDATE:PATH$ STAGE-BUILD
   builder builderu source sourceu CAPTURE ;

: RUN-SAVED-IN ( ptr u8 n -- n )
   S\" 1 set-tier\nAPP-BUILD:RUN-SAVED-REPORT\n" RUN-SAVED-SOURCE ;

: RUN-DIRECT ( -- n ) s" direct-cache" AT RUN-SAVED-IN ;
: RUN-EXECUTE ( -- n ) s" execute-cache" AT RUN-SAVED-IN ;
: RUN-DEFINITION ( -- n ) s" definition-cache" AT RUN-SAVED-IN ;
: RUN-LITERAL-FRESH ( -- n ) s" literal-fresh-cache" AT RUN-SAVED-IN ;
: RUN-LITERAL-LOOP ( -- n ) s" literal-loop-cache" AT RUN-SAVED-IN ;
: RUN-LITERAL-LOOP-FRESH ( -- n ) s" literal-loop-fresh-cache" AT RUN-SAVED-IN ;
: RUN-SEGMENT-SPLIT-FRESH ( -- n ) s" segment-split-fresh-cache" AT RUN-SAVED-IN ;
: RUN-SEGMENT-MERGE-FRESH ( -- n ) s" segment-merge-fresh-cache" AT RUN-SAVED-IN ;
: RUN-PRIVATE ( -- n ) s" private-cache" AT WHITEBOX-CHILD:ENGINE$ RUN-DIRECT-IN ;

: DIAG-COMPILED ( -- n )
   OUT$ {: out:ptr outu:n :}
   out outu s" compiled " FIND-SUB MATCH option
      none OF -1 ENDOF
      some OF
         IDX>N s" compiled " nip + {: start:n :}
         out start + outu start - 10 0 SPLIT-NEXT
         2drop {: digits:ptr digitsu:n :}
         digits digitsu TRIM STR>NUMBER? MATCH option
            none OF -1 ENDOF
            some OF ENDOF
         ;MATCH
      ENDOF
   ;MATCH ;

: DIAG-LINES ( -- n )
   ERR$ {: err:ptr erru:n :}
   0 DIAG-LINE-N ! 0 DIAG-START !
   begin
      err DIAG-START @ + erru DIAG-START @ -
         s" native-cache: compile-attempt "
         FIND-SUB MATCH option
            none OF -1 ENDOF
            some OF IDX>N DIAG-START @ + ENDOF
         ;MATCH {: at:n :}
      at 0 >= while
         1 DIAG-LINE-N +!
         at s" native-cache: compile-attempt " nip + DIAG-START !
   repeat DIAG-LINE-N @ ;

: RUN-CUSTOM-CHECK ( ptr u8 n -- n )
   {: source:ptr sourceu:n :}
   s" hook-cache" AT source sourceu RUN-SAVED-SOURCE ;

: REUSED ( -- )
   OUT$ s" definition hits " CONTAINS? TTRUE
   OUT$ S\" definition hits 0\n" CONTAINS? TFALSE ;

: HOOK-CASE ( -- )
   s" a preinstalled checker retains authority during app load" T-LABEL
   s" hook-cache" AT MAKE-DIR
   s" app.f" S\" : MAIN ( -- ) 42 . ;\n" PUT
   s" hook-cache" AT RUN-SAVED-IN SUCCESS
   s" hook-cache" AT RUN-SAVED-IN SUCCESS REUSED
   s" a warm definition still runs the live checker" T-LABEL
   S\" 1 set-tier\nTRUSTED: REJECT-MAIN ( ptr u8 n -- n ) 2dup s\" MAIN \" STARTS-WITH? if 2drop 77 throw then CHECK! ;\n' REJECT-MAIN set-check\nAPP-BUILD:RUN-SAVED-REPORT\n"
      RUN-CUSTOM-CHECK 77 T=
   S\" 1 set-tier\nTRUSTED: REJECT-CHECK ( ptr u8 n -- n ) 2drop 77 throw ;\n' REJECT-CHECK set-check\nAPP-BUILD:RUN-SAVED-REPORT\n"
      RUN-CUSTOM-CHECK 77 T=
   s" a preinstalled checker observes a successful app load" T-LABEL
   S\" 1 set-tier\nTRUSTED: OBSERVE-CHECK ( ptr u8 n -- n ) 2dup s\" MAIN \" STARTS-WITH? if s\" main-check\" type cr then CHECK! ;\n' OBSERVE-CHECK set-check\nAPP-BUILD:RUN-SAVED-REPORT\n"
      RUN-CUSTOM-CHECK SUCCESS
   OUT$ s" main-check" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 42" CONTAINS? TTRUE ;

: HISTORY-FILE ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u s" .hbd" ENDS-WITH? a u s" /hb-def-" CONTAINS? and if
      1 HISTORY-N +!
   then ;

\ The definition entries in the history cache: one hb-def-<key>.hbd each.
: HISTORY-ENTRIES ( -- n )
   0 HISTORY-N !
   s" history-cache" AT ['] HISTORY-FILE WALK-FILES
   HISTORY-N @ ;

: HISTORY-SOURCE ( n -- )
   {: value:n :}
   0 INPUT-U !
   S\" : HIST-KEEP ( -- n ) 40 ;\n: MAIN ( -- ) HIST-KEEP "
      INPUT CAP INPUT-U BUF-APPEND
   value 48 + INPUT CAP INPUT-U BUF-APPEND-C
   32 INPUT CAP INPUT-U BUF-APPEND-C
   S\" + . ;\n" INPUT CAP INPUT-U BUF-APPEND
   s" app.f" INPUT INPUT-U @ PUT ;

: HISTORY-CASE ( -- )
   s" an edit adds entries only for what it compiled and failure preserves them" T-LABEL
   1 HISTORY-SOURCE
   s" history-cache" AT RUN-SAVED-IN SUCCESS
   RUN-APP SUCCESS OUT$ s" 41" CONTAINS? TTRUE
   HISTORY-ENTRIES {: initial:n :}
   initial 0 > TTRUE
   2 HISTORY-SOURCE
   s" history-cache" AT RUN-SAVED-IN SUCCESS REUSED
   DIAG-COMPILED {: compiled:n :}
   RUN-APP SUCCESS OUT$ s" 42" CONTAINS? TTRUE
   HISTORY-ENTRIES {: stable:n :}
   stable initial - {: added:n :}
   added 0 > TTRUE
   added compiled <= TTRUE
   s" app.f" S\" : HIST-KEEP ( -- n ) 40 ;\n: MAIN ( -- ) HIST-KEEP MISSING . ;\n" PUT
   s" history-cache" AT RUN-SAVED-IN 70 T=
   HISTORY-ENTRIES stable T=
   2 HISTORY-SOURCE
   s" history-cache" AT RUN-SAVED-IN SUCCESS REUSED
   OUT$ S\" definition hits 2\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 42" CONTAINS? TTRUE
   HISTORY-ENTRIES stable T= ;

: TRACE$ ( -- ptr u8 n )
   s" trace" AT FILE-SIZE {: size:n :}
   size CAP > if E-FS-IO throw then
   s" trace" AT TRACE size READ-ALL size <> if E-FS-IO throw then
   TRACE size ;

: MARKED ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n source:ptr sourceu:n mark:ptr marku:n :}
   0 INPUT-U !
   source sourceu INPUT CAP INPUT-U BUF-APPEND
   S\" s\" " INPUT CAP INPUT-U BUF-APPEND
   s" trace" AT INPUT CAP INPUT-U BUF-APPEND
   S\" \" s\" " INPUT CAP INPUT-U BUF-APPEND
   mark marku INPUT CAP INPUT-U BUF-APPEND
   S\" \" APPEND-FILE\n" INPUT CAP INPUT-U BUF-APPEND
   name nameu INPUT INPUT-U @ PUT ;

: LEAF-SOURCE ( n -- )
   41 = if
      s" leaf.f"
         S\" require src/lib/fs-mutate.f\npackage FILE-LEAF public\n: VALUE ( -- n ) 41 ;\n;package\n"
         s" L" MARKED
   else
      s" leaf.f"
         S\" require src/lib/fs-mutate.f\npackage FILE-LEAF public\n: VALUE ( -- n ) 101 ;\n;package\n"
         s" L" MARKED
   then ;

: VALUE-SOURCE ( -- )
   s" value.f"
      S\" require src/lib/fs-mutate.f\nrequire leaf.f\npackage FILE-VALUE public\nFILE-LEAF:VALUE constant VALUE\n;package\n"
      s" V" MARKED ;

: APP-SOURCE ( bool bool -- )
   {: pad:bool extra:bool :}
   0 INPUT-U !
   S\" require src/lib/fs-mutate.f\nrequire leaf.f\nrequire value.f\npackage FILE-APP public\nvariable OBSERVED\nFILE-LEAF:VALUE OBSERVED !\n"
      INPUT CAP INPUT-U BUF-APPEND
   pad if S\" : PAD ( -- n ) 999 ;\n" INPUT CAP INPUT-U BUF-APPEND then
   S\" : LEFT ( -- n ) FILE-LEAF:VALUE 1+ ;\n: MAKE ( n -- ) create , does> ( -- n ) @ ;\n: WRAP ( n -- ) MAKE ;\n9 WRAP BOX\n: RIGHT ( -- n ) FILE-VALUE:VALUE 2 + ;\n: READ ( -- n ) OBSERVED @ ;\n;package\n: MAIN ( -- ) FILE-APP:LEFT . FILE-APP:BOX . FILE-APP:RIGHT . FILE-APP:READ .\n"
      INPUT CAP INPUT-U BUF-APPEND
   extra if S\" 7 .\n" INPUT CAP INPUT-U BUF-APPEND then
   S\" ;\n" INPUT CAP INPUT-U BUF-APPEND
   s" app.f" INPUT INPUT-U @ s" A" MARKED ;

: AUTO-CASE ( -- )
   s" source actions replay around cached native definitions" T-LABEL
   s" trace" s" " PUT
   41 LEAF-SOURCE VALUE-SOURCE false false APP-SOURCE
   RUN-DIRECT SUCCESS
   OUT$ S\" definition hits 0\n" CONTAINS? TTRUE
   OUT$ s" compiled " CONTAINS? TTRUE
   RUN-APP SUCCESS
   OUT$ S\" 42\n9\n43\n41\n" CONTAINS? TTRUE
   TRACE$ s" LVA" STR= TTRUE
   true false APP-SOURCE
   RUN-DIRECT SUCCESS REUSED
   RUN-APP SUCCESS
   OUT$ S\" 42\n9\n43\n41\n" CONTAINS? TTRUE
   TRACE$ s" LVALVA" STR= TTRUE
   true true APP-SOURCE
   RUN-DIRECT SUCCESS REUSED
   RUN-APP SUCCESS
   OUT$ S\" 42\n9\n43\n41\n7\n" CONTAINS? TTRUE
   TRACE$ s" LVALVALVA" STR= TTRUE
   101 LEAF-SOURCE
   RUN-DIRECT SUCCESS
   RUN-APP SUCCESS
   OUT$ S\" 102\n9\n103\n101\n7\n" CONTAINS? TTRUE
   TRACE$ s" LVALVALVALVA" STR= TTRUE
   \ Only the two colon neighbors can hit after MAIN changes; BOX is a create.
   s" app.f"
      S\" package FILE-NEIGHBOR public\n: LEFT ( -- n ) 40 ;\ncreate BOX 9 ,\n: RIGHT ( -- n ) 2 ;\n;package\n: MAIN ( -- ) FILE-NEIGHBOR:LEFT . FILE-NEIGHBOR:BOX @ . FILE-NEIGHBOR:RIGHT . ;\n" PUT
   RUN-DIRECT SUCCESS
   RUN-APP SUCCESS
   OUT$ S\" 40\n9\n2\n" CONTAINS? TTRUE
   s" app.f"
      S\" package FILE-NEIGHBOR public\n: LEFT ( -- n ) 40 ;\ncreate BOX 9 ,\n: RIGHT ( -- n ) 2 ;\n;package\n: MAIN ( -- ) FILE-NEIGHBOR:LEFT . FILE-NEIGHBOR:BOX @ . FILE-NEIGHBOR:RIGHT . 7 . ;\n" PUT
   RUN-DIRECT SUCCESS
   OUT$ S\" definition hits 2\n" CONTAINS? TTRUE
   RUN-APP SUCCESS
   OUT$ S\" 40\n9\n2\n7\n" CONTAINS? TTRUE ;

: PRIVATE-CASE ( -- )
   s" primitive retyping replaces a queried declaration and cached caller" T-LABEL
   \ FILE-NULL-SEED queries the declaration before PRIM: replaces its graph.
   \ RESET must retain the new null fence, including for a quotation pointee.
   s" app.f"
      S\" TRUSTED: FILE-NULL ( -- ptr a ) NULL-PTR ;\n: FILE-NULL-SEED ( -- ptr n ) FILE-NULL ;\nPRIM: FILE-NULL PE-PTR-A-NULL PE-OUT PRIM;\n: FILE-NULL-RESET ( ptr ptr a -- ) FILE-NULL swap ! ;\n: FILE-NULL-CHECK ( -- ) s\" CALL ( ptr ptr [ -- n ] -- ) FILE-NULL-RESET\" CHECK-CANDIDATE! 0 <> if s\" stale primitive effect admitted quotation\" 76 die then ;\nFILE-NULL-CHECK\nrequire number.f\n: MAIN ( -- ) 41 FILE-NUM:STEP . ;\n" PUT
   RUN-PRIVATE SUCCESS
   RUN-APP SUCCESS
   OUT$ s" 42" CONTAINS? TTRUE
   s" app.f"
      S\" package FILE-NUM private\nPPRIM: FILE-NUM 1+ PE-PTR-U8 PE-IN PE-PTR-U8 PE-OUT CLOSE-PRIVATE\n;package\nrequire number.f\n: MAIN ( -- ) 41 FILE-NUM:STEP . 7 . ;\n" PUT
   RUN-PRIVATE 70 T=
   ERR$ s" in step: at '1+'" CONTAINS? TTRUE
   ERR$ s" expected: ptr u8 actual: n" CONTAINS? TTRUE ;

: PRIVATE-CASES ( -- )
   [: s" private-cache" AT MAKE-DIR
      s" native-file-auto-primitive" WHITEBOX-CHILD:PROVIDE
      PRIVATE-CASE ;]
   [: WHITEBOX-CHILD:REMOVE ;] finally ;

: PRIMITIVE-CASES ( -- )
   s" primitive pointer consumer reuses its definition" T-LABEL
   s" pointer.f"
      S\" package FILE-PRIM public\n: NEXT ( ptr u8 -- ptr u8 ) 1+ ;\n;package\n" PUT
   s" app.f"
      S\" require pointer.f\n: MAIN ( -- ) s\" AB\" drop FILE-PRIM:NEXT c@ . ;\n" PUT
   RUN-DIRECT SUCCESS
   RUN-APP SUCCESS
   OUT$ s" 66" CONTAINS? TTRUE
   s" app.f"
      S\" require pointer.f\n: MAIN ( -- ) s\" AB\" drop FILE-PRIM:NEXT c@ . 7 . ;\n" PUT
   RUN-DIRECT SUCCESS
   REUSED
   RUN-APP SUCCESS
   OUT$ S\" 66\n7\n" CONTAINS? TTRUE

   s" number.f"
      S\" package FILE-NUM public\n: STEP ( n -- n ) 1+ ;\n;package\n" PUT
   s" app.f"
      S\" require number.f\n: MAIN ( -- ) 41 FILE-NUM:STEP . ;\n" PUT
   RUN-DIRECT SUCCESS
   RUN-APP SUCCESS
   OUT$ s" 42" CONTAINS? TTRUE

   s" incoming trust rejects the cached pointer consumer" T-LABEL
   s" app.f"
      S\" s\" 1+\" s\" n -- n\" trust\nrequire pointer.f\n: MAIN ( -- ) s\" AB\" drop FILE-PRIM:NEXT c@ . ;\n" PUT
   RUN-DIRECT 70 T=
   ERR$ s" in next: at '1+'" CONTAINS? TTRUE
   ERR$ s" expected: n actual: ptr u8" CONTAINS? TTRUE

   PRIVATE-CASES

   s" a redefined primitive spelling is a different occurrence" T-LABEL
   s" app.f"
      S\" package FILE-NUM public\n: 1+ ( n -- n ) 2 + ;\n;package\nrequire number.f\n: MAIN ( -- ) 41 FILE-NUM:STEP . ;\n" PUT
   RUN-DIRECT SUCCESS
   OUT$ S\" definition hits 0\n" CONTAINS? TTRUE
   RUN-APP SUCCESS
   OUT$ s" 43" CONTAINS? TTRUE ;

: EXECUTE-CASE ( -- )
   s" compiled execute keeps an exact seeded dependency" T-LABEL
   s" apply.f"
      S\" package FILE-ELAB public\n: APPLY ( n [ n -- n ] -- n ) execute ;\n;package\n" PUT
   s" app.f"
      S\" require apply.f\n: MAIN ( -- ) 41 [: 1+ ;] FILE-ELAB:APPLY . ;\n" PUT
   RUN-EXECUTE SUCCESS
   OUT$ S\" definition hits 0\n" CONTAINS? TTRUE
   RUN-APP SUCCESS
   OUT$ S\" 42\n" CONTAINS? TTRUE
   s" app.f"
      S\" require apply.f\n: MAIN ( -- ) 41 [: 1+ ;] FILE-ELAB:APPLY . 7 . ;\n" PUT
   RUN-EXECUTE SUCCESS
   REUSED
   RUN-APP SUCCESS
   OUT$ S\" 42\n7\n" CONTAINS? TTRUE
   s" app.f"
      S\" package FILE-ELAB private\n: execute ( n [ n -- n ] -- n ) drop 2 + ;\n;package\nrequire apply.f\n: MAIN ( -- ) 41 [: 1+ ;] FILE-ELAB:APPLY . 7 . ;\n" PUT
   RUN-EXECUTE SUCCESS
   OUT$ S\" definition hits 0\n" CONTAINS? TTRUE
   RUN-APP SUCCESS
   OUT$ S\" 43\n7\n" CONTAINS? TTRUE ;

: DEFINITION-CASE ( -- )
   s" a reused quotation still calls its external helper" T-LABEL
   s" step.f"
      S\" package DEF-STEP public\n: STEP ( n -- n ) 2 + ;\n;package\n" PUT
   s" app.f"
      S\" require step.f\n: APPLY ( n -- n ) [: DEF-STEP:STEP ;] execute ;\n: MAIN ( -- ) 40 APPLY . depth . ;\n" PUT
   RUN-DEFINITION SUCCESS
   OUT$ S\" definition hits 0\n" CONTAINS? TTRUE
   RUN-APP SUCCESS
   OUT$ S\" 42\n0\n" CONTAINS? TTRUE
   s" app.f"
      S\" require step.f\n: APPLY ( n -- n ) [: DEF-STEP:STEP ;] execute ;\n: MAIN ( -- ) 40 APPLY . 7 . depth . ;\n" PUT
   RUN-DEFINITION SUCCESS
   OUT$ S\" definition hits 2\n" CONTAINS? TTRUE
   RUN-APP SUCCESS
   OUT$ S\" 42\n7\n0\n" CONTAINS? TTRUE ;

: LITERAL-IMAGE-SAVE ( -- )
   s" app-hb" AT FILE-SIZE {: size:n :}
   size LITERAL-IMAGE-RESERVE
   s" app-hb" AT 0 LITERAL-IMAGE BYTE-VIEW size READ-ALL size T=
   size LITERAL-IMAGE-U ! ;

: LITERAL-IMAGE-SAME? ( -- bool )
   s" app-hb" AT FILE-SIZE {: size:n :}
   size LITERAL-IMAGE-U @ <> if false exit then
   size LITERAL-COLD-RESERVE
   s" app-hb" AT 0 LITERAL-COLD BYTE-VIEW size READ-ALL size <> if E-FS-IO throw then
   0 LITERAL-IMAGE BYTE-VIEW size 0 LITERAL-COLD BYTE-VIEW size STR= ;

: DIAG-CASE ( -- )
   s" opt-in fallback diagnostics follow actual compile attempts" T-LABEL
   s" diag-cache" AT MAKE-DIR
   s" app.f" S\" TRUSTED: WATCH ( n IR-CTX:ctx NCOMP:code-entry n n -- ) 2drop 2drop drop ;\n' WATCH NCOMP:PUBLISHED!\n: HELPER ( -- n ) 42 ;\nTRUSTED: MAIN ( -- ) HELPER . cr ;\n" PUT
   0 DIAG? !
   s" diag-cache" AT RUN-SAVED-IN SUCCESS
   ERR$ s" native-cache: compile-attempt " CONTAINS? TFALSE
   ERR$ s" native-cache: cache-store-refused " CONTAINS? TFALSE
   LITERAL-IMAGE-SAVE
   1 DIAG? !
   s" diag-cache" AT RUN-SAVED-IN SUCCESS
   0 DIAG? !
   s" diag.err" AT ERR$ WRITE-ALL
   DIAG-COMPILED 0 > TTRUE
   DIAG-LINES DIAG-COMPILED T=
   ERR$ s" reason=trusted name=MAIN path=" CONTAINS? TTRUE
   ERR$ s" cache-store-refused reason=not-cacheable name=HELPER path=" CONTAINS? TTRUE
   s" a definition the compiler keeps from the cache still joins its file's object" T-LABEL
   ERR$ s" /app.f key=" CONTAINS? TTRUE
   ERR$ s" /app.f reason=" CONTAINS? TFALSE
   LITERAL-IMAGE-SAME? TTRUE
   RUN-APP SUCCESS OUT$ s" 42" CONTAINS? TTRUE ;

\ The origin journal names only files under the engine root (engine-origin.f
\ ROOT-REL), and the saved builder restores that root before it loads app.f, so
\ the app declares its own directory as the root; image save drops it (RESET).
: BODY-EDIT-SOURCE ( n -- )
   {: value:n :}
   0 INPUT-U !
   S\" require src/habu/engine-origin.f\nENGINE-ORIGIN:ARM\n0 SCRIPT-ARGV$ SOURCE-ROOT:DIRNAME SOURCE-ROOT:ENGINE!\ns\" source-action\" type cr\n: LEFT ( -- n ) 1 {: left:n :} left [: 1+ ;] execute ;\n: MIDDLE ( -- n ) "
      INPUT CAP INPUT-U BUF-APPEND
   value 0 < if
      S\" 40 ;\n" INPUT CAP INPUT-U BUF-APPEND
   else
      value 100 < if
         S\" 41 ;\n" INPUT CAP INPUT-U BUF-APPEND
      else S\" 101 ;\n" INPUT CAP INPUT-U BUF-APPEND then
   then
   S\" : RIGHT ( -- n ) true if s\" a;b\" nip [: s\" q;r\" 2drop [: 1+ ;] execute ;] execute else 0 then ; \\ ; in a comment\n: MAIN ( -- ) LEFT MIDDLE RIGHT + + . cr ;\n"
      INPUT CAP INPUT-U BUF-APPEND
   INPUT INPUT-U @ s" : RIGHT " FIND-SUB MATCH option
      none OF E-FS-IO throw ENDOF
      some OF IDX>N 2 + ENDOF
   ;MATCH {: right:n :}
   S\" : CHECK-RIGHT-ORIGIN ( -- )\n  0 SCRIPT-ARGV$ "
      INPUT CAP INPUT-U BUF-APPEND
   SB-RESET right FMT:SB-INT SB$ INPUT CAP INPUT-U BUF-APPEND
   S\"  ENGINE-ORIGIN:SYMBOL-AT if\n    ENGINE-ORIGIN:TARGET\n      {: path:ptr pathu:n at:n end:n name:ptr nameu:n live:bool :}\n    live at "
      INPUT CAP INPUT-U BUF-APPEND
   SB-RESET right FMT:SB-INT SB$ INPUT CAP INPUT-U BUF-APPEND
   S\"  = and end at 5 + = and\n    path pathu s\" app.f\" STR= and\n    name nameu s\" RIGHT\" STR= and 0= if 77 throw then\n  else drop 77 throw then\n  s\" right-origin=current\" type cr ;\nCHECK-RIGHT-ORIGIN\n"
      INPUT CAP INPUT-U BUF-APPEND
   s" app.f" INPUT INPUT-U @ PUT ;

: SOURCE-ACTION-ONCE ( -- )
   OUT$ {: out:ptr outu:n :}
   s" source-action" {: action:ptr actionu:n :}
   out outu action actionu FIND-SUB MATCH option
      none OF false TTRUE ENDOF
      some OF
         IDX>N actionu + {: after:n :}
         out after + outu after - action actionu CONTAINS? TFALSE
      ENDOF
   ;MATCH ;

: BODY-EDIT-CASE ( -- )
   s" unchanged neighbors reuse across same-file body edits" T-LABEL
   s" body-edit-cache" AT MAKE-DIR
   s" body-edit-fresh-cache" AT MAKE-DIR
   -1 BODY-EDIT-SOURCE
   s" body-edit-cache" AT RUN-SAVED-IN SUCCESS
   SOURCE-ACTION-ONCE
   OUT$ s" right-origin=current" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 46" CONTAINS? TTRUE
   41 BODY-EDIT-SOURCE
   s" body-edit-cache" AT RUN-SAVED-IN SUCCESS
   \ LEFT, RIGHT, MAIN and CHECK-RIGHT-ORIGIN; only MIDDLE changed.
   OUT$ S\" definition hits 4\n" CONTAINS? TTRUE
   SOURCE-ACTION-ONCE
   OUT$ s" right-origin=current" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 47" CONTAINS? TTRUE
   101 BODY-EDIT-SOURCE
   s" body-edit-cache" AT RUN-SAVED-IN SUCCESS
   \ LEFT and the shifted RIGHT and MAIN; MIDDLE and CHECK-RIGHT-ORIGIN, which
   \ spells RIGHT's offset, compile.
   OUT$ S\" definition hits 3\n" CONTAINS? TTRUE
   SOURCE-ACTION-ONCE
   OUT$ s" right-origin=current" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 107" CONTAINS? TTRUE
   LITERAL-IMAGE-SAVE
   s" body-edit-fresh-cache" AT RUN-SAVED-IN SUCCESS
   OUT$ S\" definition hits 0\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 107" CONTAINS? TTRUE
   LITERAL-IMAGE-SAME? TTRUE ;

: BODY-READER-CASE ( -- )
   s" a newly parsing binding cannot reuse an unchanged body" T-LABEL
   s" body-reader-cache" AT MAKE-DIR
   \ Only READER-KEEP can be reused; MIDDLE changes on each build.
   s" reader.f"
      S\" package FILE-READER public\n: STEP ( -- ) s\" mode-step\" type ;\n;package\n"
      PUT
   s" app.f"
      S\" require reader.f\n: READER-KEEP ( -- n ) 42 FILE-READER:STEP ;\n: MIDDLE ( -- n ) 1 ;\nTRUSTED: MAIN ( -- ) READER-KEEP MIDDLE + . cr ;\n"
      PUT
   s" body-reader-cache" AT RUN-SAVED-IN SUCCESS
   RUN-APP SUCCESS OUT$ s" 43" CONTAINS? TTRUE
   OUT$ s" mode-step" CONTAINS? TTRUE
   s" reader.f"
      S\" package FILE-READER public\n: STEP ( -- ) s\" mode-step\" type ; immediate\n;package\ns\" FILE-READER:STEP\" 0 parse-imm\n"
      PUT
   s" app.f"
      S\" require reader.f\n: READER-KEEP ( -- n ) 42 FILE-READER:STEP ;\n: MIDDLE ( -- n ) 3 ;\nTRUSTED: MAIN ( -- ) READER-KEEP MIDDLE + . cr ;\n"
      PUT
   s" body-reader-cache" AT RUN-SAVED-IN SUCCESS
   OUT$ s" mode-step" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 45" CONTAINS? TTRUE
   OUT$ s" mode-step" CONTAINS? TFALSE ;

: LITERAL-EVENT-CASE ( -- )
   s" a reused body follows source literal calls after an edited prefix" T-LABEL
   s" literal-fresh-cache" AT MAKE-DIR
   s" prefix.f" S\" package FILE-LITERAL public\n: OLD ( -- ptr u8 n ) s\" oldBBBB\" ;\n;package\n" PUT
   s" app.f" S\" require prefix.f\nCAST: LIT-PTR>N ( ptr u8 -- n )\n: BODY ( -- ptr u8 n ) s\" seven77\" 2drop s\" \" 2drop s\" twelve123456\" 2drop s\" oldBBBB\" ;\n: BODY-LOOP ( n -- n ) {: len:n :} 0 len 0 ?do c\" oldBBBB\" LIT-PTR>N + loop ;\ns\" address-loop-folded \" type NLOOP:FOLDED . cr\n: SCALAR-LOOP ( n -- n ) {: len:n :} 0 len 0 ?do 42 + loop ;\ns\" scalar-loop-folded \" type NLOOP:FOLDED . cr\n: BODY-PTR ( -- n ) c\" oldBBBB\" LIT-PTR>N ;\n: MAIN ( -- ) BODY type cr 3 BODY-LOOP 3 BODY-PTR * = if s\" loop-ok\" else s\" loop-bad\" then type cr ;\n" PUT
   RUN-DEFINITION SUCCESS
   OUT$ s" address-loop-folded 0" CONTAINS? TTRUE
   OUT$ s" scalar-loop-folded 1" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" oldBBBB" CONTAINS? TTRUE
   OUT$ s" loop-ok" CONTAINS? TTRUE
   s" prefix.f" S\" package FILE-LITERAL public\n: OLD ( -- ptr u8 n ) s\" changed-prefix\" ;\n;package\n" PUT
   RUN-DEFINITION SUCCESS REUSED
   RUN-APP SUCCESS OUT$ s" oldBBBB" CONTAINS? TTRUE
   OUT$ s" loop-ok" CONTAINS? TTRUE
   LITERAL-IMAGE-SAVE
   RUN-LITERAL-FRESH SUCCESS
   RUN-APP SUCCESS OUT$ s" oldBBBB" CONTAINS? TTRUE
   OUT$ s" loop-ok" CONTAINS? TTRUE
   LITERAL-IMAGE-SAME? TTRUE ;

: LITERAL-LOOP-CASE ( -- )
   s" the counted literal loop alone reuses its native object" T-LABEL
   s" literal-loop-cache" AT MAKE-DIR
   s" literal-loop-fresh-cache" AT MAKE-DIR
   s" prefix.f" S\" package FILE-LITERAL public\n: OLD ( -- ptr u8 n ) s\" oldBBBB\" ;\n;package\n" PUT
   s" app.f" S\" require prefix.f\nCAST: LIT-PTR>N ( ptr u8 -- n )\n: BODY-LOOP ( n -- n ) {: len:n :} 0 len 0 ?do c\" oldBBBB\" LIT-PTR>N + loop ;\ns\" address-loop-folded \" type NLOOP:FOLDED . cr\n: MAIN ( -- ) FILE-LITERAL:OLD type cr 3 BODY-LOOP 3 c\" oldBBBB\" LIT-PTR>N * = if s\" loop-ok\" else s\" loop-bad\" then type cr ;\n" PUT
   RUN-LITERAL-LOOP SUCCESS
   OUT$ s" address-loop-folded 0" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ S\" oldBBBB\nloop-ok\n" CONTAINS? TTRUE
   s" prefix.f" S\" package FILE-LITERAL public\n: OLD ( -- ptr u8 n ) s\" changed-prefix\" ;\n;package\n" PUT
   \ Only BODY-LOOP keeps its source: OLD and MAIN change, and CAST: is a declaration.
   s" app.f" S\" require prefix.f\nCAST: LIT-PTR>N ( ptr u8 -- n )\n: BODY-LOOP ( n -- n ) {: len:n :} 0 len 0 ?do c\" oldBBBB\" LIT-PTR>N + loop ;\ns\" address-loop-folded \" type NLOOP:FOLDED . cr\n: MAIN ( -- ) FILE-LITERAL:OLD type cr 3 BODY-LOOP 3 c\" oldBBBB\" LIT-PTR>N * = if s\" loop-ok\" else s\" loop-bad\" then type cr 7 drop ;\n" PUT
   RUN-LITERAL-LOOP SUCCESS
   OUT$ S\" definition hits 1\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ S\" changed-prefix\nloop-ok\n" CONTAINS? TTRUE
   LITERAL-IMAGE-SAVE
   RUN-LITERAL-LOOP-FRESH SUCCESS
   OUT$ S\" definition hits 0\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ S\" changed-prefix\nloop-ok\n" CONTAINS? TTRUE
   LITERAL-IMAGE-SAME? TTRUE ;

: SEGMENT-PREFIX ( bool -- )
   {: large:bool :}
   large if
      s" prefix.f"
         S\" package FILE-LITERAL public\n: LARGE? ( -- bool ) true ;\n;package\n"
         PUT
   else
      s" prefix.f"
         S\" package FILE-LITERAL public\n: LARGE? ( -- bool ) false ;\n;package\n"
         PUT
   then
   ;

: SEGMENT-APP ( -- )
   s" app.f"
      S\" require prefix.f\ncreate BODY-FILL $80000 allot\n: BODY-FILL! ( -- ) $80000 0 ?do 71 BODY-FILL i + c! loop ;\n: BODY-USED ( -- n ) NSTR:SEGMENTS 1- NSTR:SOURCE-ROWS {: arena:ptr rows:n offsets:ptr lengths:ptr :} 0 rows 0 ?do lengths i cells + @ + loop ;\n: BODY-PACK ( -- ) $80000 BODY-USED 12 + - {: bytes:n :} bytes 0 > if BODY-FILL bytes NSTR:INTERN drop then ;\n: BODY-PACK-IF ( -- ) FILE-LITERAL:LARGE? if BODY-PACK then ;\nBODY-FILL! BODY-PACK-IF\n: BODY ( -- bool ) s\" repeatBBBB\" drop s\" FFFFFFFFFFFFFFFFFFFF\" 2drop s\" repeatBBBB\" drop = ;\n: MAIN ( -- ) BODY if s\" share\" else s\" split\" then type cr ;\n"
      PUT ;

: SEGMENT-EVENT-CASE ( -- )
   s" unused literal events preserve sharing across segment splits and merges" T-LABEL
   s" segment-split-fresh-cache" AT MAKE-DIR
   s" segment-merge-fresh-cache" AT MAKE-DIR
   false SEGMENT-PREFIX SEGMENT-APP
   RUN-DEFINITION SUCCESS
   RUN-APP SUCCESS OUT$ s" share" CONTAINS? TTRUE
   true SEGMENT-PREFIX
   RUN-DEFINITION SUCCESS
   RUN-APP SUCCESS OUT$ s" split" CONTAINS? TTRUE
   LITERAL-IMAGE-SAVE
   RUN-SEGMENT-SPLIT-FRESH SUCCESS
   RUN-APP SUCCESS OUT$ s" split" CONTAINS? TTRUE
   LITERAL-IMAGE-SAME? TTRUE
   false SEGMENT-PREFIX
   RUN-DEFINITION SUCCESS
   RUN-APP SUCCESS OUT$ s" share" CONTAINS? TTRUE
   LITERAL-IMAGE-SAVE
   RUN-SEGMENT-MERGE-FRESH SUCCESS
   RUN-APP SUCCESS OUT$ s" share" CONTAINS? TTRUE
   LITERAL-IMAGE-SAME? TTRUE ;

\ ROLLED runs at load, so sdfail generates SDFAIL:MAKE inside the build and then
\ fails at commit on the SDFAIL:A it would derive; the build catches the code.
: GENERATED-SOURCE ( n -- )
   {: last:n :}
   0 INPUT-U !
   S\" ENUM shade VARIANT red ;VARIANT VARIANT blue ;VARIANT ;ENUM\nSTRUCTURE pair DERIVE addr FIELD a n FIELD b n ;STRUCTURE\n: SDFAIL:A ( -- n ) 7 ;\n: ROLLED ( -- n ) [: s\" STRUCTURE sdfail DERIVE addr FIELD a n ;STRUCTURE\" evaluate-closed ;] catch ;\nROLLED constant ROLL-RC\n: MAIN ( -- ) 1 2 PAIR:MAKE PAIR:UNMAKE + . ROLL-RC . SDFAIL:A . "
      INPUT CAP INPUT-U BUF-APPEND
   SB-RESET last FMT:SB-INT SB$ INPUT CAP INPUT-U BUF-APPEND
   S\"  . ;\n" INPUT CAP INPUT-U BUF-APPEND
   s" app.f" INPUT INPUT-U @ PUT ;

: GENERATED-CASE ( -- )
   s" words a declaration generates store and reuse like other definitions" T-LABEL
   1 DIAG? !
   40 GENERATED-SOURCE
   s" generated-cache" AT RUN-SAVED-IN SUCCESS
   s" generated-cold.err" AT ERR$ WRITE-ALL
   ERR$ S\" compile-attempt reason=eligible-cache-miss name=RED\n" CONTAINS? TTRUE
   ERR$ S\" compile-attempt reason=eligible-cache-miss name=MAKE\n" CONTAINS? TTRUE
   ERR$ s" reason=checker-busy" CONTAINS? TFALSE
   RUN-APP SUCCESS OUT$ S\" 3\n7110\n7\n40\n" CONTAINS? TTRUE
   41 GENERATED-SOURCE
   s" generated-cache" AT RUN-SAVED-IN SUCCESS
   s" generated-warm.err" AT ERR$ WRITE-ALL
   REUSED
   \ Only MAIN's source changed, so every other word, generated or not, hits.
   DIAG-LINES 1 T=
   ERR$ s" name=MAIN path=" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ S\" 3\n7110\n7\n41\n" CONTAINS? TTRUE
   s" generated-cache" AT RUN-SAVED-IN SUCCESS REUSED
   DIAG-LINES 0 T=
   0 DIAG? ! ;

\ HIDE-DEFS-FROM lowers the dictionary count with no code reclaim, so CUT-NEW
\ takes CUT-OLD's ordinal. The file's object drops CUT-OLD at the cut; a row
\ left behind would repeat the ordinal and the object would not read back.
: CUT-CASE ( -- )
   s" a dictionary cut drops the hidden definition from the file object" T-LABEL
   1 DIAG? !
   s" cut-cache" AT MAKE-DIR
   s" app.f"
      S\" : CUT-OLD ( -- n ) 5 ;\ns\" CUT-OLD\" HIDE-DEFS-FROM\n: CUT-NEW ( -- n ) 6 ;\n: MAIN ( -- ) CUT-NEW . ;\n" PUT
   s" cut-cache" AT RUN-SAVED-IN SUCCESS
   s" cut-cold.err" AT ERR$ WRITE-ALL
   ERR$ s" native-file: refused" CONTAINS? TFALSE
   ERR$ s"  defs=2 " CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ S\" 6\n" CONTAINS? TTRUE
   s" cut-cache" AT RUN-SAVED-IN SUCCESS REUSED
   s" cut-warm.err" AT ERR$ WRITE-ALL
   ERR$ s" native-file: refused" CONTAINS? TFALSE
   ERR$ s"  defs=2 " CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ S\" 6\n" CONTAINS? TTRUE
   0 DIAG? ! ;

public
: MAIN ( -- )
   T-RESET
   s" native-file-auto" TREE
   s" direct-cache" AT MAKE-DIR
   s" execute-cache" AT MAKE-DIR
   s" definition-cache" AT MAKE-DIR
   s" history-cache" AT MAKE-DIR
   s" generated-cache" AT MAKE-DIR
   SCRIPT-ARGC 0 > if
      0 SCRIPT-ARGV$ s" primitive" STR= if
         PRIMITIVE-CASES
         T-REPORT
         s" native file auto artifacts: " type ROOT$ type cr
         exit
      then
      0 SCRIPT-ARGV$ s" diag" STR= if
         DIAG-CASE
         T-REPORT
         s" native file auto artifacts: " type ROOT$ type cr
         exit
      then
      0 SCRIPT-ARGV$ s" history" STR= if
         HISTORY-CASE
         T-REPORT
         s" native file auto artifacts: " type ROOT$ type cr
         exit
      then
      0 SCRIPT-ARGV$ s" actions" STR= if
         AUTO-CASE
         T-REPORT
         s" native file auto artifacts: " type ROOT$ type cr
         exit
      then
      0 SCRIPT-ARGV$ s" execute" STR= if
         EXECUTE-CASE
         T-REPORT
         s" native file auto artifacts: " type ROOT$ type cr
         exit
      then
      0 SCRIPT-ARGV$ s" definition" STR= if
         DEFINITION-CASE
         LITERAL-EVENT-CASE
         LITERAL-LOOP-CASE
         SEGMENT-EVENT-CASE
         T-REPORT
         s" native file auto artifacts: " type ROOT$ type cr
         exit
      then
      0 SCRIPT-ARGV$ s" literal" STR= if
         LITERAL-EVENT-CASE
         LITERAL-LOOP-CASE
         T-REPORT
         s" native file auto artifacts: " type ROOT$ type cr
         exit
      then
      0 SCRIPT-ARGV$ s" segment" STR= if
         SEGMENT-EVENT-CASE
         T-REPORT
         s" native file auto artifacts: " type ROOT$ type cr
         exit
      then
      0 SCRIPT-ARGV$ s" body" STR= if
         BODY-EDIT-CASE
         BODY-READER-CASE
         T-REPORT
         s" native file auto artifacts: " type ROOT$ type cr
         exit
      then
      0 SCRIPT-ARGV$ s" body-edit" STR= if
         BODY-EDIT-CASE
         T-REPORT
         s" native file auto artifacts: " type ROOT$ type cr
         exit
      then
      0 SCRIPT-ARGV$ s" body-reader" STR= if
         BODY-READER-CASE
         T-REPORT
         s" native file auto artifacts: " type ROOT$ type cr
         exit
      then
      0 SCRIPT-ARGV$ s" generated" STR= if
         GENERATED-CASE
         T-REPORT
         s" native file auto artifacts: " type ROOT$ type cr
         exit
      then
      0 SCRIPT-ARGV$ s" cut" STR= if
         CUT-CASE
         T-REPORT
         s" native file auto artifacts: " type ROOT$ type cr
         exit
      then
      0 SCRIPT-ARGV$ s" hook" STR= if
         HOOK-CASE
         T-REPORT
         s" native file auto artifacts: " type ROOT$ type cr
         exit
      then
   then
   AUTO-CASE
   DIAG-CASE
   PRIMITIVE-CASES
   EXECUTE-CASE
   DEFINITION-CASE
   LITERAL-EVENT-CASE
   LITERAL-LOOP-CASE
   SEGMENT-EVENT-CASE
   HISTORY-CASE
   BODY-EDIT-CASE
   BODY-READER-CASE
   HOOK-CASE
   GENERATED-CASE
   CUT-CASE
   T-REPORT
   s" native file auto artifacts: " type ROOT$ type cr ;
;package
;using

NATIVE-FILE-AUTO-TEST:MAIN
