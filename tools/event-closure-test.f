\ event-closure-test.f - checked fixtures for tools/event-closure-lib.f.
\ Run: bin/hb --load src/lib/errors.f src/lib/string.f src/lib/test.f src/lib/memory.f src/lib/fs.f
\ src/lib/fs-mutate.f src/lib/source.f src/lib/content-key.f tools/source-discovery.f
\ tools/event-closure-lib.f tools/event-closure-test.f
\
\ Proves the ordered transitive closure list (require/included followed, provided
\ and require-known not, canonical-path dedup, a missing file refused, transitive
\ descent) and the property a closure key relies on: a content edit to
\ any closure file changes the closure key, while an untouched closure or an edit
\ to a non-closure file leaves it stable.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/source.f
require src/lib/content-key.f
require src/lib/fmt.f                  \ the code a child prints is rendered from its name
require src/lib/test/outcome.f
require src/lib/test/subject.f
require tools/source-discovery.f
require tools/event-closure-lib.f

package EC-TEST

FS-PATH-CAP constant ECT-PC

create ECT-ROOT ECT-PC allot
create ECT-ENTRY ECT-PC allot
create ECT-DA ECT-PC allot
create ECT-DB ECT-PC allot
create ECT-DC ECT-PC allot
create ECT-UN ECT-PC allot
create ECT-ALIAS ECT-PC allot
create ECT-KEY-A 80 allot
create ECT-KEY-B 80 allot
create ECT-READ-SRC $1000 allot
create ECT-RA-ENTRY FS-PATH-CAP allot
create ECT-RB-ENTRY FS-PATH-CAP allot
create ECT-SHARED FS-PATH-CAP allot
create ECT-PATH FS-PATH-CAP allot
create ECT-RAW FS-PATH-CAP 1+ 2 * allot
variable ECT-ROOT-U
variable ECT-ENTRY-U
variable ECT-DA-U
variable ECT-DB-U
variable ECT-DC-U
variable ECT-UN-U
variable ECT-ALIAS-U
variable ECT-I
variable ECT-RA-ENTRY-U
variable ECT-RB-ENTRY-U
variable ECT-SHARED-U
variable ECT-PATH-U
variable ECT-RAW-U
variable ECT-BEFORE-ENTRY
variable ECT-BEFORE-DEP
variable ECT-BEFORE-CALLS

: ECT-ROOT$ ( -- ptr u8 n )   ECT-ROOT ECT-ROOT-U @ ;
: ECT-RA-ENTRY$ ( -- ptr u8 n ) ECT-RA-ENTRY ECT-RA-ENTRY-U @ ;
: ECT-RB-ENTRY$ ( -- ptr u8 n ) ECT-RB-ENTRY ECT-RB-ENTRY-U @ ;
: ECT-SHARED$ ( -- ptr u8 n ) ECT-SHARED ECT-SHARED-U @ ;
: ECT-PATH$ ( -- ptr u8 n ) ECT-PATH ECT-PATH-U @ ;
: ECT-ENTRY$ ( -- ptr u8 n )  ECT-ENTRY ECT-ENTRY-U @ ;
: ECT-DA$ ( -- ptr u8 n )     ECT-DA ECT-DA-U @ ;
: ECT-DB$ ( -- ptr u8 n )     ECT-DB ECT-DB-U @ ;
: ECT-DC$ ( -- ptr u8 n )     ECT-DC ECT-DC-U @ ;
: ECT-UN$ ( -- ptr u8 n )     ECT-UN ECT-UN-U @ ;

: ECT-MK ( ptr u8 n ptr u8 ptr n -- ) {: na:ptr nu:n dst:ptr up:ptr :}
   ECT-ROOT$ na nu dst JOIN-PATH up ! ;

: ECT-DQ ( -- )   34 SB-APPEND-C ;
: ECT-SP ( -- )   32 SB-APPEND-C ;
: ECT-LF ( -- )   10 SB-APPEND-C ;

: ECT-REQ-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   s" require " SB-APPEND
   a u SB-APPEND
   ECT-LF ;

: ECT-PROVIDED-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   115 SB-APPEND-C
   ECT-DQ ECT-SP
   a u SB-APPEND
   ECT-DQ ECT-SP
   s" provided" SB-APPEND
   ECT-LF ;

: ECT-WRITE ( ptr u8 n ptr u8 n -- ) {: pa:ptr pu:n ca:ptr cu:n :}
   pa pu ca cu WRITE-ALL ;

: ECT-WRITE-REL ( ptr u8 n ptr u8 n -- )
   {: na:ptr nu:n ca:ptr cu:n :}
   na nu ECT-PATH ECT-PATH-U ECT-MK
   ECT-PATH$ ca cu ECT-WRITE ;

: ECT-READ-PATH ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: pa:ptr pu:n root:ptr rootu:n :}
   root rootu 2drop
   pa pu ECT-READ-SRC $1000 READ-ALL ECT-READ-SRC swap ;

67 constant ECT-UNCAUGHT-RC
20000 constant ECT-CHILD-MS
$100 constant ECT-OUT-CAP
$1000 constant ECT-ERR-CAP
ECT-OUT-CAP BUFFER: ECT-OUT
ECT-ERR-CAP BUFFER: ECT-ERR

: ECT-PREP ( -- )
   CLEANUP-RESET
   s" habu-event-closure-test" HB-TMP-MKDIR SOURCE-ROOT:CANONICAL TTRUE {: a:ptr u:n :}
   a ECT-ROOT u BYTE-COPY  u ECT-ROOT-U !
   ECT-ROOT$ CLEANUP-TREE+
   s" entry.f" ECT-ENTRY ECT-ENTRY-U ECT-MK
   s" dep-a.f" ECT-DA ECT-DA-U ECT-MK
   s" dep-b.f" ECT-DB ECT-DB-U ECT-MK
   s" dep-c.f" ECT-DC ECT-DC-U ECT-MK
   s" unrelated.f" ECT-UN ECT-UN-U ECT-MK
   s" alias.f" ECT-ALIAS ECT-ALIAS-U ECT-MK ;

: ECT-KEY-BODY ( ptr u8 n ptr u8 CONTENT-KEY:fold -- )
   >r {: a:ptr u:n dst:ptr :} r>
   s" ect-closure-v1" CONTENT-KEY:TEXT+
   a u EC:BUILD
   0 ECT-I !
   begin ECT-I @ EC:COUNT < while
      ECT-I @ EC:PATH$ CONTENT-KEY:FILE+
      ECT-I @ 1+ ECT-I !
   repeat
   dst CONTENT-KEY:FINAL-HEX ;

: ECT-KEY-FOR ( ptr u8 n ptr u8 -- )
   [: ECT-KEY-BODY ;] CONTENT-KEY:WITH-FOLD ;

: ECT-CLOSURE-KEY ( ptr u8 -- )
   ECT-ENTRY$ rot ECT-KEY-FOR ;

\ --- ordering / dedup ---------------------------------------------------------
\ One require line per write: SB holds one path and its line, not three.
: ECT-REQ+ ( ptr u8 n -- ) {: a:ptr u:n :}
   SB-RESET a u ECT-REQ-LINE ECT-ENTRY$ SB$ APPEND-FILE ;

: ECT-TEST-ORDER ( -- )
   ECT-DA$ s\" \\ a\n" ECT-WRITE
   ECT-DB$ s\" \\ b\n" ECT-WRITE
   ECT-ENTRY$ s" " ECT-WRITE
   ECT-DA$ ECT-REQ+
   ECT-DB$ ECT-REQ+
   ECT-DA$ ECT-REQ+
   ECT-ENTRY$ EC:BUILD
   EC:COUNT 3 T=
   0 EC:PATH$ ECT-ENTRY$ T$=
   1 EC:PATH$ ECT-DA$ T$=
   2 EC:PATH$ ECT-DB$ T$= ;

\ --- transitive descent -------------------------------------------------------
: ECT-TEST-TRANSITIVE ( -- )
   ECT-DC$ s\" \\ c\n" ECT-WRITE
   SB-RESET ECT-DC$ ECT-REQ-LINE ECT-DA$ SB$ ECT-WRITE
   SB-RESET ECT-DA$ ECT-REQ-LINE ECT-ENTRY$ SB$ ECT-WRITE
   ECT-ENTRY$ EC:BUILD
   EC:COUNT 3 T=
   0 EC:PATH$ ECT-ENTRY$ T$=
   1 EC:PATH$ ECT-DA$ T$=
   2 EC:PATH$ ECT-DC$ T$= ;

\ A caller may hand BUILD the engine's transient JOIN result. The closure
\ must keep that entry across its harvest and replay passes.
: ECT-TEST-TRANSIENT-ENTRY ( -- )
   ECT-DA$ s" " ECT-WRITE
   ECT-ENTRY$ s" require dep-a.f" ECT-WRITE
   ECT-ROOT$ s" entry.f" SOURCE-ROOT:JOIN EC:BUILD
   EC:COUNT 2 T=
   0 EC:PATH$ ECT-ENTRY$ T$=
   1 EC:PATH$ ECT-DA$ T$=
   ECT-ROOT$ s" entry.f" SOURCE-ROOT:JOIN
      [: ECT-READ-PATH ;] EC:BUILD-WITH
   EC:COUNT 2 T=
   ECT-ROOT$ s" entry.f" SOURCE-ROOT:JOIN EC:LOAD-ORDER
   EC:ORDER-COUNT 2 T=
   1 EC:ORDER-PATH$ ECT-ENTRY$ T$= ;

\ A selected entry below its owner still finds a sibling at that owner's root.
\ The observer changes each source before discovery reads it; the resulting
\ transitive list is the repeatable artifact of the observation boundary.
: ECT-BEFORE ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u ECT-PATH$ STR= ECT-BEFORE-ENTRY @ 0= and if
      1 ECT-BEFORE-ENTRY !
      ECT-PATH$ s" require dep-a.f" ECT-WRITE
   then
   a u ECT-DA$ STR= ECT-BEFORE-DEP @ 0= and if
      1 ECT-BEFORE-DEP !
      ECT-DA$ s" require dep-b.f" ECT-WRITE
   then ;

: ECT-TEST-SELECTED-ROOT ( -- )
   ECT-ROOT$ s" sub" SOURCE-ROOT:JOIN MAKE-DIR
   s" sub/entry.f" ECT-PATH ECT-PATH-U ECT-MK
   ECT-PATH$ s" " ECT-WRITE
   ECT-DA$ s" " ECT-WRITE
   ECT-DB$ s" " ECT-WRITE
   ECT-PATH$ ECT-ROOT$ EC:BUILD-IN
   EC:COUNT 1 T=
   0 EC:NAME$ s" sub/entry.f" T$=
   0 ECT-BEFORE-ENTRY ! 0 ECT-BEFORE-DEP !
   ECT-PATH$ ECT-ROOT$ [: ECT-BEFORE ;] EC:BUILD-IN-EACH
   ECT-BEFORE-ENTRY @ 1 T=
   ECT-BEFORE-DEP @ 1 T=
   EC:COUNT 3 T=
   0 EC:PATH$ ECT-PATH$ T$=
   1 EC:PATH$ ECT-DA$ T$=
   2 EC:PATH$ ECT-DB$ T$= ;

\ A caller's observer error must leave the harvest, even when its code is
\ also one that discovery suppresses for a source scan.
: ECT-BEFORE-THROWS-ONCE ( ptr u8 n -- )
   2drop
   1 ECT-BEFORE-CALLS +!
   ECT-BEFORE-CALLS @ 1 = if E-FS-PATH throw then ;

: ECT-TEST-BEFORE-ERROR ( -- )
   ECT-ENTRY$ s" " ECT-WRITE
   0 ECT-BEFORE-CALLS !
   [: ECT-ENTRY$ ECT-ROOT$ [: ECT-BEFORE-THROWS-ONCE ;]
      EC:BUILD-IN-EACH ;] E-FS-PATH TTHROWSQ
   ECT-BEFORE-CALLS @ 1 T= ;

\ A spelling longer than PATH-CAP can normalize to a valid source path.
: ECT-TEST-LONG-ENTRY ( -- )
   ECT-ENTRY$ s" " ECT-WRITE
   47 ECT-RAW c!
   500 0 ?do
      46 ECT-RAW 1 i 2 * + + c!
      47 ECT-RAW 2 i 2 * + + c!
   loop
   ECT-ENTRY$ {: a:ptr u:n :}
   a 1+ ECT-RAW 1001 + u 1- BYTE-COPY
   u 1000 + ECT-RAW-U !
   ECT-RAW ECT-RAW-U @ EC:BUILD
   EC:COUNT 1 T=
   0 EC:PATH$ ECT-ENTRY$ T$=
   ECT-RAW ECT-RAW-U @ EC:LOAD-ORDER
   EC:ORDER-COUNT 1 T=
   ECT-RAW ECT-RAW-U @ [: ECT-READ-PATH ;] EC:BUILD-WITH
   EC:COUNT 1 T= ;

\ A field named like a loader belongs to the parsed declaration. The next
\ require is a real dependency and must still enter the closure.
: ECT-TEST-FAMILY-FIELD ( -- )
   ECT-DA$ s\" \\ dependency\n" ECT-WRITE
   ECT-ENTRY$ S\" require dep-a.f\nSTRUCTURE box FIELD include n ;STRUCTURE\n: structure ( -- ) ;\n"
      ECT-WRITE
   ECT-ENTRY$ EC:BUILD
   EC:COUNT 2 T=
   0 EC:PATH$ ECT-ENTRY$ T$=
   1 EC:PATH$ ECT-DA$ T$= ;

\ A later dependency cannot change an earlier standard declaration.
: ECT-TEST-FAMILY-LATE-DEPENDENCY ( -- )
   ECT-DA$ s" " ECT-WRITE
   ECT-DB$ s" : structure ( -- ) ;" ECT-WRITE
   ECT-ENTRY$ S\" package DEMO require dep-a.f STRUCTURE box FIELD x n ;STRUCTURE require dep-b.f ;package\n"
      ECT-WRITE
   ECT-ENTRY$ EC:BUILD
   EC:COUNT 3 T= ;

\ --- provided registers but does not load -------------------------------------
: ECT-TEST-PROVIDED ( -- )
   ECT-DA$ s\" \\ a\n" ECT-WRITE
   SB-RESET ECT-DA$ ECT-PROVIDED-LINE ECT-ENTRY$ SB$ ECT-WRITE
   ECT-ENTRY$ EC:BUILD
   EC:COUNT 1 T=
   0 EC:PATH$ ECT-ENTRY$ T$= ;

\ A provided path skipped by require can still load at a later include. The
\ closure order follows the loads that ran, not the earlier skipped require.
: ECT-TEST-PROVIDED-INCLUDE-ORDER ( -- )
   SB-RESET s" dep-b.f" ECT-PROVIDED-LINE ECT-DA$ SB$ ECT-WRITE
   ECT-DB$ s" 66 emit" ECT-WRITE
   ECT-DC$ s" " ECT-WRITE
   ECT-ENTRY$ S\" require dep-a.f require dep-b.f require dep-c.f include dep-b.f\n" ECT-WRITE
   ECT-ENTRY$ EC:BUILD
   EC:COUNT 4 T=
   0 EC:PATH$ ECT-ENTRY$ T$=
   1 EC:PATH$ ECT-DA$ T$=
   2 EC:PATH$ ECT-DC$ T$=
   3 EC:PATH$ ECT-DB$ T$=
   ECT-ENTRY$ EC:LOAD-ORDER
   EC:ORDER-COUNT 4 T=
   0 EC:ORDER-PATH$ ECT-DA$ T$=
   1 EC:ORDER-PATH$ ECT-DC$ T$=
   2 EC:ORDER-PATH$ ECT-DB$ T$=
   3 EC:ORDER-PATH$ ECT-ENTRY$ T$= ;

\ A provided call inside an uncalled definition is possible, not a definite
\ registration; it cannot suppress the entry's later required load.
: ECT-TEST-SPEC-PROVIDED ( -- )
   ECT-DA$ S\" package ACTIONS : MARK ( -- ) s\" dep-b.f\" provided ; ;package\n" ECT-WRITE
   ECT-DB$ s" 76 emit 79 emit 65 emit 68 emit 69 emit 68 emit" ECT-WRITE
   ECT-ENTRY$ s" require dep-a.f require dep-b.f" ECT-WRITE
   s" ECT-ENTRY$ ;package included"
   ECT-OUT ECT-OUT-CAP >LEN ECT-ERR ECT-ERR-CAP >LEN ECT-CHILD-MS >MS
   SUBJECT:RUN {: outu:len erru:len oc :}
   s" ECT-ENTRY$ ;package included"
   ECT-OUT outu LEN>N ECT-ERR erru LEN>N oc 0 T-OUTCOME-EXITED=
   ECT-OUT outu LEN>N s" LOADED" T$=
   ECT-ENTRY$ EC:BUILD
   EC:COUNT 3 T=
   2 EC:PATH$ ECT-DB$ T$= ;

\ A provided in an uncalled inner definition cannot suppress a top-level
\ require in the file loaded by an outer possible call.
: ECT-TEST-NESTED-SPEC-PROVIDED ( -- )
   ECT-DA$ S\" : MARK ( -- ) s\" dep-b.f\" provided ; require dep-b.f\n" ECT-WRITE
   ECT-DB$ s" 76 emit 79 emit 65 emit 68 emit 69 emit 68 emit" ECT-WRITE
   ECT-ENTRY$ S\" : LATER ( -- ) s\" dep-a.f\" required ; LATER\n" ECT-WRITE
   s" ECT-ENTRY$ included"
   ECT-OUT ECT-OUT-CAP >LEN ECT-ERR ECT-ERR-CAP >LEN ECT-CHILD-MS >MS
   SUBJECT:RUN {: outu:len erru:len oc :}
   s" ECT-ENTRY$ included"
   ECT-OUT outu LEN>N ECT-ERR erru LEN>N oc 0 T-OUTCOME-EXITED=
   ECT-OUT outu LEN>N s" LOADED" T$=
   ECT-ENTRY$ EC:BUILD
   EC:COUNT 3 T=
   2 EC:PATH$ ECT-DB$ T$= ;

\ A possible required load still registers its path while that call executes;
\ a transitive require back to the active source is a no-op, not a cycle.
: ECT-TEST-SPEC-REQUIRE-CYCLE ( -- )
   ECT-DA$ s" require dep-b.f" ECT-WRITE
   ECT-DB$ s" require dep-a.f" ECT-WRITE
   ECT-ENTRY$ S\" : LATER ( -- ) s\" dep-a.f\" required ; LATER 67 emit 89 emit 67 emit 76 emit 69 emit\n" ECT-WRITE
   s" ECT-ENTRY$ included"
   ECT-OUT ECT-OUT-CAP >LEN ECT-ERR ECT-ERR-CAP >LEN ECT-CHILD-MS >MS
   SUBJECT:RUN {: outu:len erru:len oc :}
   s" ECT-ENTRY$ included"
   ECT-OUT outu LEN>N ECT-ERR erru LEN>N oc 0 T-OUTCOME-EXITED=
   ECT-OUT outu LEN>N s" CYCLE" T$=
   ECT-ENTRY$ EC:BUILD
   EC:COUNT 3 T=
   ECT-ENTRY$ EC:LOAD-ORDER
   EC:ORDER-COUNT 3 T= ;

\ A body load could run before a later family call. Since the source walk
\ cannot establish whether it runs, it refuses the possible shadow.
: ECT-TEST-SPEC-FAMILY ( -- )
   ECT-DA$ s" : ENUM ( -- ) ;" ECT-WRITE
   ECT-ENTRY$ S\" : MAYBE ( -- ) s\" dep-a.f\" included ; ENUM color red ;ENUM 76 emit 79 emit 65 emit 68 emit 69 emit 68 emit\n" ECT-WRITE
   s" ECT-ENTRY$ included"
   ECT-OUT ECT-OUT-CAP >LEN ECT-ERR ECT-ERR-CAP >LEN ECT-CHILD-MS >MS
   SUBJECT:RUN {: outu:len erru:len oc :}
   s" ECT-ENTRY$ included"
   ECT-OUT outu LEN>N ECT-ERR erru LEN>N oc 0 T-OUTCOME-EXITED=
   ECT-OUT outu LEN>N s" LOADED" T$=
   [: ECT-ENTRY$ EC:BUILD ;] E-DISC-SHADOW TTHROWSQ ;

\ The same included source resolves a relative require under each loader root.
\ BUILD deduplicates its path but must still find both distinct dependencies.
: ECT-TEST-INCLUDE-ROOTS ( -- )
   s" root-a/sub" ECT-PATH ECT-PATH-U ECT-MK ECT-PATH$ MAKE-DIRS
   s" root-b" ECT-PATH ECT-PATH-U ECT-MK ECT-PATH$ MAKE-DIRS
   s" root-a/entry.f" ECT-RA-ENTRY ECT-RA-ENTRY-U ECT-MK
   s" root-b/entry.f" ECT-RB-ENTRY ECT-RB-ENTRY-U ECT-MK
   s" root-a/sub/shared.f" ECT-SHARED ECT-SHARED-U ECT-MK
   ECT-SHARED$ s" require dep.f" ECT-WRITE
   s" root-a/dep.f" s" 65 emit" ECT-WRITE-REL
   s" root-a/sub/dep.f" s" 66 emit" ECT-WRITE-REL
   SB-RESET s" include " SB-APPEND ECT-SHARED$ SB-APPEND ECT-LF
   ECT-RB-ENTRY$ SB$ ECT-WRITE
   SB-RESET s" include sub/shared.f" SB-APPEND ECT-LF
   s" include " SB-APPEND ECT-RB-ENTRY$ SB-APPEND ECT-LF
   ECT-RA-ENTRY$ SB$ ECT-WRITE
   s" ECT-RA-ENTRY$ included"
   ECT-OUT ECT-OUT-CAP >LEN ECT-ERR ECT-ERR-CAP >LEN ECT-CHILD-MS >MS
   SUBJECT:RUN {: outu:len erru:len oc :}
   s" ECT-RA-ENTRY$ included"
   ECT-OUT outu LEN>N ECT-ERR erru LEN>N oc 0 T-OUTCOME-EXITED=
   ECT-OUT outu LEN>N s" AB" T$=
   ECT-RA-ENTRY$ EC:BUILD
   EC:COUNT 5 T=
   s" root-a/sub/dep.f" ECT-PATH ECT-PATH-U ECT-MK
   4 EC:PATH$ ECT-PATH$ T$=
   ECT-RA-ENTRY$ [: ECT-READ-PATH ;] EC:BUILD-WITH
   EC:COUNT 6 T=
   5 EC:PATH$ ECT-PATH$ T$=
   ECT-RA-ENTRY$ EC:LOAD-ORDER
   EC:ORDER-COUNT 5 T=
   1 EC:ORDER-PATH$ ECT-PATH$ T$=
   2 EC:ORDER-PATH$ ECT-SHARED$ T$=
   SB-RESET s" require " SB-APPEND ECT-SHARED$ SB-APPEND ECT-LF
   ECT-RB-ENTRY$ SB$ ECT-WRITE
   SB-RESET s" require sub/shared.f" SB-APPEND ECT-LF
   s" include " SB-APPEND ECT-RB-ENTRY$ SB-APPEND ECT-LF
   ECT-RA-ENTRY$ SB$ ECT-WRITE
   s" ECT-RA-ENTRY$ included"
   ECT-OUT ECT-OUT-CAP >LEN ECT-ERR ECT-ERR-CAP >LEN ECT-CHILD-MS >MS
   SUBJECT:RUN {: rout:len rerr:len rcode :}
   s" ECT-RA-ENTRY$ included"
   ECT-OUT rout LEN>N ECT-ERR rerr LEN>N rcode 0 T-OUTCOME-EXITED=
   ECT-OUT rout LEN>N s" A" T$=
   ECT-RA-ENTRY$ [: ECT-READ-PATH ;] EC:BUILD-WITH
   EC:COUNT 4 T=
   SB-RESET s" require dep.f" SB-APPEND ECT-LF
   s" require " SB-APPEND ECT-RB-ENTRY$ SB-APPEND ECT-LF
   ECT-SHARED$ SB$ ECT-WRITE
   SB-RESET s" require " SB-APPEND ECT-SHARED$ SB-APPEND ECT-LF
   ECT-RB-ENTRY$ SB$ ECT-WRITE
   ECT-RA-ENTRY$ s" include sub/shared.f" ECT-WRITE
   s" ECT-RA-ENTRY$ included"
   ECT-OUT ECT-OUT-CAP >LEN ECT-ERR ECT-ERR-CAP >LEN ECT-CHILD-MS >MS
   SUBJECT:RUN {: aout:len aerr:len acode :}
   s" ECT-RA-ENTRY$ included"
   ECT-OUT aout LEN>N ECT-ERR aerr LEN>N acode 0 T-OUTCOME-EXITED=
   ECT-OUT aout LEN>N s" AB" T$=
   ECT-RA-ENTRY$ EC:BUILD
   EC:COUNT 5 T=
   ECT-RA-ENTRY$ EC:LOAD-ORDER
   EC:ORDER-COUNT 5 T= ;

: ECT-TEST-INCLUDE-REQUIRE-SELF ( -- )
   ECT-DA$ s" require dep-a.f 88 emit" ECT-WRITE
   ECT-ENTRY$ s" include dep-a.f" ECT-WRITE
   s" ECT-ENTRY$ included"
   ECT-OUT ECT-OUT-CAP >LEN ECT-ERR ECT-ERR-CAP >LEN ECT-CHILD-MS >MS
   SUBJECT:RUN {: outu:len erru:len oc :}
   s" ECT-ENTRY$ included"
   ECT-OUT outu LEN>N ECT-ERR erru LEN>N oc 0 T-OUTCOME-EXITED=
   ECT-OUT outu LEN>N s" XX" T$=
   ECT-ENTRY$ EC:BUILD
   EC:COUNT 2 T= ;

\ --- a require to a nonexistent path is refused where it is read --------------
\ A closure without the member would key fewer files than the build reads. A
\ child runs each walk, so its stderr shows what an uncaught refusal tells the
\ user: the code alone does not say which member is missing.
: ECT-CODE$ ( n -- ptr u8 n ) {: code:n :}
   SB-RESET
   s" uncaught throw code " SB-APPEND
   code FMT:SB-INT
   SB$ ;

: ECT-REFUSES-MISSING ( ptr u8 n -- ) {: src:ptr srcu:n :}
   src srcu ECT-OUT ECT-OUT-CAP >LEN ECT-ERR ECT-ERR-CAP >LEN ECT-CHILD-MS >MS
   SUBJECT:RUN {: outu:len erru:len oc :}
   src srcu ECT-OUT outu LEN>N ECT-ERR erru LEN>N oc ECT-UNCAUGHT-RC
   T-OUTCOME-EXITED=
   ECT-ERR erru LEN>N E-FS-STAT ECT-CODE$ CONTAINS? TTRUE
   ECT-ERR erru LEN>N ECT-DC$ CONTAINS? TTRUE ;

: ECT-READ ( ptr u8 n ptr u8 n -- ptr u8 n )
   2drop ECT-READ-SRC $1000 READ-ALL ECT-READ-SRC swap ;

\ A sibling load defines the family opener before use.f reaches it. The
\ child loads dep-c.f, despite use.f having no prior local load event.
: ECT-TEST-FAMILY-PRIOR-SIBLING ( -- )
   ECT-DA$ s" package DEMO : structure ( -- ) ; ;package" ECT-WRITE
   ECT-DB$ S\" package DEMO structure require dep-c.f ;package\n" ECT-WRITE
   ECT-DC$ s" 76 emit 79 emit 65 emit 68 emit 69 emit 68 emit" ECT-WRITE
   ECT-ENTRY$ S\" require dep-a.f require dep-b.f\n" ECT-WRITE
   s" ECT-ENTRY$ ;package included"
   ECT-OUT ECT-OUT-CAP >LEN ECT-ERR ECT-ERR-CAP >LEN ECT-CHILD-MS >MS
   SUBJECT:RUN {: outu:len erru:len oc :}
   s" ECT-ENTRY$ ;package included"
   ECT-OUT outu LEN>N ECT-ERR erru LEN>N oc 0 T-OUTCOME-EXITED=
   ECT-OUT outu LEN>N s" LOADED" T$=
   [: ECT-ENTRY$ EC:BUILD ;] E-DISC-SHADOW TTHROWSQ
   [: ECT-ENTRY$ EC:LOAD-ORDER ;] E-DISC-SHADOW TTHROWSQ
   [: ECT-ENTRY$ [: ECT-READ-PATH ;] EC:BUILD-WITH ;]
      E-DISC-SHADOW TTHROWSQ ;

\ The engine's global family word retains its grammar when its body is only
\ the actual parser runner. Native self-build loads these definitions first.
: ECT-TEST-FAMILY-FORWARDER ( -- )
   ECT-DA$ s" : ENUM ( -- ) ENUM-DECL:ED-RUN ;" ECT-WRITE
   ECT-DB$ s" ENUM color red ;ENUM" ECT-WRITE
   ECT-ENTRY$ s" require dep-a.f require dep-b.f" ECT-WRITE
   ECT-ENTRY$ EC:BUILD
   EC:COUNT 3 T= ;

\ A loaded child inherits the caller's package. Its bare family opener can
\ therefore resolve to a word a prior sibling defined in that package.
: ECT-TEST-FAMILY-INHERITED-PACKAGE ( -- )
   ECT-DA$ s" package DEMO : structure ( -- ) ; ;package" ECT-WRITE
   ECT-DB$ s" structure require dep-c.f" ECT-WRITE
   ECT-DC$ s" 76 emit 79 emit 65 emit 68 emit 69 emit 68 emit" ECT-WRITE
   ECT-ENTRY$ S\" require dep-a.f package DEMO require dep-b.f ;package\n" ECT-WRITE
   s" ECT-ENTRY$ ;package included"
   ECT-OUT ECT-OUT-CAP >LEN ECT-ERR ECT-ERR-CAP >LEN ECT-CHILD-MS >MS
   SUBJECT:RUN {: outu:len erru:len oc :}
   s" ECT-ENTRY$ ;package included"
   ECT-OUT outu LEN>N ECT-ERR erru LEN>N oc 0 T-OUTCOME-EXITED=
   ECT-OUT outu LEN>N s" LOADED" T$=
   [: ECT-ENTRY$ EC:BUILD ;] E-DISC-SHADOW TTHROWSQ ;

\ A prior provided path is known without being loaded. A later require of it
\ cannot contribute that file's definitions to the family context.
: ECT-TEST-FAMILY-PROVIDED-SIBLING ( -- )
   SB-RESET s" dep-b.f" ECT-PROVIDED-LINE ECT-DA$ SB$ ECT-WRITE
   ECT-DB$ s" package DEMO : structure ( -- ) ; ;package" ECT-WRITE
   ECT-DC$ S\" require dep-b.f package DEMO STRUCTURE box FIELD x n ;STRUCTURE ;package require unrelated.f\n" ECT-WRITE
   ECT-UN$ s" 76 emit 79 emit 65 emit 68 emit 69 emit 68 emit" ECT-WRITE
   ECT-ENTRY$ S\" require dep-a.f require dep-c.f\n" ECT-WRITE
   s" ECT-ENTRY$ ;package included"
   ECT-OUT ECT-OUT-CAP >LEN ECT-ERR ECT-ERR-CAP >LEN ECT-CHILD-MS >MS
   SUBJECT:RUN {: outu:len erru:len oc :}
   s" ECT-ENTRY$ ;package included"
   ECT-OUT outu LEN>N ECT-ERR erru LEN>N oc 0 T-OUTCOME-EXITED=
   ECT-OUT outu LEN>N s" LOADED" T$=
   ECT-ENTRY$ EC:BUILD
   EC:COUNT 4 T=
   0 EC:PATH$ ECT-ENTRY$ T$=
   1 EC:PATH$ ECT-DA$ T$=
   2 EC:PATH$ ECT-DC$ T$=
   3 EC:PATH$ ECT-UN$ T$= ;

\ `included` does not register its path as required. The later `require`
\ replays that file under the intervening definition, so the family opener is
\ shadowed on its second visit.
: ECT-TEST-FAMILY-INCLUDE-REQUIRE ( -- )
   ECT-DA$ S\" package DEMO ENUM choice red ;ENUM ;package\n76 emit 79 emit 65 emit 68 emit 69 emit 68 emit\n" ECT-WRITE
   ECT-DB$ s" package DEMO : enum ( -- ) ; : choice ( -- ) ; : red ( -- ) ; : ;ENUM ( -- ) ; ;package" ECT-WRITE
   ECT-DC$ s" 76 emit 79 emit 65 emit 68 emit 69 emit 68 emit" ECT-WRITE
   ECT-ENTRY$ S\" include dep-a.f require dep-b.f require dep-a.f\n" ECT-WRITE
   s" ECT-ENTRY$ ;package included"
   ECT-OUT ECT-OUT-CAP >LEN ECT-ERR ECT-ERR-CAP >LEN ECT-CHILD-MS >MS
   SUBJECT:RUN {: outu:len erru:len oc :}
   s" ECT-ENTRY$ ;package included"
   ECT-OUT outu LEN>N ECT-ERR erru LEN>N oc 0 T-OUTCOME-EXITED=
   ECT-OUT outu LEN>N s" LOADEDLOADED" T$=
   [: ECT-ENTRY$ EC:BUILD ;] E-DISC-SHADOW TTHROWSQ ;

\ One file can be included in two packages. Both definitions remain visible
\ under their respective packages after the second visit.
: ECT-TEST-FAMILY-MULTI-OWNER ( -- )
   ECT-DA$ s" : structure ( -- ) ;" ECT-WRITE
   ECT-DB$ s" 76 emit 79 emit 65 emit 68 emit 69 emit 68 emit" ECT-WRITE
   ECT-ENTRY$ S\" package P include dep-a.f ;package package Q include dep-a.f ;package package P structure require dep-b.f ;package\n" ECT-WRITE
   s" ECT-ENTRY$ ;package included"
   ECT-OUT ECT-OUT-CAP >LEN ECT-ERR ECT-ERR-CAP >LEN ECT-CHILD-MS >MS
   SUBJECT:RUN {: outu:len erru:len oc :}
   s" ECT-ENTRY$ ;package included"
   ECT-OUT outu LEN>N ECT-ERR erru LEN>N oc 0 T-OUTCOME-EXITED=
   ECT-OUT outu LEN>N s" LOADED" T$=
   [: ECT-ENTRY$ EC:BUILD ;] E-DISC-SHADOW TTHROWSQ
   [: ECT-ENTRY$ EC:LOAD-ORDER ;] E-DISC-SHADOW TTHROWSQ
   [: ECT-ENTRY$ [: ECT-READ-PATH ;] EC:BUILD-WITH ;]
      E-DISC-SHADOW TTHROWSQ ;

\ A transitive dependency can replace the ordinary STRUCTURE opener before the
\ entry reaches it. The resulting require runs and belongs in the closure.
: ECT-TEST-FAMILY-DEPENDENCY ( -- )
   ECT-DA$ s" require dep-c.f" ECT-WRITE
   ECT-DC$ s" : structure ( -- ) ;" ECT-WRITE
   ECT-DB$ s" 76 emit 79 emit 65 emit 68 emit 69 emit 68 emit" ECT-WRITE
   ECT-ENTRY$ S\" package DEMO require dep-a.f\nstructure require dep-b.f ;package\n" ECT-WRITE
   s" ECT-ENTRY$ ;package included"
   ECT-OUT ECT-OUT-CAP >LEN ECT-ERR ECT-ERR-CAP >LEN ECT-CHILD-MS >MS
   SUBJECT:RUN {: outu:len erru:len oc :}
   s" ECT-ENTRY$ ;package included"
   ECT-OUT outu LEN>N ECT-ERR erru LEN>N oc 0 T-OUTCOME-EXITED=
   ECT-OUT outu LEN>N s" LOADED" T$=
   [: ECT-ENTRY$ EC:BUILD ;] E-DISC-SHADOW TTHROWSQ
   DISCOVER:LAST-TOKEN {: off:n len:n :}
   off 29 T=
   len 9 T=
   [: ECT-ENTRY$ EC:LOAD-ORDER ;] E-DISC-SHADOW TTHROWSQ
   [: ECT-ENTRY$ [: ECT-READ ;] EC:BUILD-WITH ;]
   E-DISC-SHADOW TTHROWSQ ;

: ECT-TEST-MISSING ( -- )
   ECT-DC$ FILE? if ECT-DC$ REMOVE-FILE then
   ECT-DA$ s\" \\ a\n" ECT-WRITE
   SB-RESET ECT-DA$ ECT-REQ-LINE ECT-DC$ ECT-REQ-LINE ECT-ENTRY$ SB$ ECT-WRITE
   s" ECT-ENTRY$ EC:BUILD" ECT-REFUSES-MISSING
   s" ECT-ENTRY$ EC:LOAD-ORDER" ECT-REFUSES-MISSING ;

\ --- key sensitivity: the property hb-build relies on -------------------------
: ECT-TEST-KEY-CLOSURE-EDIT ( -- )
   ECT-DA$ s\" \\ a v1\n" ECT-WRITE
   SB-RESET ECT-DA$ ECT-REQ-LINE ECT-ENTRY$ SB$ ECT-WRITE
   ECT-KEY-A ECT-CLOSURE-KEY
   ECT-DA$ s\" \\ a v2 changed\n" APPEND-FILE
   ECT-KEY-B ECT-CLOSURE-KEY
   ECT-KEY-A 64 ECT-KEY-B 64 STR= TFALSE ;

: ECT-TEST-KEY-STABLE ( -- )
   ECT-DA$ s\" \\ a stable\n" ECT-WRITE
   SB-RESET ECT-DA$ ECT-REQ-LINE ECT-ENTRY$ SB$ ECT-WRITE
   ECT-KEY-A ECT-CLOSURE-KEY
   ECT-KEY-B ECT-CLOSURE-KEY
   ECT-KEY-A 64 ECT-KEY-B 64 STR= TTRUE ;

: ECT-TEST-KEY-ALIAS ( -- )
   ECT-ENTRY$ ECT-ALIAS ECT-ALIAS-U @ MAKE-SYMLINK
   ECT-KEY-A ECT-CLOSURE-KEY
   ECT-ALIAS ECT-ALIAS-U @ ECT-KEY-B ECT-KEY-FOR
   ECT-KEY-A 64 ECT-KEY-B 64 STR= TTRUE ;

: ECT-TEST-KEY-NONCLOSURE-EDIT ( -- )
   ECT-DA$ s\" \\ a base\n" ECT-WRITE
   ECT-UN$ s\" \\ unrelated v1\n" ECT-WRITE
   SB-RESET ECT-DA$ ECT-REQ-LINE ECT-ENTRY$ SB$ ECT-WRITE
   ECT-KEY-A ECT-CLOSURE-KEY
   ECT-UN$ s\" \\ unrelated v2\n" APPEND-FILE
   ECT-KEY-B ECT-CLOSURE-KEY
   ECT-KEY-A 64 ECT-KEY-B 64 STR= TTRUE ;

\ --- colon-wrapped required joins the closure (whole-file producer) -----------
: ECT-GUARDED-ENTRY$ ( -- ptr u8 n )
   SB-RESET
   s" : ECT-LOAD ( -- ) " SB-APPEND
   $73 SB-APPEND-C ECT-DQ ECT-SP
   ECT-DA$ SB-APPEND
   ECT-DQ ECT-SP
   s" required ;" SB-APPEND
   ECT-LF
   s" ECT-LOAD" SB-APPEND
   ECT-LF
   SB$ ;

: ECT-TEST-COLON-WRAPPED ( -- )
   ECT-DA$ s\" \\ a guarded\n" ECT-WRITE
   ECT-ENTRY$ ECT-GUARDED-ENTRY$ ECT-WRITE
   ECT-ENTRY$ EC:BUILD
   EC:COUNT 2 T=
   0 EC:PATH$ ECT-ENTRY$ T$=
   1 EC:PATH$ ECT-DA$ T$= ;

\ --- key sensitivity for a dep reachable only through a colon body ------------
: ECT-TEST-KEY-COLON-DEP-EDIT ( -- )
   ECT-DA$ s\" \\ a guarded v1\n" ECT-WRITE
   ECT-ENTRY$ ECT-GUARDED-ENTRY$ ECT-WRITE
   ECT-KEY-A ECT-CLOSURE-KEY
   ECT-DA$ s\" \\ a guarded v2\n" APPEND-FILE
   ECT-KEY-B ECT-CLOSURE-KEY
   ECT-KEY-A 64 ECT-KEY-B 64 STR= TFALSE ;

: ECT-MAIN ( -- )
   T-RESET
   ECT-PREP
   ECT-TEST-ORDER
   ECT-TEST-TRANSITIVE
   ECT-TEST-TRANSIENT-ENTRY
   ECT-TEST-SELECTED-ROOT
   ECT-TEST-BEFORE-ERROR
   ECT-TEST-LONG-ENTRY
   ECT-TEST-FAMILY-FIELD
   ECT-TEST-FAMILY-LATE-DEPENDENCY
   ECT-TEST-FAMILY-PRIOR-SIBLING
   ECT-TEST-FAMILY-FORWARDER
   ECT-TEST-FAMILY-INHERITED-PACKAGE
   ECT-TEST-FAMILY-PROVIDED-SIBLING
   ECT-TEST-FAMILY-INCLUDE-REQUIRE
   ECT-TEST-FAMILY-MULTI-OWNER
   ECT-TEST-FAMILY-DEPENDENCY
   ECT-TEST-PROVIDED
   ECT-TEST-PROVIDED-INCLUDE-ORDER
   ECT-TEST-SPEC-PROVIDED
   ECT-TEST-NESTED-SPEC-PROVIDED
   ECT-TEST-SPEC-REQUIRE-CYCLE
   ECT-TEST-SPEC-FAMILY
   ECT-TEST-INCLUDE-ROOTS
   ECT-TEST-INCLUDE-REQUIRE-SELF
   ECT-TEST-MISSING
   ECT-TEST-KEY-CLOSURE-EDIT
   ECT-TEST-KEY-STABLE
   ECT-TEST-KEY-ALIAS
   ECT-TEST-KEY-NONCLOSURE-EDIT
   ECT-TEST-COLON-WRAPPED
   ECT-TEST-KEY-COLON-DEP-EDIT
   EC:RESET
   CLEANUP-RUN
   T-REPORT
   s" event-closure-test: ok" type cr ;

ECT-MAIN

;package
