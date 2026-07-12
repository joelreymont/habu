\ event-closure-test.f - checked fixtures for tools/event-closure-lib.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\ lib/fs-mutate.f lib/source.f lib/content-key.f tools/source-discovery.f
\ tools/event-closure-lib.f tools/event-closure-test.f
\
\ Proves the ordered transitive closure list (require/included followed, provided
\ and require-known not, exact-string dedup, missing files skipped, transitive
\ descent) and the property the hb-build cache key relies on: a content edit to
\ any closure file changes the closure key, while an untouched closure or an edit
\ to a non-closure file leaves it stable.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/source.f
require lib/content-key.f
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
create ECT-KEY-A 80 allot
create ECT-KEY-B 80 allot
variable ECT-ROOT-U
variable ECT-ENTRY-U
variable ECT-DA-U
variable ECT-DB-U
variable ECT-DC-U
variable ECT-UN-U
variable ECT-I

: ECT-ROOT$ ( -- ptr u8 n )   ECT-ROOT ECT-ROOT-U @ ;
: ECT-ENTRY$ ( -- ptr u8 n )  ECT-ENTRY ECT-ENTRY-U @ ;
: ECT-DA$ ( -- ptr u8 n )     ECT-DA ECT-DA-U @ ;
: ECT-DB$ ( -- ptr u8 n )     ECT-DB ECT-DB-U @ ;
: ECT-DC$ ( -- ptr u8 n )     ECT-DC ECT-DC-U @ ;
: ECT-UN$ ( -- ptr u8 n )     ECT-UN ECT-UN-U @ ;

: ECT-MK ( ptr u8 n ptr u8 ptr a -- ) {: na:ptr nu:n dst:ptr up:ptr :}
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

: ECT-PREP ( -- )
   CLEANUP-RESET
   s" habu-event-closure-test" TMPDIR-MKDIR {: a:ptr u:n :}
   a ECT-ROOT u BYTE-COPY  u ECT-ROOT-U !
   ECT-ROOT$ CLEANUP-TREE+
   s" entry.f" ECT-ENTRY ECT-ENTRY-U ECT-MK
   s" dep-a.f" ECT-DA ECT-DA-U ECT-MK
   s" dep-b.f" ECT-DB ECT-DB-U ECT-MK
   s" dep-c.f" ECT-DC ECT-DC-U ECT-MK
   s" unrelated.f" ECT-UN ECT-UN-U ECT-MK ;

: ECT-CLOSURE-KEY ( ptr u8 -- ) {: dst:ptr :}
   CK-RESET
   s" ect-closure-v1" CK-TEXT+
   ECT-ENTRY$ EC:BUILD
   0 ECT-I !
   begin ECT-I @ EC:COUNT < while
      ECT-I @ EC:PATH$ CK-FILE+
      ECT-I @ 1+ ECT-I !
   repeat
   dst CK-FINAL-HEX ;

\ --- ordering / dedup ---------------------------------------------------------
: ECT-MIXED-ENTRY$ ( -- ptr u8 n )
   SB-RESET
   ECT-DA$ ECT-REQ-LINE
   ECT-DB$ ECT-REQ-LINE
   ECT-DA$ ECT-REQ-LINE
   SB$ ;

: ECT-TEST-ORDER ( -- )
   ECT-DA$ s\" \\ a\n" ECT-WRITE
   ECT-DB$ s\" \\ b\n" ECT-WRITE
   ECT-ENTRY$ ECT-MIXED-ENTRY$ ECT-WRITE
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

\ --- provided registers but does not load -------------------------------------
: ECT-TEST-PROVIDED ( -- )
   ECT-DA$ s\" \\ a\n" ECT-WRITE
   SB-RESET ECT-DA$ ECT-PROVIDED-LINE ECT-ENTRY$ SB$ ECT-WRITE
   ECT-ENTRY$ EC:BUILD
   EC:COUNT 1 T=
   0 EC:PATH$ ECT-ENTRY$ T$= ;

\ --- a require to a nonexistent path is skipped -------------------------------
: ECT-TEST-MISSING ( -- )
   ECT-DC$ FILE? if ECT-DC$ REMOVE-FILE then
   SB-RESET ECT-DC$ ECT-REQ-LINE ECT-ENTRY$ SB$ ECT-WRITE
   ECT-ENTRY$ EC:BUILD
   EC:COUNT 1 T=
   0 EC:PATH$ ECT-ENTRY$ T$= ;

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
   ECT-TEST-PROVIDED
   ECT-TEST-MISSING
   ECT-TEST-KEY-CLOSURE-EDIT
   ECT-TEST-KEY-STABLE
   ECT-TEST-KEY-NONCLOSURE-EDIT
   ECT-TEST-COLON-WRAPPED
   ECT-TEST-KEY-COLON-DEP-EDIT
   EC:RESET
   CLEANUP-RUN
   T-REPORT
   s" event-closure-test: ok" type cr ;

ECT-MAIN

;package
