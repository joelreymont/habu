\ diff-frame.f - validated framed unified-diff artifact reader (Option B).
\
\ The framed artifact is a self-describing byte container for a set of
\ per-file diff sections: a header (magic, version, from/to commit ids), a
\ run of sections (status, form, body/mode flags, old/new paths, raw body),
\ and a trailer (section count, SHA-256 digest over everything before it).
\
\ Section validation is a CLIENT of the shared unified-diff event parser in
\ tools/lint/diff.f: each section's raw body is replayed through DIFF:LINE,
\ and the frame form is derived from the parser's own classification. There
\ is exactly one unified-diff parser in the tree; this file adds no second
\ validator.

require src/core/sha256.f
require lib/prelude.f
require lib/string.f
require lib/memory.f
require tools/lint/diff.f
require tools/lint/diff-path.f
require tools/lint/diff-error.f

package DIFF-FRAME
public

\ status: how the file changed (producer-declared, cross-checked here).
ENUM status modified added removed renamed copied ;ENUM

\ form: the shape of the section's content, derived from the parser.
ENUM form text mode empty pure binary ;ENUM

\ event: the reader's per-line stream over a section's raw body.
ENUM event none file hunk add context delete ;ENUM

\ Clean constructors for the producer-declared status (the generated variant
\ constructors carry an escaped package prefix; these read better at call sites).
: STATUS-MODIFIED ( -- status ) construct status modified ;
: STATUS-ADDED    ( -- status ) construct status added ;
: STATUS-REMOVED  ( -- status ) construct status removed ;
: STATUS-RENAMED  ( -- status ) construct status renamed ;
: STATUS-COPIED   ( -- status ) construct status copied ;

private

40 constant COMMIT-ID-U
64 constant COMMIT-ID-U2
$30 constant ZERO-C
$39 constant NINE-C
$61 constant LC-A-C
$66 constant LC-F-C
$0A constant LF-C

\ ---- commit id validation ------------------------------------------------

: HEX-C? ( n -- bool ) {: c:n :}
   c ZERO-C >= c NINE-C <= and if true exit then
   c LC-A-C >= c LC-F-C <= and ;

: HEX$? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0= if false exit then
   0 begin dup u < while
      dup a + c@ HEX-C? 0= if drop false exit then
      1+
   repeat drop true ;

public

: COMMIT-ID? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u COMMIT-ID-U = u COMMIT-ID-U2 = or 0= if false exit then
   a u HEX$? ;

private

\ ---- section scan: replay a raw body through the shared parser -----------

variable SC-OFF          \ cursor into the raw body
variable SC-SAW          \ count of parser section events (the body must carry one)
variable SC-PFORM        \ parser form captured at the section event (see PF#)
variable SC-BODY         \ parser body flag captured at the section event
variable SC-MODE         \ an "old mode" meta line was accepted
variable SC-OLD-P        \ parser section: old side present (nonzero)
variable SC-NEW-P        \ parser section: new side present (nonzero)
PTR-VARIABLE SC-OLD-A  variable SC-OLD-U   \ parser section: old path span
PTR-VARIABLE SC-NEW-A  variable SC-NEW-U   \ parser section: new path span

: PF# ( DIFF:form -- n )
   MATCH DIFF:form
      modify      OF 0 ENDOF
      add-file    OF 1 ENDOF
      delete-file OF 2 ENDOF
      rename      OF 3 ENDOF
      copy        OF 4 ENDOF
      mode        OF 5 ENDOF
      binary      OF 6 ENDOF
   ;MATCH ;

0 constant PF-MODIFY
1 constant PF-ADD
2 constant PF-DELETE
3 constant PF-RENAME
4 constant PF-COPY
5 constant PF-MODE
6 constant PF-BINARY

: SECTION-EVENT? ( DIFF:event -- bool )
   MATCH DIFF:event
      none    OF false ENDOF
      section OF true  ENDOF
      hunk    OF false ENDOF
      add     OF false ENDOF
      context OF false ENDOF
      delete  OF false ENDOF
   ;MATCH ;

\ Snapshot the parser section's form, body flag, and the old/new side identity
\ (presence plus the exact path bytes it extracted), and count the event. The
\ spans borrow the raw body, which stays live for the whole validation.
: CAPTURE-SECTION ( -- )
   DIFF:SECTION-FORM PF# SC-PFORM !
   DIFF:SECTION-BODY? if 1 else 0 then SC-BODY !
   DIFF:SECTION-OLD$ SC-OLD-U ! SC-OLD-A !
   DIFF:SECTION-NEW$ SC-NEW-U ! SC-NEW-A !
   SC-OLD-U @ 0<> if 1 else 0 then SC-OLD-P !
   SC-NEW-U @ 0<> if 1 else 0 then SC-NEW-P !
   SC-SAW @ 1+ SC-SAW ! ;

: MODE-LINE? ( ptr u8 n -- bool )
   s" old mode " STARTS-WITH? ;

: SC-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u MODE-LINE? if 1 SC-MODE ! then
   a u DIFF:LINE SECTION-EVENT? if CAPTURE-SECTION then ;

: SC-FEED-LINES ( ptr u8 n -- ) {: a:ptr u:n :}
   0 SC-OFF !
   0 begin dup u < while
      dup a + c@ LF-C = if
         a SC-OFF @ + over SC-OFF @ - SC-LINE
         dup 1+ SC-OFF !
      then
      1+
   repeat drop ;

: SC-FINISH ( -- )
   DIFF:FINISH SECTION-EVENT? if CAPTURE-SECTION then ;

\ Replay one section's raw body through the shared parser. The body must be a
\ single well-formed file diff: zero or duplicate parser section events (e.g. a
\ raw stream that packs several files under one declared section) are rejected.
: SCAN-RAW ( ptr u8 n -- ) {: a:ptr u:n :}
   DIFF:RESET
   0 SC-SAW ! 0 SC-PFORM ! 0 SC-BODY ! 0 SC-MODE !
   0 SC-OLD-P ! 0 SC-NEW-P ! 0 SC-OLD-U ! 0 SC-NEW-U !
   a u DIFF:SOURCE-VALIDATE
   a u SC-FEED-LINES
   SC-FINISH
   SC-SAW @ 1 <> if E-DIFF-FRAME-SECTIONS throw then ;

\ ---- frame-form mapping table --------------------------------------------
\
\ The frame form is a function of the parser's form, whether the parser saw a
\ changed text hunk (body), and whether the section carried an old/new mode
\ change. The correspondence is spelled out one arm per parser form so that a
\ future parser form is unmapped and MAP-FORM throws, never silently
\ mis-classified:
\
\   parser modify      , body        -> text
\   parser add-file     , body        -> text     (new file with content)
\   parser add-file     , no body     -> empty    (empty blob add)
\   parser delete-file  , body        -> text
\   parser delete-file  , no body     -> empty
\   parser rename/copy  , body        -> text
\   parser rename/copy  , mode change -> mode
\   parser rename/copy  , neither     -> pure
\   parser mode                        -> mode
\   parser binary                      -> binary

: MAP-ADD-DELETE ( bool -- form )
   if construct form text else construct form empty then ;

: MAP-RENAME-COPY ( bool bool -- form ) {: body:bool mode:bool :}
   body if construct form text exit then
   mode if construct form mode exit then
   construct form pure ;

: MAP-FORM ( n bool bool -- form ) {: pf:n body:bool mode:bool :}
   pf PF-BINARY = if construct form binary exit then
   pf PF-MODE   = if construct form mode exit then
   pf PF-MODIFY = if body 0= if E-DIFF-SYNTAX throw then construct form text exit then
   pf PF-ADD    = if body MAP-ADD-DELETE exit then
   pf PF-DELETE = if body MAP-ADD-DELETE exit then
   pf PF-RENAME = if body mode MAP-RENAME-COPY exit then
   pf PF-COPY   = if body mode MAP-RENAME-COPY exit then
   E-DIFF-SYNTAX throw ;

\ frame body flag: the section carries diffable content (text hunk or binary).
: MAP-BODY ( n bool -- bool ) {: pf:n body:bool :}
   pf PF-BINARY = if true exit then
   body ;

\ ---- status / form family cross-check ------------------------------------
\
\ The producer-declared status must agree with the parser's form family, so a
\ mislabelled section (e.g. an "added" status over a rename body) is rejected.

: STATUS-OK? ( status n -- bool ) {: change:status pf:n :}
   change MATCH status
      modified OF pf PF-MODIFY = pf PF-MODE = or pf PF-BINARY = or ENDOF
      added    OF pf PF-ADD = ENDOF
      removed  OF pf PF-DELETE = ENDOF
      renamed  OF pf PF-RENAME = ENDOF
      copied   OF pf PF-COPY = ENDOF
   ;MATCH ;

\ ---- declared identity binding --------------------------------------------
\
\ Each declared side must match the single parser section's own side: presence
\ agrees, and when present the path bytes are byte-for-byte identical. The
\ parser is the only authority for a section's paths, so a declared path can
\ hold exactly the bytes the parser can extract from the raw body.

: SIDE-MATCH? ( bool ptr u8 n n ptr u8 n -- bool )
   {: decl?:bool da:ptr du:n scan?:n pa:ptr pu:n :}
   decl? if 1 else 0 then  scan? 0= if 0 else 1 then  <> if false exit then
   decl? if da du pa pu STR= exit then
   true ;

\ The declared status also fixes which sides a section must carry: an added file
\ has no old side, a removed file has no new side, everything else has both.

: PRESENCE-OK? ( status bool bool -- bool ) {: change:status old?:bool new?:bool :}
   change MATCH status
      modified OF old? if new? else false then ENDOF
      added    OF old? if false else new? then ENDOF
      removed  OF new? if false else old? then ENDOF
      renamed  OF old? if new? else false then ENDOF
      copied   OF old? if new? else false then ENDOF
   ;MATCH ;

public

\ VALIDATE-SECTION replays the raw body through the shared parser, binds the
\ declared status, side presence, and side path bytes to that one parser
\ section, and returns the derived (form, body, mode).
: VALIDATE-SECTION ( status bool ptr u8 n bool ptr u8 n ptr u8 n -- form bool bool )
   {: change:status old?:bool oa:ptr ou:n new?:bool na:ptr nu:n raw:ptr rawu:n :}
   old? oa ou DIFF-PATH:VALIDATE-SIDE
   new? na nu DIFF-PATH:VALIDATE-SIDE
   raw rawu SCAN-RAW
   old? oa ou SC-OLD-P @ SC-OLD-A @ SC-OLD-U @ SIDE-MATCH? 0= if E-DIFF-FRAME-IDENTITY throw then
   new? na nu SC-NEW-P @ SC-NEW-A @ SC-NEW-U @ SIDE-MATCH? 0= if E-DIFF-FRAME-IDENTITY throw then
   change SC-PFORM @ STATUS-OK? 0= if E-DIFF-FRAME-STATUS throw then
   change old? new? PRESENCE-OK? 0= if E-DIFF-FRAME-STATUS throw then
   SC-PFORM @ SC-BODY @ 0<> SC-MODE @ 0<> MAP-FORM
   SC-PFORM @ SC-BODY @ 0<> MAP-BODY
   SC-MODE @ 0<> ;

\ ==========================================================================
\ Reader: validate and iterate a framed artifact.
\ ==========================================================================

private

$20 constant DIGEST-U
$53 constant SECTION-C
$54 constant TRAILER-C
1 constant FRAME-VERSION
7 constant RESERVED-U
8 constant MAGIC-LEN
75 constant MIN-ART-U

PTR-VARIABLE ART-A       \ owned copy of the artifact bytes
variable ART-U
variable ART-CAP
PTR-VARIABLE VIEW-A      \ detach buffer for content/path spans
variable VIEW-CAP
PTR-VARIABLE FILE-VIEW-A \ separate detach buffer for the file-path span
variable FILE-VIEW-CAP
variable CUR
variable LIMIT
variable DATA-START
variable FRAME-N
variable OPENED
variable ITER-STATE      \ 0 idle, 1 feeding a section body, 2 finishing it
variable ITER-N
variable SEC-IDX
variable RAW-OFF
variable STEP-CODE
variable U64-I
variable U64-V
PTR-VARIABLE FROM-A  variable FROM-U
PTR-VARIABLE TO-A    variable TO-U

1 LAYOUT-BUFFER SEC-STATUS status
1 LAYOUT-BUFFER SEC-FORM form
variable SEC-BODY
variable SEC-MODE
variable SEC-OLD-PRESENT
variable SEC-NEW-PRESENT
PTR-VARIABLE SEC-OLD-A  variable SEC-OLD-U
PTR-VARIABLE SEC-NEW-A  variable SEC-NEW-U
PTR-VARIABLE SEC-RAW-A  variable SEC-RAW-U

PTR-VARIABLE EV-A  variable EV-U  variable EV-META
1 LAYOUT-BUFFER EV-KIND event

create GOT-DIGEST DIGEST-U allot

: ART-A@ ( -- ptr u8 ) ART-A @ ;
: SEC-OLD-A@ ( -- ptr u8 ) SEC-OLD-A @ ;
: SEC-NEW-A@ ( -- ptr u8 ) SEC-NEW-A @ ;
: SEC-RAW-A@ ( -- ptr u8 ) SEC-RAW-A @ ;

: SEC-STATUS-AT ( -- ptr status ) 0 SEC-STATUS ;
: SEC-FORM-AT   ( -- ptr form )   0 SEC-FORM ;
: EV-KIND-AT    ( -- ptr event )  0 EV-KIND ;

: SEC-OLD$ ( -- ptr u8 n ) SEC-OLD-A@ SEC-OLD-U @ ;
: SEC-NEW$ ( -- ptr u8 n ) SEC-NEW-A@ SEC-NEW-U @ ;
: SEC-RAW$ ( -- ptr u8 n ) SEC-RAW-A@ SEC-RAW-U @ ;
: SEC-OLD? ( -- bool ) SEC-OLD-PRESENT @ 0<> ;
: SEC-NEW? ( -- bool ) SEC-NEW-PRESENT @ 0<> ;

\ ---- primitive byte readers over the owned artifact ----------------------

: ROOM? ( n -- bool ) {: need:n :}
   need 0 >= CUR @ need + CUR @ >= and
   CUR @ need + LIMIT @ <= and ;

: R-NEED ( n -- )
   ROOM? 0= if E-DIFF-SYNTAX throw then ;

: ART-C@ ( n -- n ) {: off:n :}
   ART-A@ off + c@ ;

: RU8 ( -- n )
   1 R-NEED
   CUR @ ART-C@
   CUR @ 1+ CUR ! ;

: RU64 ( -- n )
   8 R-NEED
   CUR @ 7 + ART-C@ $80 >= if E-DIFF-SYNTAX throw then
   0 U64-I ! 0 U64-V !
   begin U64-I @ 8 < while
      CUR @ U64-I @ + ART-C@ U64-I @ 8 * lshift U64-V @ or U64-V !
      U64-I @ 1+ U64-I !
   repeat
   CUR @ 8 + CUR !
   U64-V @ ;

: RSPAN ( n -- ptr u8 n ) {: u:n :}
   u R-NEED
   ART-A@ CUR @ + u
   CUR @ u + CUR ! ;

: RBOOL ( -- bool )
   RU8 dup 0= if drop false exit then
   1 = if true exit then
   E-DIFF-SYNTAX throw ;

: BYTE>STATUS ( n -- status )
   case
      0 of construct status modified endof
      1 of construct status added endof
      2 of construct status removed endof
      3 of construct status renamed endof
      4 of construct status copied endof
      E-DIFF-SYNTAX throw
   endcase ;

: BYTE>FORM ( n -- form )
   case
      0 of construct form text endof
      1 of construct form mode endof
      2 of construct form empty endof
      3 of construct form pure endof
      4 of construct form binary endof
      E-DIFF-SYNTAX throw
   endcase ;

: FORM>BYTE ( form -- n )
   MATCH form
      text   OF 0 ENDOF
      mode   OF 1 ENDOF
      empty  OF 2 ENDOF
      pure   OF 3 ENDOF
      binary OF 4 ENDOF
   ;MATCH ;

: BOOL>N ( bool -- n ) if 1 else 0 then ;

\ ---- one-time validation walk --------------------------------------------

: READ-SECTION ( -- )
   RU8 SECTION-C <> if E-DIFF-SYNTAX throw then
   RU8 BYTE>STATUS SEC-STATUS-AT !
   RU8 BYTE>FORM SEC-FORM-AT !
   RBOOL SEC-BODY !
   RBOOL SEC-MODE !
   RBOOL SEC-OLD-PRESENT !
   RBOOL SEC-NEW-PRESENT !
   RU8 0<> if E-DIFF-SYNTAX throw then
   RU64 RSPAN SEC-OLD-U ! SEC-OLD-A !
   RU64 RSPAN SEC-NEW-U ! SEC-NEW-A !
   RU64 RSPAN SEC-RAW-U ! SEC-RAW-A ! ;

\ Re-derive the section's form/body/mode through the shared parser and confirm
\ the bytes stored in the section header agree with the raw body.
: CHECK-SECTION ( -- )
   SEC-OLD? SEC-OLD$ DIFF-PATH:VALIDATE-SIDE
   SEC-NEW? SEC-NEW$ DIFF-PATH:VALIDATE-SIDE
   SEC-STATUS-AT @ SEC-OLD? SEC-OLD$ SEC-NEW? SEC-NEW$ SEC-RAW$
   VALIDATE-SECTION {: got:form body:bool mode:bool :}
   got FORM>BYTE SEC-FORM-AT @ FORM>BYTE <> if E-DIFF-SYNTAX throw then
   body BOOL>N SEC-BODY @ 0<> BOOL>N <> if E-DIFF-SYNTAX throw then
   mode BOOL>N SEC-MODE @ 0<> BOOL>N <> if E-DIFF-SYNTAX throw then ;

: MAGIC? ( -- bool )
   8 R-NEED
   CUR @ s" HABUDIF2" {: a:ptr u:n :}
   0 begin dup u < while
      dup a + c@ over CUR @ + ART-C@ <> if 2drop false exit then
      1+
   repeat 2drop true ;

: HEADER ( -- )
   MAGIC? 0= if E-DIFF-SYNTAX throw then
   CUR @ MAGIC-LEN + CUR !
   RU8 FRAME-VERSION <> if E-DIFF-SYNTAX throw then
   0 begin dup RESERVED-U < while
      RU8 0<> if E-DIFF-SYNTAX throw then
      1+
   repeat drop
   RU64 dup 0= if E-DIFF-SYNTAX throw then RSPAN
   2dup COMMIT-ID? 0= if 2drop E-DIFF-SYNTAX throw then
   FROM-U ! FROM-A !
   RU64 dup 0= if E-DIFF-SYNTAX throw then RSPAN
   2dup COMMIT-ID? 0= if 2drop E-DIFF-SYNTAX throw then
   TO-U ! TO-A !
   CUR @ DATA-START ! ;

: DIGEST-VALID? ( -- bool )
   ART-U @ DIGEST-U < if false exit then
   ART-A@ ART-U @ DIGEST-U - GOT-DIGEST SHA256
   GOT-DIGEST DIGEST-U ART-A@ ART-U @ DIGEST-U - + DIGEST-U STR= ;

: TRAILER ( -- )
   RU8 TRAILER-C <> if E-DIFF-SYNTAX throw then
   RU64 FRAME-N @ <> if E-DIFF-SYNTAX throw then
   CUR @ LIMIT @ <> if E-DIFF-SYNTAX throw then ;

: VALIDATE ( -- )
   HEADER
   0 FRAME-N !
   begin
      CUR @ LIMIT @ >= if E-DIFF-SYNTAX throw then
      ART-A@ CUR @ + c@ TRAILER-C = if TRAILER exit then
      READ-SECTION
      CHECK-SECTION
      FRAME-N @ $7FFFFFFFFFFFFFFF = if E-DIFF-SYNTAX throw then
      FRAME-N @ 1+ FRAME-N !
   again ;

\ ---- owning + detaching buffers ------------------------------------------

: NEXT-CAP ( n n -- n ) {: need:n current:n :}
   current 1 < if 1 else current then
   begin dup need < while
      dup $3FFFFFFFFFFFFFFF > if drop need exit then
      2 *
   repeat ;

: OWN-ARTIFACT ( ptr u8 n -- ) {: a:ptr u:n :}
   u ART-CAP @ > if
      u ART-CAP @ NEXT-CAP {: cap:n :}
      cap MEM-ALLOC-BYTES drop ART-A !
      cap ART-CAP !
   then
   a ART-A@ u BYTE-COPY
   u ART-U ! ;

: DETACH ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u VIEW-CAP @ > if
      u VIEW-CAP @ NEXT-CAP {: cap:n :}
      cap MEM-ALLOC-BYTES drop VIEW-A !
      cap VIEW-CAP !
   then
   u 0 > if a VIEW-A @ u BYTE-COPY then
   VIEW-A @ u ;

: DETACH-FILE ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u FILE-VIEW-CAP @ > if
      u FILE-VIEW-CAP @ NEXT-CAP {: cap:n :}
      cap MEM-ALLOC-BYTES drop FILE-VIEW-A !
      cap FILE-VIEW-CAP !
   then
   u 0 > if a FILE-VIEW-A @ u BYTE-COPY then
   FILE-VIEW-A @ u ;

\ ---- streaming a section's raw body as events ----------------------------

: SEC-PATH$ ( -- ptr u8 n )
   SEC-NEW? if SEC-NEW$ else SEC-OLD$ then ;

: SET-EV ( ptr u8 n n event -- ) {: a:ptr u:n meta:n k:event :}
   a EV-A !  u EV-U !  meta EV-META !  k EV-KIND-AT ! ;

\ Store the current DIFF event into the EV holder; true if it is a real event,
\ false for the parser's structural NONE (which the reader skips).
: MAP-EVENT ( DIFF:event -- bool )
   MATCH DIFF:event
      none    OF false ENDOF
      section OF SEC-PATH$ DETACH-FILE 0 construct event file SET-EV true ENDOF
      hunk    OF s" " DIFF:HUNK-NEW-START construct event hunk SET-EV true ENDOF
      add     OF DIFF:CONTENT$ DETACH 0 construct event add SET-EV true ENDOF
      context OF DIFF:CONTENT$ DETACH 0 construct event context SET-EV true ENDOF
      delete  OF DIFF:CONTENT$ DETACH 0 construct event delete SET-EV true ENDOF
   ;MATCH ;

: NEXT-LINE ( -- ptr u8 n )
   RAW-OFF @ {: start:n :}
   start begin dup SEC-RAW-U @ < while
      dup SEC-RAW-A@ + c@ LF-C = if
         {: i:n :}
         i 1+ RAW-OFF !
         SEC-RAW-A@ start + i start -
         exit
      then
      1+
   repeat
   drop
   SEC-RAW-U @ RAW-OFF !
   SEC-RAW-A@ start + SEC-RAW-U @ start - ;

: START-NEXT-SECTION ( -- )
   READ-SECTION
   ITER-N @ SEC-IDX !
   ITER-N @ 1+ ITER-N !
   DIFF:RESET
   0 RAW-OFF !
   1 ITER-STATE ! ;

\ Advance the stream one unit: 0 an event is in EV, 1 retry, 2 end.
\ States: 0 idle (between sections), 1 feeding a body, 2 finishing a body,
\ 3 the section's closing FILE event was just handed out (fields still live).
: STEP ( -- n )
   ITER-STATE @ 0 = if
      ART-A@ CUR @ + c@ TRAILER-C = if 2 exit then
      START-NEXT-SECTION 1 exit
   then
   ITER-STATE @ 3 = if 0 ITER-STATE ! 1 exit then
   ITER-STATE @ 1 = if
      RAW-OFF @ SEC-RAW-U @ < if
         NEXT-LINE DIFF:LINE MAP-EVENT if 0 else 1 then exit
      then
      2 ITER-STATE ! 1 exit
   then
   DIFF:FINISH MAP-EVENT if 3 ITER-STATE ! 0 exit then
   0 ITER-STATE ! 1 ;

: NEED-OPEN ( -- )
   OPENED @ 0= if E-DIFF-SYNTAX throw then ;

: NEED-SECTION ( -- )
   NEED-OPEN
   ITER-STATE @ 0= if E-DIFF-SYNTAX throw then ;

public

: OPEN ( ptr u8 n -- ) {: a:ptr u:n :}
   u MIN-ART-U < if E-DIFF-SYNTAX throw then
   a u OWN-ARTIFACT
   u DIGEST-U - LIMIT !
   0 CUR !
   false OPENED !
   0 ITER-STATE !
   0 ITER-N !
   0 SEC-IDX !
   DIGEST-VALID? 0= if E-DIFF-SYNTAX throw then
   VALIDATE
   DATA-START @ CUR !
   true OPENED ! ;

: SECTION-COUNT ( -- n )
   NEED-OPEN
   FRAME-N @ ;

: FROM$ ( -- ptr u8 n )
   NEED-OPEN
   FROM-A @ FROM-U @ DETACH ;

: TO$ ( -- ptr u8 n )
   NEED-OPEN
   TO-A @ TO-U @ DETACH ;

: SECTION-INDEX ( -- n )
   NEED-SECTION
   SEC-IDX @ ;

: SECTION-STATUS ( -- status )
   NEED-SECTION
   SEC-STATUS-AT @ ;

: SECTION-FORM ( -- form )
   NEED-SECTION
   SEC-FORM-AT @ ;

: SECTION-BODY? ( -- bool )
   NEED-SECTION
   SEC-BODY @ 0<> ;

: SECTION-MODE? ( -- bool )
   NEED-SECTION
   SEC-MODE @ 0<> ;

: SECTION-OLD? ( -- bool )
   NEED-SECTION
   SEC-OLD? ;

: SECTION-NEW? ( -- bool )
   NEED-SECTION
   SEC-NEW? ;

: SECTION-OLD$ ( -- ptr u8 n )
   NEED-SECTION
   SEC-OLD$ DETACH ;

: SECTION-NEW$ ( -- ptr u8 n )
   NEED-SECTION
   SEC-NEW$ DETACH ;

\ Stream the next event of the artifact: a FILE event opens each section (its
\ file path as the span), then the section body's HUNK/ADD/CONTEXT/DELETE
\ events; a final NONE with present=false ends the walk.
: NEXT? ( -- ptr u8 n n event bool )
   NEED-OPEN
   begin
      STEP STEP-CODE !
      STEP-CODE @ 2 = if
         false OPENED !
         s" " 0 construct event none false exit
      then
      STEP-CODE @ 0 = if
         EV-A @ EV-U @ EV-META @ EV-KIND-AT @ true exit
      then
   again ;

;package
