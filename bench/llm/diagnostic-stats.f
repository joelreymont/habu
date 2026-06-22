\ diagnostic-stats.f - checked diagnostic field and repair-class reducers.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/json-write.f,
\ tools/json.f, and bench/llm/manifest.f.

8 constant DGS-KNOWN#
64 constant DGS-CLASS-MAX
64 constant DGS-CLASS-CAP
512 constant DGS-PAIR-MAX
$8000 constant DGS-EVENT-CAP
20 constant DGS-NUM-CAP

create DGS-CLASS-BUF DGS-CLASS-MAX DGS-CLASS-CAP * allot
create DGS-CLASS-U DGS-CLASS-MAX cells allot
create DGS-DIAG-N DGS-CLASS-MAX cells allot
create DGS-ROUND-N DGS-CLASS-MAX cells allot
create DGS-FIRST-ROUND DGS-CLASS-MAX cells allot
create DGS-FIRST-ORDER DGS-CLASS-MAX cells allot
create DGS-EMITTED DGS-CLASS-MAX cells allot
create DGS-PAIR-CLASS DGS-PAIR-MAX cells allot
create DGS-PAIR-ROUND DGS-PAIR-MAX cells allot
create DGS-EVENT-BUF DGS-EVENT-CAP allot
create DGS-NUM-BUF DGS-NUM-CAP allot

variable DGS-CLASS#
variable DGS-PAIR#
variable DGS-I
variable DGS-NEXT
variable DGS-CAND
variable DGS-FIRST
variable DGS-TMP-IDX
variable DGS-ORDER#
variable DGS-NUM-I
variable DGS-EVENT-U
variable DGS-DIAG#
variable DGS-HAS-TOKEN
variable DGS-HAS-SPAN
variable DGS-HAS-EXPECTED
variable DGS-HAS-ACTUAL
variable DGS-HAS-CODE
variable DGS-HAS-REPAIR-CLASS

: DGS-TRUE ( -- bool )
   0 0= ;

: DGS-FALSE ( -- bool )
   DGS-TRUE 0= ;

: DGS-CHECK-KNOWN ( n -- ) {: idx :}
   idx 0 < if E-BM-FIELD throw then
   idx DGS-KNOWN# >= if E-BM-FIELD throw then ;

: DGS-KNOWN$ ( n -- ptr u8 n ) {: idx :}
   idx DGS-CHECK-KNOWN
   idx 0= if s" remove_producer" exit then
   idx 1 = if s" add_producer" exit then
   idx 2 = if s" fix_type" exit then
   idx 3 = if s" fix_return_stack" exit then
   idx 4 = if s" trusted_boundary_required" exit then
   idx 5 = if s" fix_signature_syntax" exit then
   idx 6 = if s" rewrite_uncheckable" exit then
   s" unknown_rejection" ;

: DGS-CHECK-CLASS ( n -- ) {: idx :}
   idx 0 < if E-BM-FIELD throw then
   idx DGS-CLASS-MAX >= if E-BM-FIELD throw then ;

: DGS-CLASS-SLOT ( n -- ptr u8 ) {: idx :}
   idx DGS-CHECK-CLASS
   DGS-CLASS-BUF idx DGS-KNOWN# - DGS-CLASS-CAP * + ;

: DGS-CLASS-U-PTR ( n -- ptr n ) {: idx :}
   idx DGS-CHECK-CLASS
   DGS-CLASS-U idx cells + ;

: DGS-DIAG-N-PTR ( n -- ptr n ) {: idx :}
   idx DGS-CHECK-CLASS
   DGS-DIAG-N idx cells + ;

: DGS-ROUND-N-PTR ( n -- ptr n ) {: idx :}
   idx DGS-CHECK-CLASS
   DGS-ROUND-N idx cells + ;

: DGS-FIRST-ROUND-PTR ( n -- ptr n ) {: idx :}
   idx DGS-CHECK-CLASS
   DGS-FIRST-ROUND idx cells + ;

: DGS-FIRST-ORDER-PTR ( n -- ptr n ) {: idx :}
   idx DGS-CHECK-CLASS
   DGS-FIRST-ORDER idx cells + ;

: DGS-EMITTED-PTR ( n -- ptr n ) {: idx :}
   idx DGS-CHECK-CLASS
   DGS-EMITTED idx cells + ;

: DGS-PAIR-CLASS-PTR ( n -- ptr n ) {: idx :}
   idx 0 < if E-BM-FIELD throw then
   idx DGS-PAIR-MAX >= if E-BM-FIELD throw then
   DGS-PAIR-CLASS idx cells + ;

: DGS-PAIR-ROUND-PTR ( n -- ptr n ) {: idx :}
   idx 0 < if E-BM-FIELD throw then
   idx DGS-PAIR-MAX >= if E-BM-FIELD throw then
   DGS-PAIR-ROUND idx cells + ;

: DGS-CLASS$ ( n -- ptr u8 n ) {: idx :}
   idx DGS-KNOWN# < if idx DGS-KNOWN$ exit then
   idx DGS-CLASS-SLOT idx DGS-CLASS-U-PTR @ ;

: DGS-STR< ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u b:ptr v :}
   0 DGS-I !
   begin DGS-I @ u < DGS-I @ v < and while
      a DGS-I @ + c@ b DGS-I @ + c@
      2dup < if 2drop DGS-TRUE exit then
      > if DGS-FALSE exit then
      DGS-I @ 1+ DGS-I !
   repeat
   u v < ;

: DGS-RESET-DIAGS ( -- )
   0 DGS-DIAG# !
   0 DGS-HAS-TOKEN !
   0 DGS-HAS-SPAN !
   0 DGS-HAS-EXPECTED !
   0 DGS-HAS-ACTUAL !
   0 DGS-HAS-CODE !
   0 DGS-HAS-REPAIR-CLASS ! ;

: DGS-RESET-EVENTS ( -- )
   DGS-KNOWN# DGS-CLASS# !
   0 DGS-PAIR# !
   0 DGS-ORDER# !
   0 begin dup DGS-CLASS-MAX < while
      0 over DGS-CLASS-U-PTR !
      0 over DGS-DIAG-N-PTR !
      0 over DGS-ROUND-N-PTR !
      0 over DGS-FIRST-ROUND-PTR !
      0 over DGS-FIRST-ORDER-PTR !
      0 over DGS-EMITTED-PTR !
      1+
   repeat drop ;

: DGS-RESET ( -- )
   DGS-RESET-DIAGS
   DGS-RESET-EVENTS ;

: DGS-EVENT-ROOM ( n -- ) {: add :}
   add 0 < if E-BM-FIELD throw then
   add DGS-EVENT-CAP DGS-EVENT-U @ - > if E-BM-FIELD throw then ;

: DGS-EVENT+ ( ptr u8 n -- ) {: a:ptr u :}
   u DGS-EVENT-ROOM
   a DGS-EVENT-BUF DGS-EVENT-U @ + u BYTE-COPY
   DGS-EVENT-U @ u + DGS-EVENT-U ! ;

: DGS-EVENT-C ( n -- ) {: c :}
   1 DGS-EVENT-ROOM
   c DGS-EVENT-BUF DGS-EVENT-U @ + c!
   DGS-EVENT-U @ 1+ DGS-EVENT-U ! ;

: DGS-EVENT-U+ ( n -- ) {: u :}
   u 0 < if E-BM-FIELD throw then
   DGS-NUM-CAP DGS-NUM-I !
   u 0= if s" 0" DGS-EVENT+ exit then
   u begin dup 0 > while
      dup 10 mod 48 +
      DGS-NUM-I @ 1- DGS-NUM-I !
      DGS-NUM-BUF DGS-NUM-I @ + c!
      10 /
   repeat drop
   DGS-NUM-BUF DGS-NUM-I @ + DGS-NUM-CAP DGS-NUM-I @ - DGS-EVENT+ ;

: DGS-KNOWN-INDEX? ( ptr u8 n -- n bool ) {: a:ptr u :}
   0 begin dup DGS-KNOWN# < while
      dup DGS-KNOWN$ a u STR= if DGS-TRUE exit then
      1+
   repeat drop 0 DGS-FALSE ;

: DGS-FIND-UNKNOWN? ( ptr u8 n -- n bool ) {: a:ptr u :}
   DGS-KNOWN# begin dup DGS-CLASS# @ < while
      dup DGS-CLASS$ a u STR= if DGS-TRUE exit then
      1+
   repeat drop 0 DGS-FALSE ;

: DGS-COPY-UNKNOWN ( ptr u8 n n -- ) {: a:ptr u idx :}
   u 0 <= if E-BM-FIELD throw then
   u DGS-CLASS-CAP > if E-BM-FIELD throw then
   a idx DGS-CLASS-SLOT u BYTE-COPY
   u idx DGS-CLASS-U-PTR ! ;

: DGS-ADD-UNKNOWN ( ptr u8 n -- n ) {: a:ptr u :}
   DGS-CLASS# @ DGS-CLASS-MAX >= if E-BM-FIELD throw then
   DGS-CLASS# @ DGS-TMP-IDX !
   a u DGS-TMP-IDX @ DGS-COPY-UNKNOWN
   DGS-CLASS# @ 1+ DGS-CLASS# !
   DGS-TMP-IDX @ ;

: DGS-CLASS-INDEX-TRIMMED ( ptr u8 n -- n ) {: a:ptr u :}
   u 0= if E-BM-FIELD throw then
   a u DGS-KNOWN-INDEX? if exit then drop
   a u DGS-FIND-UNKNOWN? if exit then drop
   a u DGS-ADD-UNKNOWN ;

: DGS-CLASS-INDEX ( ptr u8 n -- n )
   TRIM DGS-CLASS-INDEX-TRIMMED ;

: DGS-COUNT-DIAG ( n -- ) {: idx :}
   idx DGS-DIAG-N-PTR @ 1+ idx DGS-DIAG-N-PTR ! ;

: DGS-MARK-FIRST ( n n -- ) {: round idx :}
   idx DGS-FIRST-ORDER-PTR @ 0 <> if exit then
   round idx DGS-FIRST-ROUND-PTR !
   DGS-ORDER# @ 1+ DGS-ORDER# !
   DGS-ORDER# @ idx DGS-FIRST-ORDER-PTR ! ;

: DGS-COUNT-ROUND ( n -- ) {: idx :}
   idx DGS-ROUND-N-PTR @ 1+ idx DGS-ROUND-N-PTR ! ;

: DGS-PAIR-SEEN? ( n n -- bool ) {: idx round :}
   0 begin dup DGS-PAIR# @ < while
      dup DGS-PAIR-CLASS-PTR @ idx =
      over DGS-PAIR-ROUND-PTR @ round = and if drop DGS-TRUE exit then
      1+
   repeat drop DGS-FALSE ;

: DGS-ADD-PAIR ( n n -- ) {: idx round :}
   idx round DGS-PAIR-SEEN? if exit then
   DGS-PAIR# @ DGS-PAIR-MAX >= if E-BM-FIELD throw then
   idx DGS-PAIR# @ DGS-PAIR-CLASS-PTR !
   round DGS-PAIR# @ DGS-PAIR-ROUND-PTR !
   DGS-PAIR# @ 1+ DGS-PAIR# !
   idx DGS-COUNT-ROUND ;

: DGS-PARSE-ROUND ( ptr u8 n -- n )
   TRIM STR>NUMBER? 0= if E-BM-FIELD throw then
   dup 0 < if E-BM-FIELD throw then ;

: DGS-EVENT-ROUND ( ptr u8 n -- n )
   0 BM-FIELD$ DGS-PARSE-ROUND ;

: DGS-EVENT-CLASS$ ( ptr u8 n -- ptr u8 n )
   1 BM-FIELD$ TRIM ;

: DGS-HANDLE-EVENT-CLASS ( n ptr u8 n -- ) {: round cls:ptr clsu :}
   clsu 0= if exit then
   cls clsu DGS-CLASS-INDEX DGS-TMP-IDX !
   round DGS-TMP-IDX @ DGS-MARK-FIRST
   DGS-TMP-IDX @ DGS-COUNT-DIAG
   DGS-TMP-IDX @ round DGS-ADD-PAIR ;

: DGS-HANDLE-EVENT ( ptr u8 n -- ) {: a:ptr u :}
   a u BM-BLANK-OR-COMMENT? if exit then
   a u 2 BM-REQUIRE-FIELDS
   a u DGS-EVENT-ROUND
   a u DGS-EVENT-CLASS$ DGS-HANDLE-EVENT-CLASS ;

: DGS-SCAN-EVENTS ( ptr u8 n -- ) {: a:ptr u :}
   DGS-RESET-EVENTS
   0 DGS-NEXT !
   begin a u DGS-NEXT @ BM-LINE-NEXT while
      DGS-NEXT !
      DGS-HANDLE-EVENT
   repeat
   drop 2drop ;

: DGS-HAS-KEY? ( n ptr u8 n -- bool )
   JSON-GET -1 <> ;

: DGS-MARK-DIAG-FIELDS ( n -- ) {: node :}
   DGS-DIAG# @ 1+ DGS-DIAG# !
   node s" token" DGS-HAS-KEY? if -1 DGS-HAS-TOKEN ! then
   node s" byte_start" DGS-HAS-KEY? if -1 DGS-HAS-SPAN ! then
   node s" expected" DGS-HAS-KEY? if -1 DGS-HAS-EXPECTED ! then
   node s" actual" DGS-HAS-KEY? if -1 DGS-HAS-ACTUAL ! then
   node s" code" DGS-HAS-KEY? if -1 DGS-HAS-CODE ! then
   node s" repair_class" DGS-HAS-KEY? if -1 DGS-HAS-REPAIR-CLASS ! then ;

: DGS-SCAN-DIAGS ( ptr u8 n -- ) {: a:ptr u :}
   DGS-RESET-DIAGS
   a u JSONL-START-STRICT
   begin JSONL-NEXT-OBJECT dup -1 <> while
      DGS-MARK-DIAG-FIELDS
   repeat drop ;

: DGS-FIELD-OK? ( ptr n -- bool ) {: flagp:ptr :}
   DGS-DIAG# @ 0= if DGS-TRUE exit then
   flagp @ 0 <> ;

: DGS-DIAGNOSTIC-COUNT ( -- n )
   DGS-DIAG# @ ;

: DGS-DIAGNOSTIC-TOKEN? ( -- bool )
   DGS-HAS-TOKEN DGS-FIELD-OK? ;

: DGS-DIAGNOSTIC-SPAN? ( -- bool )
   DGS-HAS-SPAN DGS-FIELD-OK? ;

: DGS-DIAGNOSTIC-EXPECTED? ( -- bool )
   DGS-HAS-EXPECTED DGS-FIELD-OK? ;

: DGS-DIAGNOSTIC-ACTUAL? ( -- bool )
   DGS-HAS-ACTUAL DGS-FIELD-OK? ;

: DGS-DIAGNOSTIC-CODE? ( -- bool )
   DGS-HAS-CODE DGS-FIELD-OK? ;

: DGS-DIAGNOSTIC-REPAIR-CLASS? ( -- bool )
   DGS-HAS-REPAIR-CLASS DGS-FIELD-OK? ;

: DGS-CHECK-REPAIR-CLASS-NODE ( n -- ) {: cls :}
   cls -1 = if exit then
   cls JSON-KIND J-STR <> if E-BM-SCHEMA throw then
   cls JSON-STRING$ nip 0= if E-BM-FIELD throw then ;

: DGS-REPAIR-CLASS$? ( n -- ptr u8 n bool ) {: node :}
   node s" repair_class" JSON-GET {: cls :}
   cls DGS-CHECK-REPAIR-CLASS-NODE
   cls -1 = if DGS-EVENT-BUF 0 DGS-FALSE exit then
   cls JSON-STRING$
   DGS-TRUE ;

: DGS-APPEND-DIAG-EVENT ( n n -- ) {: node round :}
   round 0 < if E-BM-FIELD throw then
   node DGS-REPAIR-CLASS$? if
      round DGS-EVENT-U+
      STR-TAB DGS-EVENT-C
      DGS-EVENT+
      STR-LF DGS-EVENT-C
   else
      2drop
   then ;

: DGS-EVENTS-FROM-DIAGS$ ( ptr u8 n n -- ptr u8 n ) {: a:ptr u round :}
   round 0 < if E-BM-FIELD throw then
   0 DGS-EVENT-U !
   a u JSONL-START-STRICT
   begin JSONL-NEXT-OBJECT dup -1 <> while
      round DGS-APPEND-DIAG-EVENT
   repeat drop
   DGS-EVENT-BUF DGS-EVENT-U @ ;

: DGS-MAYBE-COMMA ( -- )
   DGS-FIRST @ if
      0 DGS-FIRST !
   else
      JW-COMMA
   then ;

: DGS-EMIT-STAT ( n bool n -- ) {: idx success delta :}
   idx DGS-DIAG-N-PTR @ 0= if exit then
   DGS-MAYBE-COMMA
   JW-OBJECT-START
   s" repair_class" idx DGS-CLASS$ JW-FIELD-S
   JW-COMMA s" diagnostic_count" idx DGS-DIAG-N-PTR @ JW-FIELD-U
   JW-COMMA s" repair_success" success JW-FIELD-BOOL
   JW-COMMA s" repair_iterations" idx DGS-ROUND-N-PTR @ JW-FIELD-U
   JW-COMMA s" first_round" idx DGS-FIRST-ROUND-PTR @ JW-FIELD-U
   JW-COMMA s" first_order" idx DGS-FIRST-ORDER-PTR @ JW-FIELD-U
   JW-COMMA s" token_delta" delta JW-FIELD-U
   JW-OBJECT-END ;

: DGS-RESET-EMITTED ( -- )
   DGS-KNOWN# begin dup DGS-CLASS# @ < while
      0 over DGS-EMITTED-PTR !
      1+
   repeat drop ;

: DGS-CANDIDATE-BETTER? ( n -- bool ) {: idx :}
   DGS-CAND @ -1 = if DGS-TRUE exit then
   idx DGS-CLASS$ DGS-CAND @ DGS-CLASS$ DGS-STR< ;

: DGS-NEXT-UNKNOWN? ( -- n bool )
   -1 DGS-CAND !
   DGS-KNOWN# begin dup DGS-CLASS# @ < while
      dup DGS-EMITTED-PTR @ 0= if
         dup DGS-CANDIDATE-BETTER? if dup DGS-CAND ! then
      then
      1+
   repeat drop
   DGS-CAND @ -1 = if 0 DGS-FALSE exit then
   DGS-CAND @ DGS-TRUE ;

: DGS-EMIT-KNOWN ( bool n -- ) {: success delta :}
   0 begin dup DGS-KNOWN# < while
      dup success delta DGS-EMIT-STAT
      1+
   repeat drop ;

: DGS-EMIT-UNKNOWN ( bool n -- ) {: success delta :}
   DGS-RESET-EMITTED
   begin DGS-NEXT-UNKNOWN? while
      dup DGS-EMITTED-PTR -1 swap !
      success delta DGS-EMIT-STAT
   repeat drop ;

: DGS-REPAIR-STATS$ ( ptr u8 n bool n -- ptr u8 n ) {: events:ptr eventsu success delta :}
   events eventsu DGS-SCAN-EVENTS
   JW-RESET
   JW-ARRAY-START
   -1 DGS-FIRST !
   success delta DGS-EMIT-KNOWN
   success delta DGS-EMIT-UNKNOWN
   JW-ARRAY-END
   JW$ ;
