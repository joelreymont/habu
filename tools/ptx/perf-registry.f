\ perf-registry.f - checked kernel profile-row registry (tools/ptx/perf-rows.tsv).
\ Row format: 12 tab-separated fields per data line, `#`/blank lines ignored:
\   kernel grid gridy block blocky iters work metric value_x1000 device date note
\ metric is GBS, GFLOPS, PCT-ROOF, or WAIVER; WAIVER rows carry value 0 and a
\ mandatory note documenting the device-gated reason.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/adt/option.f

-7300 constant E-PERF-ROW    \ malformed or invalid registry row
-7301 constant E-PERF-CAP    \ registry buffer/row capacity exceeded
-7302 constant E-PERF-KEY    \ compared rows do not share kernel+config+device

\ typed STR:SPLIT-NEXT boundary: the field cursor stores raw byte offsets, so
\ project the byte-off / byte-len role results back to the raw cells the row
\ scanner threads through PF-START and the field span.
package CAD-NUM
public
: PF-BO>N ( CAD-NUM:byte-off -- n ) BYTE-OFF>N ;
: PF-BL>N ( CAD-NUM:byte-len -- n ) BYTE-LEN>N ;
;package

package PERF

$10000 constant BUF-CAP
512 constant ROW-MAX
16 constant ROW-CELLS
9 constant TAB-C
13 constant CR-C
10 constant LF-C
35 constant HASH-C

0 constant F-KOFF    1 constant F-KU
2 constant F-GRID    3 constant F-GRIDY
4 constant F-BLOCK   5 constant F-BLOCKY
6 constant F-ITERS   7 constant F-WORK
8 constant F-METRIC  9 constant F-VALUE
10 constant F-DOFF   11 constant F-DU
12 constant F-DATEOFF 13 constant F-DATEU
14 constant F-NOFF   15 constant F-NU

create BUF BUF-CAP allot
create ROWS ROW-MAX ROW-CELLS * cells allot

variable BUF-U
variable ROW-N
variable PF-START
variable PF-LOFF
variable PF-LU
variable LN-START
variable LN-I
variable LINE-NO
variable LOK-A
variable LOK-U

: PERF-TRUE ( -- bool )
   0 0= ;

: PERF-FALSE ( -- bool )
   PERF-TRUE 0= ;

: LOK-A-FIELD ( -- ptr ptr u8 )
   LOK-A 0 ptr-field ;

: ROW-CELL ( n n -- ptr a ) {: row:n fld:n :}
   row 0 < row ROW-MAX < 0= or if E-PERF-CAP throw then
   ROWS row ROW-CELLS * fld + cells + ;

: ROW@ ( n n -- n )   \ committed-row field read; a row >= ROW-N is stale, not data
   over ROW-N @ < 0= if E-PERF-CAP throw then
   ROW-CELL @ ;

: CUR! ( n n -- ) {: v:n fld:n :}
   v ROW-N @ fld ROW-CELL ! ;

: CUR@ ( n -- n )   \ in-progress row (slot ROW-N); bypasses the committed-read guard
   ROW-N @ swap ROW-CELL @ ;

: ROW-SPAN$ ( n n n -- ptr u8 n ) {: i:n fo:n fu:n :}
   BUF i fo ROW@ +  i fu ROW@ ;

: CUR-SPAN$ ( n n -- ptr u8 n ) {: fo:n fu:n :}   \ in-progress row span (slot ROW-N)
   BUF ROW-N @ fo ROW-CELL @ +  ROW-N @ fu ROW-CELL @ ;

: LINE$ ( -- ptr u8 n )
   BUF PF-LOFF @ + PF-LU @ ;

: FIELD-NEXT ( -- ptr u8 n )
   LINE$ STR:LENGTH TAB-C PF-START @ STR:OFFSET STR:SPLIT-NEXT 0= if E-PERF-ROW throw then
   CAD-NUM:PF-BO>N PF-START !
   CAD-NUM:PF-BL>N ;

: FIELDS-END-CHECK ( -- )
   LINE$ STR:LENGTH TAB-C PF-START @ STR:OFFSET STR:SPLIT-NEXT if drop 2drop E-PERF-ROW throw then
   drop 2drop ;

: FIELD-NUM ( -- n )
   FIELD-NEXT STR>NUMBER? MATCH option
     none OF E-PERF-ROW throw ENDOF
     some OF ENDOF
   ;MATCH
   dup 0 < if E-PERF-ROW throw then ;

: FIELD-SPAN ( n n -- ) {: fo:n fu:n :}
   FIELD-NEXT {: a:ptr u:n :}
   a BUF - fo CUR!
   u fu CUR! ;

: FIELD-INT ( n -- ) {: fld:n :}
   FIELD-NUM fld CUR! ;

: DIGIT? ( n -- bool )
   dup 47 > swap 58 < and ;

: DATE-BYTE-OK? ( ptr u8 n -- bool ) {: a:ptr i:n :}
   i 4 = i 7 = or if a i + c@ 45 = exit then
   a i + c@ DIGIT? ;

: DATE-2DIGIT ( ptr u8 n -- n ) {: a:ptr i:n :}   \ value of the two digits at offset i
   a i + c@ 48 - 10 *  a i 1+ + c@ 48 - + ;

: DATE-OK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 10 <> if PERF-FALSE exit then
   0 begin dup 10 < while
      a over DATE-BYTE-OK? 0= if drop PERF-FALSE exit then
      1+
   repeat drop
   a 5 DATE-2DIGIT {: mm:n :}          \ month at chars 5..6
   a 8 DATE-2DIGIT {: dd:n :}          \ day at chars 8..9
   mm 1 < mm 12 > or if PERF-FALSE exit then
   dd 1 < dd 31 > or if PERF-FALSE exit then
   PERF-TRUE ;

public

0 constant M-GBS
1 constant M-GFLOPS
2 constant M-PCT-ROOF
3 constant M-WAIVER

private

: METRIC-CODE? ( ptr u8 n -- n ) {: a:ptr u:n :}   \ metric code, -1 if unknown
   a u s" GBS" STR= if M-GBS exit then
   a u s" GFLOPS" STR= if M-GFLOPS exit then
   a u s" PCT-ROOF" STR= if M-PCT-ROOF exit then
   a u s" WAIVER" STR= if M-WAIVER exit then
   -1 ;

: FIELD-METRIC ( -- n )
   FIELD-NEXT METRIC-CODE?
   dup 0 < if E-PERF-ROW throw then ;

: CONFIG-OK? ( -- bool )
   F-GRID CUR@ 1 <
   F-GRIDY CUR@ 1 < or
   F-BLOCK CUR@ 1 < or
   F-BLOCKY CUR@ 1 < or
   F-ITERS CUR@ 1 < or
   F-WORK CUR@ 1 < or
   F-VALUE CUR@ 1 < or
   0= ;

: WAIVER-CHECK ( -- )
   F-VALUE CUR@ 0 <> if E-PERF-ROW throw then
   F-NOFF F-NU CUR-SPAN$ nip 0= if E-PERF-ROW throw then ;

: METRIC-CHECK ( -- )
   F-METRIC CUR@ M-WAIVER = if WAIVER-CHECK exit then
   CONFIG-OK? 0= if E-PERF-ROW throw then ;

: ROW-CHECK ( -- )
   F-KOFF F-KU CUR-SPAN$ nip 0= if E-PERF-ROW throw then
   F-DOFF F-DU CUR-SPAN$ nip 0= if E-PERF-ROW throw then
   F-DATEOFF F-DATEU CUR-SPAN$ DATE-OK? 0= if E-PERF-ROW throw then
   METRIC-CHECK ;

: ROW-PARSE ( -- )
   ROW-N @ ROW-MAX < 0= if E-PERF-CAP throw then
   0 PF-START !
   F-KOFF F-KU FIELD-SPAN
   F-GRID FIELD-INT
   F-GRIDY FIELD-INT
   F-BLOCK FIELD-INT
   F-BLOCKY FIELD-INT
   F-ITERS FIELD-INT
   F-WORK FIELD-INT
   FIELD-METRIC F-METRIC CUR!
   F-VALUE FIELD-INT
   F-DOFF F-DU FIELD-SPAN
   F-DATEOFF F-DATEU FIELD-SPAN
   F-NOFF F-NU FIELD-SPAN
   FIELDS-END-CHECK
   ROW-CHECK
   ROW-N @ 1+ ROW-N ! ;

: WS? ( n -- bool )
   dup 32 = over TAB-C = or swap CR-C = or ;

public

: LINE-DATA? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ non-blank, non-comment line
   0 begin dup u < while
      a over + c@ WS? 0= if
         a + c@ HASH-C <> exit
      then
      1+
   repeat drop PERF-FALSE ;

: RESET ( -- )
   0 BUF-U !
   0 ROW-N !
   0 LINE-NO ! ;

: ADD-LINE ( ptr u8 n -- ) {: a:ptr u:n :}   \ append one registry line and parse it
   BUF-U @ u + BUF-CAP > if E-PERF-CAP throw then
   a BUF BUF-U @ + u BYTE-COPY
   BUF-U @ PF-LOFF !
   u PF-LU !
   BUF-U @ u + BUF-U !
   LINE-NO @ 1+ LINE-NO !
   LINE$ LINE-DATA? if ROW-PARSE then ;

: LINE-OK? ( ptr u8 n -- bool ) {: a:ptr u:n :}   \ blank/comment or valid data row
   BUF-U @ ROW-N @ LINE-NO @ {: bu:n rn:n ln:n :}
   a LOK-A-FIELD !
   u LOK-U !
   [: LOK-A-FIELD @ LOK-U @ ADD-LINE ;] catch {: code:n :}
   bu BUF-U !
   rn ROW-N !
   ln LINE-NO !
   code 0= ;

private

: LINE-LEN-SANS-CR ( n n -- n ) {: off:n u:n :}
   u 0= if 0 exit then
   BUF off + u 1- + c@ CR-C = if u 1- exit then
   u ;

: PARSE-AT ( n n -- ) {: off:n u:n :}
   off PF-LOFF !
   off u LINE-LEN-SANS-CR PF-LU !
   LINE-NO @ 1+ LINE-NO !
   LINE$ LINE-DATA? if ROW-PARSE then ;

: PARSE-ALL ( -- )
   0 LN-START !
   0 LN-I !
   begin LN-I @ BUF-U @ < while
      BUF LN-I @ + c@ LF-C = if
         LN-START @  LN-I @ LN-START @ -  PARSE-AT
         LN-I @ 1+ LN-START !
      then
      LN-I @ 1+ LN-I !
   repeat
   LN-START @ BUF-U @ < if
      LN-START @  BUF-U @ LN-START @ -  PARSE-AT
   then ;

public

: LOAD ( ptr u8 n -- ) {: a:ptr u:n :}
   RESET
   a u FILE-SIZE BUF-CAP > if E-PERF-CAP throw then
   a u BUF BUF-CAP READ-ALL BUF-U !
   PARSE-ALL ;

: LINE@ ( -- n )   \ 1-based line number of the last parsed line
   LINE-NO @ ;

: LAST-LINE$ ( -- ptr u8 n )   \ text of the last line parsed (the offender after a failed LOAD)
   LINE$ ;

: ROW# ( -- n )
   ROW-N @ ;

: KERNEL$ ( n -- ptr u8 n )
   F-KOFF F-KU ROW-SPAN$ ;

: DEVICE$ ( n -- ptr u8 n )
   F-DOFF F-DU ROW-SPAN$ ;

: DATE$ ( n -- ptr u8 n )
   F-DATEOFF F-DATEU ROW-SPAN$ ;

: NOTE$ ( n -- ptr u8 n )
   F-NOFF F-NU ROW-SPAN$ ;

: GRID@ ( n -- n )
   F-GRID ROW@ ;

: GRIDY@ ( n -- n )
   F-GRIDY ROW@ ;

: BLOCK@ ( n -- n )
   F-BLOCK ROW@ ;

: BLOCKY@ ( n -- n )
   F-BLOCKY ROW@ ;

: ITERS@ ( n -- n )
   F-ITERS ROW@ ;

: WORK@ ( n -- n )
   F-WORK ROW@ ;

: METRIC@ ( n -- n )
   F-METRIC ROW@ ;

: VALUE@ ( n -- n )
   F-VALUE ROW@ ;

: WAIVER? ( n -- bool )
   METRIC@ M-WAIVER = ;

: METRIC$ ( n -- ptr u8 n ) {: m:n :}
   m M-GBS = if s" GBS" exit then
   m M-GFLOPS = if s" GFLOPS" exit then
   m M-PCT-ROOF = if s" PCT-ROOF" exit then
   s" WAIVER" ;

: KEY= ( n n -- bool ) {: i:n j:n :}
   i KERNEL$ j KERNEL$ STR= 0= if PERF-FALSE exit then
   i GRID@ j GRID@ <> if PERF-FALSE exit then
   i GRIDY@ j GRIDY@ <> if PERF-FALSE exit then
   i BLOCK@ j BLOCK@ <> if PERF-FALSE exit then
   i BLOCKY@ j BLOCKY@ <> if PERF-FALSE exit then
   i ITERS@ j ITERS@ <> if PERF-FALSE exit then
   i WORK@ j WORK@ <> if PERF-FALSE exit then
   i METRIC@ j METRIC@ <> if PERF-FALSE exit then
   i DEVICE$ j DEVICE$ STR= ;

;package
