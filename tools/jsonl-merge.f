\ jsonl-merge.f - validate and merge JSONL object rows.
\
\ Run:
\   bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f \
\     lib/fs-mutate.f tools/argv.f tools/json.f tools/json-file.f \
\     tools/jsonl-merge.f -- OUT.jsonl IN1.jsonl [IN2.jsonl ...]

74 constant JM-IO-RC
10 constant JM-LF-C
45 constant JM-MINUS
10 constant JM-DEC
48 constant JM-ZERO

create JM-LF 1 allot
create JM-TMP-PATH FS-PATH-CAP allot

variable JM-OUT-FD
variable JM-WR-OFF
variable JM-WR-N
variable JM-NODE
variable JM-KIND
variable JM-CODE
variable JM-TMP-U

JM-LF-C JM-LF c!
-1 JM-OUT-FD !

: JM-TRUE ( -- bool )
   0 0= ;

: JM-FALSE ( -- bool )
   JM-TRUE 0= ;

: JM-EMPTY$ ( -- ptr u8 n )
   s" " drop 0 ;

: JM-TMP$ ( -- ptr u8 n )
   JM-TMP-PATH JM-TMP-U @ ;

: JM-USAGE ( -- )
   s" tools/jsonl-merge.f OUT.jsonl IN1.jsonl [IN2.jsonl ...]" ARGV-USAGE! ;

: JM-CLOSE-OUT ( -- )
   JM-OUT-FD @ dup 0 >= if close else drop then
   -1 JM-OUT-FD ! ;

: JM-DIE ( ptr u8 n -- )
   JM-CLOSE-OUT
   JSONLF-CLOSE
   JM-TMP$ nip 0 > if JM-TMP$ EXISTS? if JM-TMP$ REMOVE-FILE then then
   JM-IO-RC die ;

: JM-CLOSE-COMMIT ( -- )
   JM-OUT-FD @ dup 0 >= if
      close
   else
      drop
   then
   -1 JM-OUT-FD ! ;

: JM-SB-U+ ( n -- ) {: n :}
   n JM-DEC >= if n JM-DEC / recurse then
   n JM-DEC mod JM-ZERO + SB-APPEND-C ;

: JM-SB-N+ ( n -- ) {: n :}
   n 0 < if
      JM-MINUS SB-APPEND-C
      0 n - JM-SB-U+
      exit
   then
   n JM-SB-U+ ;

: JM-BUILD-TMP ( ptr u8 n -- ) {: path:ptr pathu :}
   SB-RESET
   path pathu SB-APPEND
   s" .tmp-" SB-APPEND
   mono-ns JM-SB-U+
   SB$ {: a:ptr u :}
   u FS-PATH-CAP > if s" jsonl-merge: temp path too long" JM-DIE then
   a JM-TMP-PATH u BYTE-COPY
   u JM-TMP-U ! ;

: JM-ROW-DIE ( ptr u8 n n -- ) {: path:ptr pathu code :}
   SB-RESET
   s" jsonl-merge: invalid row " SB-APPEND
   path pathu SB-APPEND
   s" :" SB-APPEND
   JSONLF-LINE# JM-SB-U+
   s"  code " SB-APPEND
   code JM-SB-N+
   SB$ JM-DIE ;

: JM-WRITE-FD ( n ptr u8 n -- ) {: fd a:ptr u :}
   u 0 < if s" jsonl-merge: negative write length" JM-DIE then
   0 JM-WR-OFF !
   begin JM-WR-OFF @ u < while
      fd a JM-WR-OFF @ + u JM-WR-OFF @ - write JM-WR-N !
      JM-WR-N @ 0 <= if s" jsonl-merge: write failed" JM-DIE then
      JM-WR-N @ u JM-WR-OFF @ - > if s" jsonl-merge: bad write count" JM-DIE then
      JM-WR-OFF @ JM-WR-N @ + JM-WR-OFF !
   repeat ;

: JM-WRITE-LINE ( ptr u8 n -- ) {: a:ptr u :}
   JM-OUT-FD @ a u JM-WRITE-FD
   JM-OUT-FD @ JM-LF 1 JM-WRITE-FD ;

: JM-STORE-ROW ( n n n -- )
   JM-CODE !
   JM-KIND !
   JM-NODE ! ;

: JM-READ-ROW? ( -- bool )
   JSONLF-NEXT-ROW if
      JM-STORE-ROW
      JM-TRUE
      exit
   then
   JM-STORE-ROW
   JM-FALSE ;

: JM-CHECK-ROW ( ptr u8 n -- ) {: path:ptr pathu :}
   JM-KIND @ JSONL-ROW-BLANK = if exit then
   JM-KIND @ JSONL-ROW-ERROR = if path pathu JM-CODE @ JM-ROW-DIE then
   JM-KIND @ JSONL-ROW-JSON <> if path pathu E-JSON-TYPE JM-ROW-DIE then
   JM-NODE @ JSON-KIND J-OBJ <> if path pathu E-JSON-TYPE JM-ROW-DIE then ;

: JM-COPY-ROW? ( ptr u8 n -- bool ) {: path:ptr pathu :}
   JM-READ-ROW? 0= if JM-FALSE exit then
   path pathu JM-CHECK-ROW
   JM-KIND @ JSONL-ROW-BLANK = if JM-TRUE exit then
   JSONL-LINE$ JM-WRITE-LINE
   JM-TRUE ;

: JM-MERGE-FILE ( ptr u8 n -- ) {: path:ptr pathu :}
   path pathu JSONLF-OPEN
   begin path pathu JM-COPY-ROW? while repeat
   JSONLF-CLOSE ;

: JM-OPEN-OUT ( -- )
   0 ARGV-POS$ JM-BUILD-TMP
   JM-TMP$ JM-EMPTY$ WRITE-ALL
   JM-TMP$ OPEN-APPEND-FD JM-OUT-FD ! ;

: JM-COMMIT-OUT ( -- )
   JM-CLOSE-COMMIT
   JM-TMP$ 0 ARGV-POS$ RENAME-FILE
   0 JM-TMP-U ! ;

: JM-MERGE-INPUTS ( -- )
   1 begin dup ARGV-POS# < while
      dup ARGV-POS$ JM-MERGE-FILE
      1+
   repeat drop ;

: JM-MAIN ( -- )
   JM-USAGE
   ARGV-PARSE
   2 -1 ARGV-EXPECT-POS
   JM-OPEN-OUT
   JM-MERGE-INPUTS
   JM-COMMIT-OUT ;

JM-MAIN
