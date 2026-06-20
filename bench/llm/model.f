\ model.f - checked model registry scanner for LLM benchmarks.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, and bench/llm/manifest.f.

65536 constant MR-REG-CAP
1024 constant MR-FIELD-CAP
-3202 constant E-BM-MODEL-NOT-FOUND

create MR-REG-BUF MR-REG-CAP allot
create MR-ID-BUF MR-FIELD-CAP allot
create MR-LABEL-BUF MR-FIELD-CAP allot
create MR-COMMAND-BUF MR-FIELD-CAP allot
create MR-ARGS-BUF MR-FIELD-CAP allot
create MR-PARSER-BUF MR-FIELD-CAP allot
create MR-TOKEN-FIELDS-BUF MR-FIELD-CAP allot
create MR-TIMEOUT-BUF MR-FIELD-CAP allot

variable MR-REG-LEN
variable MR-NEXT
variable MR-LINE-A
variable MR-LINE-U
variable MR-ID-U
variable MR-LABEL-U
variable MR-COMMAND-U
variable MR-ARGS-U
variable MR-PARSER-U
variable MR-TOKEN-FIELDS-U
variable MR-TIMEOUT-U
variable MR-TIMEOUT#
variable MR-COUNT#

: MR-TRUE ( -- bool )
   0 0= ;

: MR-FALSE ( -- bool )
   MR-TRUE 0= ;

TRUSTED: MR-LINE! ( ptr u8 n -- )
   MR-LINE-U !
   MR-LINE-A ! ;

TRUSTED: MR-LINE$ ( -- ptr u8 n )
   MR-LINE-A @
   MR-LINE-U @ ;

: MR-COPY$ ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u 0 < if E-BM-FIELD throw then
   u MR-FIELD-CAP > if E-BM-FIELD throw then
   a dst u BYTE-COPY
   u lenp ! ;

: MR-RESET-SELECTION ( -- )
   0 MR-ID-U !
   0 MR-LABEL-U !
   0 MR-COMMAND-U !
   0 MR-ARGS-U !
   0 MR-PARSER-U !
   0 MR-TOKEN-FIELDS-U !
   0 MR-TIMEOUT-U !
   0 MR-TIMEOUT# ! ;

: MR-REGISTRY! ( ptr u8 n -- ) {: a:ptr u :}
   u 0 < if E-BM-SCHEMA throw then
   u MR-REG-CAP > if E-BM-SCHEMA throw then
   a MR-REG-BUF u BYTE-COPY
   u MR-REG-LEN !
   0 MR-NEXT !
   MR-RESET-SELECTION ;

: MR-LOAD ( ptr u8 n -- )
   MR-REG-BUF MR-REG-CAP READ-ALL MR-REG-LEN !
   0 MR-NEXT !
   MR-RESET-SELECTION ;

: MR-READ-LINE ( -- bool )
   MR-REG-BUF MR-REG-LEN @ MR-NEXT @ BM-LINE-NEXT if
      MR-NEXT !
      MR-LINE!
      MR-TRUE
   else
      drop 2drop MR-FALSE
   then ;

: MR-REQUIRE-HEADER ( -- )
   0 MR-NEXT !
   MR-READ-LINE 0= if E-BM-SCHEMA throw then
   MR-LINE$ BM-REQUIRE-MODEL-HEADER ;

: MR-NONEMPTY ( ptr u8 n -- )
   nip 0= if E-BM-FIELD throw then ;

: MR-TIMEOUT>N ( ptr u8 n -- n ) {: a:ptr u :}
   a u TRIM STR>NUMBER? 0= if E-BM-FIELD throw then
   dup 0 < if E-BM-FIELD throw then ;

: MR-LINE-FIELD$ ( n -- ptr u8 n )
   MR-LINE$ rot BM-MODEL-FIELD$ ;

: MR-VALIDATE-LINE ( -- )
   MR-LINE$ BM-MODEL-FIELDS BM-REQUIRE-FIELDS
   BM-M-ID MR-LINE-FIELD$ MR-NONEMPTY
   BM-M-LABEL MR-LINE-FIELD$ MR-NONEMPTY
   BM-M-COMMAND MR-LINE-FIELD$ MR-NONEMPTY
   BM-M-PARSER MR-LINE-FIELD$ MR-NONEMPTY
   BM-M-TIMEOUT MR-LINE-FIELD$ MR-NONEMPTY
   BM-M-TIMEOUT MR-LINE-FIELD$ MR-TIMEOUT>N drop ;

: MR-COPY-FIELD ( n ptr u8 ptr n -- ) {: idx dst:ptr lenp:ptr :}
   idx MR-LINE-FIELD$ dst lenp MR-COPY$ ;

: MR-COPY-SELECTION ( -- )
   MR-VALIDATE-LINE
   BM-M-ID MR-ID-BUF MR-ID-U MR-COPY-FIELD
   BM-M-LABEL MR-LABEL-BUF MR-LABEL-U MR-COPY-FIELD
   BM-M-COMMAND MR-COMMAND-BUF MR-COMMAND-U MR-COPY-FIELD
   BM-M-ARGS MR-ARGS-BUF MR-ARGS-U MR-COPY-FIELD
   BM-M-PARSER MR-PARSER-BUF MR-PARSER-U MR-COPY-FIELD
   BM-M-TOKEN-FIELDS MR-TOKEN-FIELDS-BUF MR-TOKEN-FIELDS-U MR-COPY-FIELD
   BM-M-TIMEOUT MR-TIMEOUT-BUF MR-TIMEOUT-U MR-COPY-FIELD
   BM-M-TIMEOUT MR-LINE-FIELD$ MR-TIMEOUT>N MR-TIMEOUT# ! ;

: MR-LINE-ID$ ( -- ptr u8 n )
   BM-M-ID MR-LINE-FIELD$ ;

: MR-ID-MATCH? ( ptr u8 n -- bool ) {: id:ptr idu :}
   idu 0= if MR-TRUE exit then
   MR-LINE-ID$ id idu STR= ;

: MR-SELECT? ( ptr u8 n -- bool ) {: id:ptr idu :}
   MR-REQUIRE-HEADER
   begin MR-READ-LINE while
      MR-LINE$ BM-BLANK-OR-COMMENT? 0= if
         id idu MR-ID-MATCH? if
            MR-COPY-SELECTION
            MR-TRUE exit
         then
      then
   repeat
   MR-FALSE ;

: MR-REQUIRE ( ptr u8 n -- )
   MR-SELECT? 0= if E-BM-MODEL-NOT-FOUND throw then ;

: MR-COUNT ( -- n )
   0 MR-COUNT# !
   MR-REQUIRE-HEADER
   begin MR-READ-LINE while
      MR-LINE$ BM-BLANK-OR-COMMENT? 0= if
         MR-VALIDATE-LINE
         MR-COUNT# @ 1+ MR-COUNT# !
      then
   repeat
   MR-COUNT# @ ;

: MR-ID$ ( -- ptr u8 n )
   MR-ID-BUF MR-ID-U @ ;

: MR-LABEL$ ( -- ptr u8 n )
   MR-LABEL-BUF MR-LABEL-U @ ;

: MR-COMMAND$ ( -- ptr u8 n )
   MR-COMMAND-BUF MR-COMMAND-U @ ;

: MR-ARGS$ ( -- ptr u8 n )
   MR-ARGS-BUF MR-ARGS-U @ ;

: MR-PARSER$ ( -- ptr u8 n )
   MR-PARSER-BUF MR-PARSER-U @ ;

: MR-TOKEN-FIELDS$ ( -- ptr u8 n )
   MR-TOKEN-FIELDS-BUF MR-TOKEN-FIELDS-U @ ;

: MR-TIMEOUT$ ( -- ptr u8 n )
   MR-TIMEOUT-BUF MR-TIMEOUT-U @ ;

: MR-TIMEOUT ( -- n )
   MR-TIMEOUT# @ ;
