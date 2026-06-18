\ gate-json-assert.f - native JSON assertions for test/run.sh.
\ Load after tools/json.f.

0 set-check

$8000 constant GJA-IN-CAP
$1000 constant GJA-SRC-CAP
$400 constant GJA-PATH-CAP
$100 constant GJA-LINE-MAX
32 constant GJA-NUM-CAP

create GJA-IN GJA-IN-CAP allot
create GJA-SRC GJA-SRC-CAP allot
create GJA-PATH GJA-PATH-CAP allot
create GJA-NUM GJA-NUM-CAP allot
create GJA-LINE-A GJA-LINE-MAX cells allot
create GJA-LINE-U GJA-LINE-MAX cells allot

variable GJA-FD
variable GJA-RD
variable GJA-LEN
variable GJA-ROOT
variable GJA-NODE
variable GJA-ARR
variable GJA-ITEM
variable GJA-I
variable GJA-J
variable GJA-N
variable GJA-OK1
variable GJA-OK2
variable GJA-IDX
variable GJA-LINE
variable GJA-COL
variable GJA-LAST-NL
variable GJA-SRC-LEN
variable GJA-LINE#
variable GJA-LS
variable GJA-LX
variable GJA-SA
variable GJA-SU
variable GJA-NUM-I
variable GJA-A
variable GJA-U
variable GJA-B
variable GJA-V
variable GJA-WANT-A
variable GJA-WANT-U
variable GJA-SRC-PATH-A
variable GJA-SRC-PATH-U
variable GJA-STENCILS
variable GJA-PADDING
variable GJA-DIRECT

: GJA-NL ( -- ) 10 emit ;

: GJA-FAIL ( a u -- )
   s" gate-json: " type
   type
   GJA-NL
   1 throw ;

: GJA-USAGE ( -- )
   s" usage: tools/gate-json-assert.f MODE FILE [ARG]" GJA-FAIL ;

: GJA-COPY ( a dst u -- )
   GJA-U ! GJA-B ! GJA-A !
   0 GJA-J !
   begin GJA-J @ GJA-U @ < while
      GJA-A @ GJA-J @ + c@ GJA-B @ GJA-J @ + c!
      GJA-J @ 1+ GJA-J !
   repeat ;

: GJA-PATHZ ( a u -- z )
   GJA-U ! GJA-A !
   GJA-U @ 1+ GJA-PATH-CAP > IF s" path too long" GJA-FAIL THEN
   GJA-A @ GJA-PATH GJA-U @ GJA-COPY
   0 GJA-PATH GJA-U @ + c!
   GJA-PATH ;

: GJA-BYTES= ( a u b v -- f )
   GJA-V ! GJA-B ! GJA-U ! GJA-A !
   GJA-U @ GJA-V @ <> IF 0 exit THEN
   0 GJA-J !
   begin GJA-J @ GJA-U @ < while
      GJA-A @ GJA-J @ + c@ GJA-B @ GJA-J @ + c@ <> IF 0 exit THEN
      GJA-J @ 1+ GJA-J !
   repeat -1 ;

: GJA-LINE! ( a u k -- )
   GJA-N ! GJA-U ! GJA-A !
   GJA-A @ GJA-LINE-A GJA-N @ cells + !
   GJA-U @ GJA-LINE-U GJA-N @ cells + ! ;

: GJA-LINE$ ( k -- a u )
   dup cells GJA-LINE-A + @
   swap cells GJA-LINE-U + @ ;

: GJA-LINE+ ( a u -- )
   GJA-LINE# @ GJA-LINE-MAX >= IF s" too many JSON lines" GJA-FAIL THEN
   dup 0 > IF
      2dup + 1- c@ 13 = IF 1- THEN
   THEN
   GJA-LINE# @ GJA-LINE!
   GJA-LINE# @ 1+ GJA-LINE# ! ;

: GJA-SPLIT-LINES ( a u -- )
   GJA-SU ! GJA-SA !
   0 GJA-LINE# !
   0 GJA-LS !
   0 GJA-LX !
   begin GJA-LX @ GJA-SU @ < while
      GJA-SA @ GJA-LX @ + c@ 10 = IF
         GJA-SA @ GJA-LS @ + GJA-LX @ GJA-LS @ - GJA-LINE+
         GJA-LX @ 1+ GJA-LS !
      THEN
      GJA-LX @ 1+ GJA-LX !
   repeat
   GJA-LS @ GJA-SU @ < IF
      GJA-SA @ GJA-LS @ + GJA-SU @ GJA-LS @ - GJA-LINE+
   THEN ;

: GJA-U? ( a u -- n ok )
   GJA-U ! GJA-A !
   GJA-U @ 0= IF 0 0 exit THEN
   0 GJA-J !
   0
   begin GJA-J @ GJA-U @ < while
      GJA-A @ GJA-J @ + c@ dup 48 < over 57 > or IF drop drop 0 0 exit THEN
      48 - swap 10 * +
      GJA-J @ 1+ GJA-J !
   repeat -1 ;

: GJA-READ ( path-a path-u -- a u )
   GJA-PATHZ 0 0 open GJA-FD !
   GJA-FD @ 0 < IF s" cannot open JSON fixture" GJA-FAIL THEN
   0 GJA-LEN !
   begin
      GJA-LEN @ GJA-IN-CAP >= IF
         GJA-FD @ close
         s" JSON fixture too large" GJA-FAIL
      THEN
      GJA-FD @ GJA-IN GJA-LEN @ + GJA-IN-CAP GJA-LEN @ - read GJA-RD !
      GJA-RD @ 0 >
   while
      GJA-LEN @ GJA-RD @ + GJA-LEN !
   repeat
   GJA-RD @ 0 < IF
      GJA-FD @ close
      s" JSON fixture read failed" GJA-FAIL
   THEN
   GJA-FD @ close
   GJA-IN GJA-LEN @ ;

: GJA-READ-SRC ( path-a path-u -- )
   GJA-PATHZ 0 0 open GJA-FD !
   GJA-FD @ 0 < IF s" cannot open source fixture" GJA-FAIL THEN
   0 GJA-SRC-LEN !
   begin
      GJA-SRC-LEN @ GJA-SRC-CAP >= IF
         GJA-FD @ close
         s" source fixture too large" GJA-FAIL
      THEN
      GJA-FD @ GJA-SRC GJA-SRC-LEN @ + GJA-SRC-CAP GJA-SRC-LEN @ - read GJA-RD !
      GJA-RD @ 0 >
   while
      GJA-SRC-LEN @ GJA-RD @ + GJA-SRC-LEN !
   repeat
   GJA-RD @ 0 < IF
      GJA-FD @ close
      s" source fixture read failed" GJA-FAIL
   THEN
   GJA-FD @ close ;

: GJA-PARSE-FILE ( path-a path-u -- root )
   GJA-READ JSON-PARSE ;

: GJA-REQ ( root a u -- node )
   JSON-GET dup -1 = IF s" missing JSON field" GJA-FAIL THEN ;

: GJA-OBJ ( node -- )
   JSON-KIND J-OBJ <> IF s" expected JSON object" GJA-FAIL THEN ;

: GJA-ARR-KIND ( node -- )
   JSON-KIND J-ARR <> IF s" expected JSON array" GJA-FAIL THEN ;

: GJA-INT ( node -- n )
   dup JSON-KIND J-NUM <> IF drop s" expected JSON integer" GJA-FAIL THEN
   JSON-NUMBER$ GJA-U? 0= IF drop s" invalid JSON integer" GJA-FAIL THEN ;

: GJA-STR= ( node a u -- f )
   GJA-WANT-U ! GJA-WANT-A !
   dup JSON-KIND J-STR <> IF drop 0 exit THEN
   JSON-STRING$ GJA-WANT-A @ GJA-WANT-U @ GJA-BYTES= ;

: GJA-ASSERT-STR ( node a u -- )
   GJA-STR= 0= IF s" unexpected JSON string" GJA-FAIL THEN ;

: GJA-NONEMPTY-STR ( node -- )
   dup JSON-KIND J-STR <> IF drop s" expected JSON string" GJA-FAIL THEN
   JSON-STRING$ nip 0= IF s" expected nonempty JSON string" GJA-FAIL THEN ;

: GJA-REQ-STRF ( root a u -- )
   GJA-REQ dup JSON-KIND J-STR <> IF drop s" expected JSON string" GJA-FAIL THEN
   drop ;

: GJA-REQ-INTF ( root a u -- )
   GJA-REQ GJA-INT drop ;

: GJA-SCHEMA1 ( root -- )
   s" schema_version" GJA-REQ GJA-INT 1 <> IF s" schema_version is not 1" GJA-FAIL THEN ;

: GJA-LINE-STARTS-OBJECT ( a u -- )
   dup 0= IF 2drop s" empty JSON line" GJA-FAIL THEN
   over c@ 123 <> IF 2drop s" JSON line does not start with object" GJA-FAIL THEN
   2drop ;

: GJA-JSON-LINES-SCHEMA ( path-a path-u -- )
   GJA-READ GJA-SPLIT-LINES
   GJA-LINE# @ 0= IF s" no JSON lines" GJA-FAIL THEN
   0 GJA-I !
   begin GJA-I @ GJA-LINE# @ < while
      GJA-I @ GJA-LINE$ 2dup GJA-LINE-STARTS-OBJECT
      JSON-PARSE dup GJA-OBJ GJA-SCHEMA1
      GJA-I @ 1+ GJA-I !
   repeat ;

: GJA-JSON-ONE-SCHEMA ( path-a path-u -- )
   GJA-READ GJA-SPLIT-LINES
   GJA-LINE# @ 1 <> IF s" expected one JSON line" GJA-FAIL THEN
   0 GJA-LINE$ 2dup GJA-LINE-STARTS-OBJECT
   JSON-PARSE dup GJA-OBJ GJA-SCHEMA1 ;

: GJA-FIRST-JSON ( path-a path-u -- root )
   GJA-READ GJA-SPLIT-LINES
   GJA-LINE# @ 0= IF s" no JSON lines" GJA-FAIL THEN
   0 GJA-LINE$ 2dup GJA-LINE-STARTS-OBJECT
   JSON-PARSE dup GJA-OBJ ;

: GJA-SRC-LINE-COL ( idx -- )
   GJA-IDX !
   1 GJA-LINE !
   -1 GJA-LAST-NL !
   0 GJA-I !
   begin GJA-I @ GJA-IDX @ < while
      GJA-SRC GJA-I @ + c@ 10 = IF
         GJA-LINE @ 1+ GJA-LINE !
         GJA-I @ GJA-LAST-NL !
      THEN
      GJA-I @ 1+ GJA-I !
   repeat
   GJA-IDX @ GJA-LAST-NL @ - GJA-COL ! ;

: GJA-ASSERT-INT-FIELD ( root a u want -- )
   GJA-N ! GJA-REQ GJA-INT GJA-N @ <> IF s" unexpected JSON integer field" GJA-FAIL THEN ;

: GJA-DIAG-FILE-ORIGIN ( json-a json-u src-a src-u -- )
   2dup GJA-SRC-PATH-U ! GJA-SRC-PATH-A !
   GJA-READ-SRC
   -1 GJA-IDX !
   0 GJA-I !
   begin GJA-I @ 3 + GJA-SRC-LEN @ <= while
      GJA-SRC GJA-I @ + 3 s" dup" GJA-BYTES= IF
         GJA-I @ GJA-IDX !
         GJA-SRC-LEN @ GJA-I !
      THEN
      GJA-I @ 1+ GJA-I !
   repeat
   GJA-IDX @ 0 < IF s" cannot find dup in source fixture" GJA-FAIL THEN
   GJA-IDX @ GJA-SRC-LINE-COL
   GJA-FIRST-JSON GJA-ROOT !
   GJA-ROOT @ s" schema_version" 1 GJA-ASSERT-INT-FIELD
   GJA-ROOT @ s" file" GJA-REQ GJA-SRC-PATH-A @ GJA-SRC-PATH-U @ GJA-ASSERT-STR
   GJA-ROOT @ s" line" GJA-LINE @ GJA-ASSERT-INT-FIELD
   GJA-ROOT @ s" column" GJA-COL @ GJA-ASSERT-INT-FIELD
   GJA-ROOT @ s" byte_start" GJA-IDX @ GJA-ASSERT-INT-FIELD
   GJA-ROOT @ s" byte_end" GJA-IDX @ 3 + GJA-ASSERT-INT-FIELD ;

: GJA-SUGGEST-FOR ( class-a class-u -- hint-a hint-u )
   2dup s" remove_producer" GJA-BYTES= IF 2drop
      s" Remove an extra producer or drop the surplus value." exit
   THEN
   2dup s" add_producer" GJA-BYTES= IF 2drop
      s" Add the missing producer or stop consuming a required value." exit
   THEN
   2dup s" fix_type" GJA-BYTES= IF 2drop
      s" Change the body so produced types match the signature." exit
   THEN
   2dup s" fix_return_stack" GJA-BYTES= IF 2drop
      s" Balance return-stack transfers before the definition exits." exit
   THEN
   2dup s" trusted_boundary_required" GJA-BYTES= IF 2drop
      s" Move this compiler or runtime boundary behind audited TRUST." exit
   THEN
   2dup s" fix_signature_syntax" GJA-BYTES= IF 2drop
      s" Repair the stack-effect comment syntax, including --." exit
   THEN
   2dup s" rewrite_uncheckable" GJA-BYTES= IF 2drop
      s" Rewrite with modeled words or isolate an audited primitive." exit
   THEN
   2dup s" unknown_rejection" GJA-BYTES= IF 2drop
      s" Inspect the token, signature, and raw stack evidence." exit
   THEN
   2drop s" unknown repair class in suggestion assertion" GJA-FAIL ;

: GJA-DIAG-REPAIR-CLASS ( json-a json-u class-a class-u -- )
   GJA-WANT-U ! GJA-WANT-A !
   GJA-FIRST-JSON GJA-ROOT !
   GJA-ROOT @ s" repair_class" GJA-REQ
   GJA-WANT-A @ GJA-WANT-U @ GJA-ASSERT-STR
   GJA-ROOT @ s" suggestion" GJA-REQ dup GJA-NONEMPTY-STR
   GJA-WANT-A @ GJA-WANT-U @ GJA-SUGGEST-FOR GJA-ASSERT-STR ;

: GJA-DIAG-COMMON ( root -- )
   dup GJA-SCHEMA1
   dup s" code" GJA-REQ GJA-NONEMPTY-STR
   dup s" repair_class" GJA-REQ GJA-NONEMPTY-STR
   dup s" word" GJA-REQ GJA-NONEMPTY-STR
   dup s" token" GJA-REQ GJA-NONEMPTY-STR
   dup s" token_index" GJA-REQ-INTF
   dup s" file" GJA-REQ GJA-NONEMPTY-STR
   dup s" line" GJA-REQ-INTF
   dup s" column" GJA-REQ-INTF
   dup s" byte_start" GJA-REQ-INTF
   dup s" byte_end" GJA-REQ-INTF
   dup s" definition_source" GJA-REQ GJA-NONEMPTY-STR
   dup s" suggestion" GJA-REQ GJA-NONEMPTY-STR
   dup s" return_stack" GJA-REQ dup GJA-OBJ
   dup s" expected" GJA-REQ-STRF
   s" actual" GJA-REQ-STRF
   drop ;

: GJA-DIAG-DSTACK ( root -- )
   dup s" expected" GJA-REQ-STRF
   s" actual" GJA-REQ-STRF ;

: GJA-ALL-ROW0 ( root -- )
   dup GJA-DIAG-COMMON
   dup s" word" GJA-REQ s" bad1" GJA-ASSERT-STR
   dup s" code" GJA-REQ s" E-MISMATCH" GJA-ASSERT-STR
   dup s" repair_class" GJA-REQ s" remove_producer" GJA-ASSERT-STR
   GJA-DIAG-DSTACK ;

: GJA-ALL-ROW1 ( root -- )
   dup GJA-DIAG-COMMON
   dup s" word" GJA-REQ s" bad2" GJA-ASSERT-STR
   dup s" code" GJA-REQ s" E-REJECTED" GJA-ASSERT-STR
   dup s" repair_class" GJA-REQ s" fix_return_stack" GJA-ASSERT-STR
   s" return_stack" GJA-REQ s" actual" GJA-REQ GJA-NONEMPTY-STR ;

: GJA-ALL-ERRORS ( path-a path-u -- )
   GJA-READ GJA-SPLIT-LINES
   GJA-LINE# @ 2 <> IF s" expected two all-errors diagnostics" GJA-FAIL THEN
   0 GJA-LINE$ 2dup GJA-LINE-STARTS-OBJECT JSON-PARSE dup GJA-OBJ GJA-ALL-ROW0
   1 GJA-LINE$ 2dup GJA-LINE-STARTS-OBJECT JSON-PARSE dup GJA-OBJ GJA-ALL-ROW1 ;

: GJA-START-LINE-OK ( result -- )
   s" locations" GJA-REQ dup GJA-ARR-KIND
   0 JSON-ARR@
   s" physicalLocation" GJA-REQ
   s" region" GJA-REQ
   s" startLine" GJA-REQ GJA-INT 0 <= IF s" SARIF result missing startLine" GJA-FAIL THEN ;

: GJA-SARIF ( path-a path-u -- )
   GJA-PARSE-FILE GJA-ROOT !
   GJA-ROOT @ s" version" GJA-REQ s" 2.1.0" GJA-ASSERT-STR
   GJA-ROOT @ s" runs" GJA-REQ dup GJA-ARR-KIND 0 JSON-ARR@
   s" results" GJA-REQ dup GJA-ARR-KIND GJA-ARR !
   GJA-ARR @ JSON-COUNT 2 <> IF s" SARIF result count mismatch" GJA-FAIL THEN
   0 GJA-I !
   begin GJA-I @ GJA-ARR @ JSON-COUNT < while
      GJA-ARR @ GJA-I @ JSON-ARR@ GJA-START-LINE-OK
      GJA-I @ 1+ GJA-I !
   repeat ;

: GJA-CHECK-PUBLIC-ITEM ( item -- )
   GJA-ITEM !
   GJA-ITEM @ s" word" GJA-REQ s" SQUARE" GJA-STR= IF
      GJA-ITEM @ s" signature" GJA-REQ s" (i64 -- i64)" GJA-ASSERT-STR
      -1 GJA-OK1 !
      exit
   THEN
   GJA-ITEM @ s" word" GJA-REQ s" APPLY" GJA-STR= IF
      GJA-ITEM @ s" signature" GJA-REQ s" (i64 [ i64 -- i64 ] -- i64)" GJA-ASSERT-STR
      -1 GJA-OK2 !
      exit
   THEN ;

: GJA-PUBLIC-SIGNATURES ( path-a path-u -- )
   0 GJA-OK1 ! 0 GJA-OK2 !
   GJA-PARSE-FILE GJA-ROOT !
   GJA-ROOT @ GJA-SCHEMA1
   GJA-ROOT @ s" definitions" GJA-REQ dup GJA-ARR-KIND GJA-ARR !
   0 GJA-I !
   begin GJA-I @ GJA-ARR @ JSON-COUNT < while
      GJA-ARR @ GJA-I @ JSON-ARR@ GJA-CHECK-PUBLIC-ITEM
      GJA-I @ 1+ GJA-I !
   repeat
   GJA-OK1 @ 0= IF s" missing SQUARE public signature" GJA-FAIL THEN
   GJA-OK2 @ 0= IF s" missing APPLY public signature" GJA-FAIL THEN ;

: GJA-AOT-COMMON ( root -- )
   dup GJA-SCHEMA1
   dup s" file_bytes" GJA-REQ GJA-INT 0 <= IF s" AOT report file_bytes invalid" GJA-FAIL THEN
   dup s" patched_call_stencils" GJA-REQ GJA-INT GJA-STENCILS !
   dup s" padding_bytes" GJA-REQ GJA-INT GJA-PADDING !
   s" direct_bl_instructions" GJA-REQ GJA-INT GJA-DIRECT ! ;

: GJA-AOT-STRIPPED ( path-a path-u -- )
   GJA-PARSE-FILE GJA-AOT-COMMON
   GJA-PADDING @ GJA-STENCILS @ 12 * <> IF s" AOT padding relation mismatch" GJA-FAIL THEN
   GJA-STENCILS @ 0 <> IF s" AOT report found patched stencils" GJA-FAIL THEN
   GJA-DIRECT @ 0 <= IF s" AOT report missing direct BL" GJA-FAIL THEN ;

: GJA-AOT-COMPACT ( path-a path-u -- )
   GJA-PARSE-FILE GJA-AOT-COMMON
   GJA-STENCILS @ 0 <> IF s" compact AOT report found patched stencils" GJA-FAIL THEN
   GJA-PADDING @ 0 <> IF s" compact AOT report found padding" GJA-FAIL THEN
   GJA-DIRECT @ 3 < IF s" compact AOT report missing direct BLs" GJA-FAIL THEN ;

: GJA-DISPATCH ( -- )
   SCRIPT-ARGC 2 < IF GJA-USAGE THEN
   0 SCRIPT-ARGV$ s" json-lines-schema" GJA-BYTES= IF
      SCRIPT-ARGC 2 <> IF GJA-USAGE THEN
      1 SCRIPT-ARGV$ GJA-JSON-LINES-SCHEMA exit
   THEN
   0 SCRIPT-ARGV$ s" json-one-schema" GJA-BYTES= IF
      SCRIPT-ARGC 2 <> IF GJA-USAGE THEN
      1 SCRIPT-ARGV$ GJA-JSON-ONE-SCHEMA exit
   THEN
   0 SCRIPT-ARGV$ s" diag-file-origin" GJA-BYTES= IF
      SCRIPT-ARGC 3 <> IF GJA-USAGE THEN
      1 SCRIPT-ARGV$ 2 SCRIPT-ARGV$ GJA-DIAG-FILE-ORIGIN exit
   THEN
   0 SCRIPT-ARGV$ s" diag-repair-class" GJA-BYTES= IF
      SCRIPT-ARGC 3 <> IF GJA-USAGE THEN
      1 SCRIPT-ARGV$ 2 SCRIPT-ARGV$ GJA-DIAG-REPAIR-CLASS exit
   THEN
   0 SCRIPT-ARGV$ s" all-errors" GJA-BYTES= IF
      SCRIPT-ARGC 2 <> IF GJA-USAGE THEN
      1 SCRIPT-ARGV$ GJA-ALL-ERRORS exit
   THEN
   0 SCRIPT-ARGV$ s" sarif" GJA-BYTES= IF
      SCRIPT-ARGC 2 <> IF GJA-USAGE THEN
      1 SCRIPT-ARGV$ GJA-SARIF exit
   THEN
   0 SCRIPT-ARGV$ s" public-signatures" GJA-BYTES= IF
      SCRIPT-ARGC 2 <> IF GJA-USAGE THEN
      1 SCRIPT-ARGV$ GJA-PUBLIC-SIGNATURES exit
   THEN
   0 SCRIPT-ARGV$ s" aot-stripped" GJA-BYTES= IF
      SCRIPT-ARGC 2 <> IF GJA-USAGE THEN
      1 SCRIPT-ARGV$ GJA-AOT-STRIPPED exit
   THEN
   0 SCRIPT-ARGV$ s" aot-compact" GJA-BYTES= IF
      SCRIPT-ARGC 2 <> IF GJA-USAGE THEN
      1 SCRIPT-ARGV$ GJA-AOT-COMPACT exit
   THEN
   GJA-USAGE ;

GJA-DISPATCH
