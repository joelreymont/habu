\ parse-resp.f - Habu-native model response parser.
\ Load after lib/errors.f, lib/string.f, lib/fs.f, tools/json.f, tools/argv.f.

0 set-check

$40000 constant PR-IN-CAP
$10000 constant PR-OUT-CAP
32 constant PR-NUM-CAP
76 constant PR-E-INTERNAL

create PR-IN PR-IN-CAP allot
create PR-OUT PR-OUT-CAP allot
create PR-NUM PR-NUM-CAP allot

variable PR-IN-U
variable PR-OUT-U
variable PR-TOKENS
variable PR-ROOT
variable PR-NODE
variable PR-TYPE-NODE
variable PR-ITEM
variable PR-NUM-I

: PR-CHECK-HOOK ( -- )
   CHECK! ;
' PR-CHECK-HOOK set-check

: PR-FAIL ( ptr u8 n -- )
   type cr PR-E-INTERNAL die ;

: PR-OUT-CHECK ( n -- )
   PR-OUT-CAP > IF s" parse-resp: output overflow" PR-FAIL THEN ;

: PR-OUT-RESET ( -- )
   0 PR-OUT-U ! ;

: PR-OUT-SET ( ptr u8 n -- ) {: a:ptr u :}
   u PR-OUT-CHECK
   a PR-OUT u BYTE-COPY
   u PR-OUT-U ! ;

: PR-OUT-APPEND ( ptr u8 n -- ) {: a:ptr u :}
   PR-OUT-U @ u + PR-OUT-CHECK
   a PR-OUT PR-OUT-U @ + u BYTE-COPY
   PR-OUT-U @ u + PR-OUT-U ! ;

: PR-GET ( n ptr u8 n -- n ) {: node key:ptr ku :}
   node 0 < IF -1 exit THEN
   node JSON-KIND J-OBJ <> IF -1 exit THEN
   node key ku JSON-GET ;

: PR-NODE>NUMBER ( n -- n ) {: node :}
   node 0 < IF 0 exit THEN
   node JSON-KIND J-NUM <> IF 0 exit THEN
   node JSON-NUMBER$ STR>NUMBER? 0= IF drop 0 exit THEN ;

: PR-GET2-NUM ( n ptr u8 n ptr u8 n -- n ) {: root a:ptr u b:ptr v :}
   root a u PR-GET PR-NODE !
   PR-NODE @ b v PR-GET PR-NODE>NUMBER ;

: PR-TF$ ( -- ptr u8 n )
   ARGV-POS# 3 > IF 3 ARGV-POS$ ELSE s" usage.output_tokens,modelUsage.*.outputTokens" THEN ;

: PR-TOKENS+ ( n -- )
   PR-TOKENS @ + PR-TOKENS ! ;

: PR-MODELUSAGE-ITEM ( n n -- n ) {: obj idx :}
   obj idx JSON-OBJ@ PR-ITEM ! 2drop
   PR-ITEM @ s" outputTokens" PR-GET PR-NODE>NUMBER ;

: PR-MODELUSAGE-TOKENS ( n -- n ) {: root :}
   root s" modelUsage" PR-GET PR-NODE !
   PR-NODE @ 0 < IF 0 exit THEN
   PR-NODE @ JSON-KIND J-OBJ <> IF 0 exit THEN
   0 0 begin dup PR-NODE @ JSON-COUNT < while
      PR-NODE @ over PR-MODELUSAGE-ITEM rot + swap
      1+
   repeat drop ;

: PR-ADD-TOKEN-FIELDS ( n -- ) {: root :}
   PR-TF$ s" usage.output_tokens" CONTAINS? IF
      root s" usage" s" output_tokens" PR-GET2-NUM PR-TOKENS+
   THEN
   PR-TF$ s" usage.completion_tokens" CONTAINS? IF
      root s" usage" s" completion_tokens" PR-GET2-NUM PR-TOKENS+
   THEN
   PR-TF$ s" modelUsage.*.outputTokens" CONTAINS? IF
      root PR-MODELUSAGE-TOKENS PR-TOKENS+
   THEN ;

: PR-MAYBE-SET-STRING ( n -- bool ) {: node :}
   node 0 < IF 0 exit THEN
   node JSON-KIND J-STR <> IF 0 exit THEN
   node JSON-STRING$ PR-OUT-SET
   -1 ;

: PR-MAYBE-APPEND-TEXT ( n -- )
   s" text" PR-GET
   dup 0 < IF drop exit THEN
   dup JSON-KIND J-STR <> IF drop exit THEN
   JSON-STRING$ PR-OUT-APPEND ;

: PR-CLAUDE-CONTENT ( n -- ) {: root :}
   root s" content" PR-GET PR-NODE !
   PR-NODE @ 0 < IF exit THEN
   PR-NODE @ JSON-KIND J-ARR <> IF exit THEN
   0 begin dup PR-NODE @ JSON-COUNT < while
      PR-NODE @ over JSON-ARR@ PR-MAYBE-APPEND-TEXT
      1+
   repeat drop ;

: PR-PARSE-CLAUDE ( -- )
   PR-IN PR-IN-U @ JSON-PARSE PR-ROOT !
   PR-ROOT @ PR-ADD-TOKEN-FIELDS
   PR-ROOT @ s" result" PR-GET PR-MAYBE-SET-STRING IF exit THEN
   PR-CLAUDE-CONTENT ;

: PR-PARSE-OPENAI-CHOICE ( n -- ) {: choices :}
   choices 0 < IF exit THEN
   choices JSON-KIND J-ARR <> IF exit THEN
   choices JSON-COUNT 0= IF exit THEN
   choices 0 JSON-ARR@ s" message" PR-GET PR-NODE !
   PR-NODE @ s" content" PR-GET PR-MAYBE-SET-STRING drop ;

: PR-PARSE-OPENAI ( -- )
   PR-IN PR-IN-U @ JSON-PARSE PR-ROOT !
   PR-ROOT @ PR-ADD-TOKEN-FIELDS
   PR-ROOT @ s" output_text" PR-GET PR-MAYBE-SET-STRING IF exit THEN
   PR-ROOT @ s" choices" PR-GET PR-PARSE-OPENAI-CHOICE ;

: PR-AGENT-MESSAGE$? ( ptr u8 i64 -- bool )
   s" agent_message" STR= ;

: PR-CODEX-AGENT-MESSAGE? ( n -- bool ) {: item :}
   item s" type" PR-GET PR-TYPE-NODE !
   PR-TYPE-NODE @ 0 < IF 0 exit THEN
   PR-TYPE-NODE @ JSON-KIND J-STR <> IF 0 exit THEN
   PR-TYPE-NODE @ JSON-STRING$ PR-AGENT-MESSAGE$? ;

: PR-CODEX-EVENT ( n -- ) {: root :}
   root PR-ADD-TOKEN-FIELDS
   root s" item" PR-GET PR-NODE !
   PR-NODE @ PR-CODEX-AGENT-MESSAGE? 0= IF exit THEN
   PR-NODE @ s" text" PR-GET PR-MAYBE-SET-STRING drop ;

: PR-PARSE-CODEX ( -- )
   PR-IN PR-IN-U @ JSONL-START
   begin JSONL-NEXT-OBJECT dup 0 >= while
      PR-CODEX-EVENT
   repeat drop ;

: PR-PARSE-RAW ( -- )
   PR-IN PR-IN-U @ PR-OUT-SET ;

: PR-PARSER$ ( -- ptr u8 n )
   ARGV-POS# 2 > IF 2 ARGV-POS$ ELSE s" claude-json" THEN ;

: PR-PARSE-RESP-STRICT ( -- )
   PR-PARSER$ s" raw" STR= IF PR-PARSE-RAW exit THEN
   PR-PARSER$ s" claude-json" STR= IF PR-PARSE-CLAUDE exit THEN
   PR-PARSER$ s" openai-json" STR= IF PR-PARSE-OPENAI exit THEN
   PR-PARSER$ s" codex-jsonl" STR= IF PR-PARSE-CODEX exit THEN
   s" parse-resp: unsupported parser" PR-FAIL ;

: PR-PARSE-RESP ( -- )
   ['] PR-PARSE-RESP-STRICT catch
   dup 0= IF drop exit THEN
   drop PR-PARSE-RAW ;

: PR-U$ ( u -- ptr u8 n ) {: u :}
   PR-NUM-CAP PR-NUM-I !
   u 0= IF
      PR-NUM-I @ 1- PR-NUM-I !
      STR-ZERO PR-NUM PR-NUM-I @ + c!
      PR-NUM PR-NUM-I @ + 1
      exit
   THEN
   u begin dup 0 > while
      dup STR-BASE mod STR-ZERO +
      PR-NUM-I @ 1- PR-NUM-I !
      PR-NUM PR-NUM-I @ + c!
      STR-BASE /
   repeat drop
   PR-NUM PR-NUM-I @ + PR-NUM-CAP PR-NUM-I @ - ;

: PR-USAGE ( -- )
   s" bench/llm/parse-resp.f RESP OUT [parser] [token-fields]" ARGV-USAGE! ;

: PARSE-RESP-MAIN ( -- )
   PR-USAGE
   ARGV-PARSE
   2 4 ARGV-EXPECT-POS
   0 PR-TOKENS !
   PR-OUT-RESET
   0 ARGV-POS$ PR-IN PR-IN-CAP READ-ALL PR-IN-U !
   PR-PARSE-RESP
   PR-OUT-U @ 0= IF PR-PARSE-RAW THEN
   1 ARGV-POS$ PR-OUT PR-OUT-U @ WRITE-ALL
   PR-TOKENS @ PR-U$ type ;

PARSE-RESP-MAIN
