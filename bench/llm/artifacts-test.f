\ artifacts-test.f - focused tests for bench/llm/artifacts.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\ lib/fs-mutate.f src/core/sha256.f bench/llm/artifacts.f
\ bench/llm/artifacts-test.f

create BAT-ROOT FS-PATH-CAP allot
create BAT-PROMPT FS-PATH-CAP allot
create BAT-RESPONSE FS-PATH-CAP allot
create BAT-CANDIDATE FS-PATH-CAP allot
create BAT-DIAGNOSTIC FS-PATH-CAP allot
create BAT-MISSING FS-PATH-CAP allot
create BAT-DIGEST 32 allot
create BAT-HEX BA-SHA-LEN allot

variable BAT-ROOT-U
variable BAT-PROMPT-U
variable BAT-RESPONSE-U
variable BAT-CANDIDATE-U
variable BAT-DIAGNOSTIC-U
variable BAT-MISSING-U

: BAT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: BAT-ROOT$ ( -- ptr u8 n )
   BAT-ROOT BAT-ROOT-U @ ;

: BAT-PROMPT$ ( -- ptr u8 n )
   BAT-PROMPT BAT-PROMPT-U @ ;

: BAT-RESPONSE$ ( -- ptr u8 n )
   BAT-RESPONSE BAT-RESPONSE-U @ ;

: BAT-CANDIDATE$ ( -- ptr u8 n )
   BAT-CANDIDATE BAT-CANDIDATE-U @ ;

: BAT-DIAGNOSTIC$ ( -- ptr u8 n )
   BAT-DIAGNOSTIC BAT-DIAGNOSTIC-U @ ;

: BAT-MISSING$ ( -- ptr u8 n )
   BAT-MISSING BAT-MISSING-U @ ;

: BAT-PROMPT-DATA$ ( -- ptr u8 n )
   s" prompt body" ;

: BAT-RESPONSE-DATA$ ( -- ptr u8 n )
   s" raw response" ;

: BAT-CANDIDATE-DATA$ ( -- ptr u8 n )
   s" : CANDIDATE ( i64 -- i64 ) 1 + ;" ;

: BAT-DIAGNOSTIC-DATA$ ( -- ptr u8 n )
   s" diagnostic: stack_mismatch" ;

: BAT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-bench-artifacts" TMPDIR-MKDIR BAT-ROOT BAT-ROOT-U BAT-COPY!
   BAT-ROOT$ CLEANUP-TREE+
   BAT-ROOT$ s" prompt.txt" BAT-PROMPT JOIN-PATH BAT-PROMPT-U !
   BAT-ROOT$ s" response.json" BAT-RESPONSE JOIN-PATH BAT-RESPONSE-U !
   BAT-ROOT$ s" candidate.f" BAT-CANDIDATE JOIN-PATH BAT-CANDIDATE-U !
   BAT-ROOT$ s" diagnostics.jsonl" BAT-DIAGNOSTIC JOIN-PATH BAT-DIAGNOSTIC-U !
   BAT-ROOT$ s" missing.txt" BAT-MISSING JOIN-PATH BAT-MISSING-U !
   BAT-PROMPT$ BAT-PROMPT-DATA$ WRITE-ALL
   BAT-RESPONSE$ BAT-RESPONSE-DATA$ WRITE-ALL
   BAT-CANDIDATE$ BAT-CANDIDATE-DATA$ WRITE-ALL
   BAT-DIAGNOSTIC$ BAT-DIAGNOSTIC-DATA$ WRITE-ALL ;

: BAT-SET-ARTIFACTS ( -- )
   BA-RESET
   BAT-PROMPT$ BAT-RESPONSE$ BAT-CANDIDATE$ BAT-DIAGNOSTIC$ BA-RECORD ;

: BAT-EXPECT-HASH ( ptr u8 n ptr u8 n -- ) {: body:ptr bodyu got:ptr gotu :}
   body bodyu BAT-DIGEST SHA256
   BAT-DIGEST BAT-HEX SHA256>HEX
   got gotu BAT-HEX BA-SHA-LEN T$= ;

: BAT-TEST-HASHES ( -- )
   BAT-SET-ARTIFACTS
   BA-HASH-ALL
   BAT-PROMPT-DATA$ BA-PROMPT-SHA$ BAT-EXPECT-HASH
   BAT-RESPONSE-DATA$ BA-RESPONSE-SHA$ BAT-EXPECT-HASH
   BAT-CANDIDATE-DATA$ BA-CANDIDATE-SHA$ BAT-EXPECT-HASH
   BAT-DIAGNOSTIC-DATA$ BA-DIAGNOSTIC-SHA$ BAT-EXPECT-HASH ;

: BAT-CONTAINS ( ptr u8 n ptr u8 n -- )
   CONTAINS? TTRUE ;

: BAT-ORDER ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: row:ptr rowu first:ptr firstu second:ptr secondu :}
   row rowu first firstu FIND-SUB {: a :}
   row rowu second secondu FIND-SUB {: b :}
   a 0 >= TTRUE
   b 0 >= TTRUE
   a b < TTRUE ;

: BAT-TEST-JSON ( -- )
   BAT-SET-ARTIFACTS
   BA-JSON$ {: row:ptr rowu :}
   row c@ BA-LBRACE T=
   row rowu 1- + c@ BA-RBRACE T=
   row rowu s" prompt_path" s" raw_response_path" BAT-ORDER
   row rowu s" raw_response_path" s" extracted_candidate_path" BAT-ORDER
   row rowu s" extracted_candidate_path" s" checker_diagnostics_path" BAT-ORDER
   row rowu BAT-PROMPT$ BAT-CONTAINS
   row rowu BAT-RESPONSE$ BAT-CONTAINS
   row rowu BAT-CANDIDATE$ BAT-CONTAINS
   row rowu BAT-DIAGNOSTIC$ BAT-CONTAINS
   row rowu BA-PROMPT-SHA$ BAT-CONTAINS
   row rowu BA-RESPONSE-SHA$ BAT-CONTAINS
   row rowu BA-CANDIDATE-SHA$ BAT-CONTAINS
   row rowu BA-DIAGNOSTIC-SHA$ BAT-CONTAINS ;

: BAT-MISSING-PROMPT ( -- )
   BA-RESET
   BAT-MISSING$ BAT-RESPONSE$ BAT-CANDIDATE$ BAT-DIAGNOSTIC$ BA-RECORD
   BA-HASH-ALL ;

: BAT-EMPTY-PROMPT ( -- )
   BA-RESET
   s" " BAT-RESPONSE$ BAT-CANDIDATE$ BAT-DIAGNOSTIC$ BA-RECORD ;

: BAT-TEST-MISSING ( -- )
   ['] BAT-MISSING-PROMPT E-BM-ARTIFACT-MISSING TTHROWS
   ['] BAT-EMPTY-PROMPT E-BM-ARTIFACT-MISSING TTHROWS ;

: BAT-MAIN ( -- )
   T-RESET
   BAT-PREPARE
   BAT-TEST-HASHES
   BAT-TEST-JSON
   BAT-TEST-MISSING
   CLEANUP-RUN
   T-REPORT
   s" artifacts-test: ok" type cr ;

BAT-MAIN
