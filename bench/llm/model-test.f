\ model-test.f - focused tests for bench/llm/model.f.

: MR-FIXTURE$ ( -- ptr u8 n )
   s" id	label	command	args	parser	token_fields	timeout_s
alpha	Alpha Model	/tmp/alpha	-p {prompt} --json	raw		5

# comment
codex	Codex	codex	codex-exec {prompt}	codex-jsonl	usage.output_tokens	300
" ;

: MR-BAD-HEADER$ ( -- ptr u8 n )
   s" id	label
alpha	Alpha
" ;

: MR-BAD-TIMEOUT$ ( -- ptr u8 n )
   s" id	label	command	args	parser	token_fields	timeout_s
bad	Bad	/tmp/bad	-p	raw		nope
" ;

: MR-BAD-MISSING$ ( -- ptr u8 n )
   s" id	label	command	args	parser	token_fields	timeout_s
bad	Bad		-p	raw		1
" ;

: MR-EXPECT-BAD-HEADER ( -- )
   MR-BAD-HEADER$ MR-REGISTRY!
   s" alpha" MR-REQUIRE ;

: MR-EXPECT-BAD-TIMEOUT ( -- )
   MR-BAD-TIMEOUT$ MR-REGISTRY!
   MR-COUNT drop ;

: MR-EXPECT-BAD-MISSING ( -- )
   MR-BAD-MISSING$ MR-REGISTRY!
   MR-COUNT drop ;

: MR-EXPECT-MISSING ( -- )
   MR-FIXTURE$ MR-REGISTRY!
   s" missing" MR-REQUIRE ;

: MR-MODEL-TEST-MAIN ( -- )
   T-RESET
   MR-FIXTURE$ MR-REGISTRY!
   MR-COUNT 2 T=
   s" alpha" MR-REQUIRE
   MR-ID$ s" alpha" T$=
   MR-LABEL$ s" Alpha Model" T$=
   MR-COMMAND$ s" /tmp/alpha" T$=
   MR-ARGS$ s" -p {prompt} --json" T$=
   MR-PARSER$ s" raw" T$=
   MR-TOKEN-FIELDS$ s" " T$=
   MR-TIMEOUT$ s" 5" T$=
   MR-TIMEOUT 5 T=
   s" codex" MR-REQUIRE
   MR-ID$ s" codex" T$=
   MR-COMMAND$ s" codex" T$=
   MR-PARSER$ s" codex-jsonl" T$=
   MR-TOKEN-FIELDS$ s" usage.output_tokens" T$=
   MR-TIMEOUT 300 T=
   MR-FIXTURE$ MR-REGISTRY!
   s" " MR-REQUIRE
   MR-ID$ s" alpha" T$=
   ['] MR-EXPECT-BAD-HEADER E-BM-SCHEMA TTHROWS
   ['] MR-EXPECT-BAD-TIMEOUT E-BM-FIELD TTHROWS
   ['] MR-EXPECT-BAD-MISSING E-BM-FIELD TTHROWS
   ['] MR-EXPECT-MISSING E-BM-MODEL-NOT-FOUND TTHROWS
   s" bench/llm/models.tsv" MR-LOAD
   MR-COUNT 2 T=
   s" claude" MR-REQUIRE
   MR-ID$ s" claude" T$=
   MR-PARSER$ s" claude-json" T$=
   MR-TIMEOUT 120 T=
   T-REPORT
   s" model-test: ok" type cr ;

MR-MODEL-TEST-MAIN
