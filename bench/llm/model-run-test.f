\ model-run-test.f - focused tests for bench/llm/model-run.f.

variable MRT-HERE

JSON-STR-CAP 64 + constant MRT-RESP-CAP
120 constant MRT-RESP-X

create MRT-RESP-BUF MRT-RESP-CAP allot
variable MRT-RESP-U

: MRT-RESP-ROOM ( n -- ) {: add :}
   add 0 < if E-MRUN-CAPACITY throw then
   add MRT-RESP-CAP MRT-RESP-U @ - > if E-MRUN-CAPACITY throw then ;

: MRT-RESP-RESET ( -- )
   0 MRT-RESP-U ! ;

: MRT-RESP-C ( n -- ) {: c :}
   1 MRT-RESP-ROOM
   c MRT-RESP-BUF MRT-RESP-U @ + c!
   MRT-RESP-U @ 1+ MRT-RESP-U ! ;

: MRT-RESP+ ( ptr u8 n -- ) {: a:ptr u :}
   u MRT-RESP-ROOM
   a MRT-RESP-BUF MRT-RESP-U @ + u BYTE-COPY
   MRT-RESP-U @ u + MRT-RESP-U ! ;

: MRT-RESP$ ( -- ptr u8 n )
   MRT-RESP-BUF MRT-RESP-U @ ;

: MRT-RESP-S ( ptr u8 n -- )
   J-DQ MRT-RESP-C
   MRT-RESP+
   J-DQ MRT-RESP-C ;

: MRT-RESP-KEY ( ptr u8 n -- )
   MRT-RESP-S
   J-COLON MRT-RESP-C ;

: MRT-CLAUDE-OK$ ( -- ptr u8 n )
   MRT-RESP-RESET
   J-LBRACE MRT-RESP-C
   s" result" MRT-RESP-KEY s" ok" MRT-RESP-S
   J-COMMA MRT-RESP-C
   s" usage" MRT-RESP-KEY J-LBRACE MRT-RESP-C
   s" output_tokens" MRT-RESP-KEY s" 3" MRT-RESP+
   J-RBRACE MRT-RESP-C
   J-RBRACE MRT-RESP-C
   MRT-RESP$ ;

: MRT-CLAUDE-LARGE$ ( -- ptr u8 n )
   MRT-RESP-RESET
   J-LBRACE MRT-RESP-C
   s" result" MRT-RESP-KEY
   J-DQ MRT-RESP-C
   JSON-STR-CAP 1+ 0 ?do MRT-RESP-X MRT-RESP-C loop
   J-DQ MRT-RESP-C
   J-RBRACE MRT-RESP-C
   MRT-RESP$ ;

: MRT-REGISTRY$ ( -- ptr u8 n )
   s" id	label	command	args	parser	token_fields	timeout_s
prompt	Prompt	/bin/echo	{prompt}	raw		2
claude	Claude	/bin/echo	-p {prompt} --output-format json	raw		2
codex	Codex	/bin/echo	codex-exec {prompt}	raw		2
empty	Empty	/bin/echo		raw		2
slow	Slow	/bin/sleep	{prompt}	raw		1
bad	Bad	/bin/echo	--bad-template	raw		2
" ;

: MRT-RUN ( ptr u8 n ptr u8 n -- )
   {: id:ptr idu prompt:ptr promptu :}
   MRT-REGISTRY$ MR-REGISTRY!
   id idu MR-REQUIRE
   prompt promptu MRUN-RUN ;

: MRT-HERE-SNAPSHOT ( -- )
   here data-base - MRT-HERE ! ;

: MRT-HERE-UNCHANGED ( -- )
   here data-base - MRT-HERE @ T= ;

: MRT-TEST-BUFFERS ( -- )
   MRT-HERE-SNAPSHOT
   MRUN-RESET
   MRUN-OUT-CAP MRUN-OUT-NEED MEM-64K-SPAN-BYTES T=
   MRUN-ERR-CAP MRUN-ERR-NEED MEM-64K-SPAN-BYTES T=
   MRUN-TEXT-CAP MRUN-TEXT-NEED MEM-64K-SPAN-BYTES T=
   MRT-HERE-UNCHANGED ;

: MRT-BAD-TEMPLATE ( -- )
   s" bad" s" hello" MRT-RUN ;

: MRT-PARSE-LARGE-CLAUDE ( -- )
   MRT-CLAUDE-LARGE$ s" claude-json" s" usage.output_tokens" PR-PARSE-BUFFER ;

: MRT-TEST-PARSE-CLAUDE ( -- )
   MRT-CLAUDE-OK$ s" claude-json" s" usage.output_tokens" PR-PARSE-BUFFER
   PR-OUT$ s" ok" T$=
   PR-TOKEN-COUNT 3 T= ;

: MRT-TEST-PARSE-SYNTAX-FALLBACK ( -- )
   s" prose not json" s" claude-json" s" usage.output_tokens" PR-PARSE-BUFFER
   PR-OUT$ s" prose not json" T$=
   PR-TOKEN-COUNT 0 T= ;

: MRT-TEST-PARSE-TYPE-FALLBACK ( -- )
   s" []" s" claude-json" s" usage.output_tokens" PR-PARSE-BUFFER
   PR-OUT$ s" []" T$=
   PR-TOKEN-COUNT 0 T= ;

: MRT-MAIN ( -- )
   T-RESET
   MRT-TEST-BUFFERS
   MRT-TEST-PARSE-CLAUDE
   MRT-TEST-PARSE-SYNTAX-FALLBACK
   MRT-TEST-PARSE-TYPE-FALLBACK
   [: MRT-PARSE-LARGE-CLAUDE ;] E-JSON-CAPACITY TTHROWSQ
   s" prompt" s" hello" MRT-RUN
   MRUN-RC @ 0 T=
   MRUN-TEXT$ s" hello
" T$=
   MRUN-TOKENS @ 0 T=
   s" claude" s" hi" MRT-RUN
   MRUN-TEXT$ s" -p hi --output-format json
" T$=
   s" codex" s" task" MRT-RUN
   MRUN-TEXT$ s" exec --disable plugins --disable apps --disable multi_agent --disable tool_suggest --disable workspace_dependencies --skip-git-repo-check --ignore-rules --ignore-user-config --sandbox read-only --json task
" T$=
   s" empty" s" fallback" MRT-RUN
   MRUN-TEXT$ s" fallback
" T$=
   s" slow" s" 2" MRT-RUN
   MRUN-RC @ 137 T=
   MRUN-OUT$ nip 0 T=
   MRUN-ERR$ nip 0 T=
   [: MRT-BAD-TEMPLATE ;] E-MRUN-TEMPLATE TTHROWSQ
   T-REPORT
   s" model-run-test: ok" type cr ;

MRT-MAIN
