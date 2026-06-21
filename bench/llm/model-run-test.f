\ model-run-test.f - focused tests for bench/llm/model-run.f.

variable MRT-HERE

: MRT-REGISTRY$ ( -- ptr u8 n )
   s" id	label	command	args	parser	token_fields	timeout_s
prompt	Prompt	/bin/echo	{prompt}	raw		2
claude	Claude	/bin/echo	-p {prompt} --output-format json	raw		2
codex	Codex	/bin/echo	codex-exec {prompt}	raw		2
empty	Empty	/bin/echo		raw		2
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

: MRT-MAIN ( -- )
   T-RESET
   MRT-TEST-BUFFERS
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
   ['] MRT-BAD-TEMPLATE E-MRUN-TEMPLATE TTHROWS
   T-REPORT
   s" model-run-test: ok" type cr ;

MRT-MAIN
