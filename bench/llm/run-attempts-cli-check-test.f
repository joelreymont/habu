\ run-attempts-cli-check-test.f - checker-safe CLI smoke.
\
\ Load after bench/llm/run-attempts.f with diagnostic-json-check-stub.f.

: RACTC-MAIN ( -- )
   RUNA-DEFAULT-RUN$ s" attempt-" STARTS-WITH? TTRUE
   s" out.jsonl" RUNA-LAST-SLASH -1 T=
   s" a/b/out.jsonl" RUNA-LAST-SLASH 3 T=
   s" run-attempts-cli-check-test: ok" type cr ;

RACTC-MAIN
