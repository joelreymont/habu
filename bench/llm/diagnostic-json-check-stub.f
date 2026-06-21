\ diagnostic-json-check-stub.f - checker contract for JSON diagnostic parsing.
\
\ Load only in checker fixtures, before bench/llm/diagnostic-stats.f. Runtime
\ tests load tools/json.f instead. These bodies are deliberately simple and
\ checked; their stack effects model the JSON parser interface used by the
\ diagnostic reducer while the real parser's catch-based recovery remains a
\ separate boundary.

: JSONL-START-STRICT ( ptr u8 n -- )
   2drop ;

: JSONL-NEXT-OBJECT ( -- n )
   -1 ;

: JSON-GET ( n ptr u8 n -- n )
   2drop drop -1 ;
