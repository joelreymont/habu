\ diagnostic-json-check-stub.f - checker contract for JSON diagnostic parsing.
\
\ Load only in checker fixtures, before bench/llm/diagnostic-stats.f. Runtime
\ tests load tools/json.f instead. These bodies are deliberately simple and
\ checked; their stack effects model the JSON parser interface used by the
\ diagnostic reducer while the real parser's catch-based recovery remains a
\ separate boundary.

-7102 constant E-JSON-TYPE

3 constant J-STR
5 constant J-OBJ

0 constant JSONL-ROW-JSON
1 constant JSONL-ROW-BLANK
2 constant JSONL-ROW-ERROR
3 constant JSONL-ROW-EOF

: JSONL-STUB-FALSE ( -- bool )
   0 0= 0= ;

: JSONL-START-STRICT ( ptr u8 n -- )
   2drop ;

: JSONL-NEXT-ROW ( -- n n n bool )
   -1 JSONL-ROW-EOF 0 JSONL-STUB-FALSE ;

: JSONL-NEXT-OBJECT ( -- n )
   -1 ;

: JSON-GET ( n ptr u8 n -- n )
   2drop drop -1 ;

: JSON-KIND ( n -- n )
   drop J-STR ;

: JSON-STRING$ ( n -- ptr u8 n )
   drop s" stub" ;
