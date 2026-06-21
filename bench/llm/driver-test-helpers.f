\ driver-test-helpers.f - shared checked helpers for live driver tests.
\
\ Load after lib/test.f, lib/json-write.f, bench/llm/live-row.f, and
\ bench/llm/drive-stdlib-lib.f.

: DTH-MODELS$ ( -- ptr u8 n )
   s" id	label	command	args	parser	token_fields	timeout_s
fixture	Fixture	/bin/echo	{prompt}	raw		2
" ;

: DTH-SRC-RESET ( -- )
   SB-RESET ;

: DTH-SRC+ ( ptr u8 n -- )
   SB-APPEND ;

: DTH-SRC-C ( n -- )
   SB-APPEND-C ;

: DTH-SRC-DQ ( -- )
   JW-DQ DTH-SRC-C ;

: DTH-SRC-SP ( -- )
   JW-SP DTH-SRC-C ;

: DTH-SRC-S" ( ptr u8 n -- ) {: a:ptr u :}
   s" s" DTH-SRC+
   DTH-SRC-DQ
   DTH-SRC-SP
   a u DTH-SRC+
   DTH-SRC-DQ ;

: DTH-SRC-TASK-HEAD ( -- )
   s" : " DTH-SRC+
   DS-NAME$ DTH-SRC+
   s"  ( " DTH-SRC+
   DS-SIG$ DTH-SRC+
   s"  ) " DTH-SRC+ ;

: DTH-SRC-END ( -- ptr u8 n )
   s" ;" DTH-SRC+
   SB$ ;

: DTH-ROW-HAS ( ptr u8 n -- )
   LR-ROW$ 2swap CONTAINS? TTRUE ;

: DTH-ROW-NEED-EMBEDDED-TEXT ( ptr u8 n -- ) {: a:ptr u :}
   JW-RESET
   0 begin dup u < while
      dup a + c@ JW-ESC-C
      1+
   repeat drop
   JW$ DTH-ROW-HAS ;

: DTH-ROW-NEED-EMBEDDED-FIELD-S ( ptr u8 n ptr u8 n -- )
   JW-RESET
   JW-FIELD-S
   SB-RESET
   JW$ SB-APPEND
   SB$ DTH-ROW-NEED-EMBEDDED-TEXT ;

: DTH-ROW-HAS-JSON ( -- )
   JW$ DTH-ROW-HAS ;

: DTH-ROW-HAS-KEY ( ptr u8 n -- )
   JW-RESET
   JW-KEY
   DTH-ROW-HAS-JSON ;

: DTH-ROW-HAS-S ( ptr u8 n ptr u8 n -- )
   JW-RESET
   JW-FIELD-S
   DTH-ROW-HAS-JSON ;

: DTH-ROW-NEED-KEY ( ptr u8 n -- )
   DTH-ROW-HAS-KEY ;

: DTH-ROW-NEED-S ( ptr u8 n ptr u8 n -- )
   DTH-ROW-HAS-S ;

: DTH-ROW-NEED-U ( ptr u8 n n -- )
   JW-RESET
   JW-FIELD-U
   DTH-ROW-HAS-JSON ;

: DTH-ROW-NEED-BOOL ( ptr u8 n bool -- )
   JW-RESET
   JW-FIELD-BOOL
   DTH-ROW-HAS-JSON ;

: DTH-ROW-NEED-NULL ( ptr u8 n -- )
   JW-RESET
   JW-FIELD-NULL
   DTH-ROW-HAS-JSON ;
