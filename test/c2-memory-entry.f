\ Scoped C2 entries refuse a prompt callback before it runs; an ordinary one runs.
require src/lib/test.f
require src/lib/test/subject.f

package C2-MEMORY-ENTRY
private

: ORDINARY ( -- )
   s" an ordinary callback remains callable at the prompt" T-LABEL
   S\" : C2-ENTRY-INC ( n -- n ) 1 + ;\n: C2-ENTRY-APPLY ( n [ n -- n ] -- n ) execute ;\n41 ' C2-ENTRY-INC C2-ENTRY-APPLY\n"
   TEST-EVAL:N 42 T= ;

public

: RUN ( -- )
   T-RESET
   s" a mutable entry rejects a prompt callback" T-LABEL
   S\" package C2-ENTRY-TEST public TRUSTED: LEAKER ( ptr u8 n -- ptr u8 n ) ; ;package\n8 MEM:BYTES-ALLOC-LEN ' C2-ENTRY-TEST:LEAKER C2-MEM:WITH-MUT\n"
   70 s" hb: internal engine word: C2-MEM:WITH-MUT" SUBJECT:REFUSED? TTRUE
   s" an exported mutable entry rejects a prompt callback" T-LABEL
   S\" package C2-ENTRY-ALIAS public EXPORT C2-MEM:WITH-MUT ;package\npackage C2-ENTRY-TEST public TRUSTED: LEAKER ( ptr u8 n -- ptr u8 n ) ; ;package\n8 MEM:BYTES-ALLOC-LEN ' C2-ENTRY-TEST:LEAKER C2-ENTRY-ALIAS:WITH-MUT\n"
   70 s" hb: internal engine word: C2-ENTRY-ALIAS:WITH-MUT" SUBJECT:REFUSED? TTRUE
   s" a read entry rejects a prompt callback" T-LABEL
   S\" package C2-ENTRY-TEST public TRUSTED: LEAKER ( ptr u8 n -- ptr u8 n ) ; ;package\ns\q abc\q ' C2-ENTRY-TEST:LEAKER C2-MEM:WITH-READ\n"
   70 s" hb: internal engine word: C2-MEM:WITH-READ" SUBJECT:REFUSED? TTRUE
   s" a mutable loan entry rejects a prompt callback" T-LABEL
   S\" package C2-ENTRY-TEST public TRUSTED: LEAKER ( ptr u8 n -- ptr u8 n ) ; ;package\ns\q abc\q ' C2-ENTRY-TEST:LEAKER C2-MEM:WITH-MUT-LOAN\n"
   70 s" hb: internal engine word: C2-MEM:WITH-MUT-LOAN" SUBJECT:REFUSED? TTRUE
   s" the outer interpreter rejects a mutable entry" T-LABEL
   S\" require src/habu/interpret.f\ns\q 8 MEM:BYTES-ALLOC-LEN 0 C2-MEM:WITH-MUT\q OUTER:INTERPRET\n"
   70 s" hb: internal engine word: C2-MEM:WITH-MUT" SUBJECT:REFUSED? TTRUE
   s" a layout wrapper rejects a raw prompt callback" T-LABEL
   S\" require test/c2-memory-wrapper.f\n: C2-ENTRY-RAW ( n ptr u8 n -- n ptr u8 n ) s\q raw callback entered\q type cr ;\n0 ' C2-ENTRY-RAW C2-MEMORY-WRAPPER:TWICE .\n"
   70 s" hb: interpret-mode layout value: C2-MEMORY-WRAPPER:TWICE" SUBJECT:REFUSED? TTRUE
   s" an exported layout wrapper rejects a raw prompt callback" T-LABEL
   S\" require test/c2-memory-wrapper.f\npackage C2-ENTRY-ALIAS public EXPORT C2-MEMORY-WRAPPER:TWICE ;package\n: C2-ENTRY-RAW ( n ptr u8 n -- n ptr u8 n ) s\q raw callback entered\q type cr ;\n0 ' C2-ENTRY-RAW C2-ENTRY-ALIAS:TWICE .\n"
   70 s" hb: interpret-mode layout value: C2-ENTRY-ALIAS:TWICE" SUBJECT:REFUSED? TTRUE
   ORDINARY
   T-REPORT
   s" c2-memory-entry: ok" type cr ;

;package

C2-MEMORY-ENTRY:RUN
