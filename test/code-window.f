split-str
\ code-window.f - the code region's RX view is never writable. The region is
\ mapped twice (src/habu/habu2.f EM-MMAP-CODE-REGION): code runs from DBASE and
\ every store goes to the RW alias at DBASE plus REGION-ALIAS:DELTA-CELL. A
\ store that misses the alias faults in the build: `install --force` compiles
\ the prefix through every definer and patch path, the AOT seed and pass 2.
\ A raw store into the RX view faults wherever it lands: the code at and above
\ CP, the live code and dictionary below it, the control-flow stack. The read
\ control pins that the refusal is about writability, not an unmapped address.

require src/lib/error.f
require src/lib/test.f
require src/lib/test/ending.f
require src/lib/test/subject.f
require src/lib/process.f
require src/lib/string.f

package CODE-WINDOW-TEST

$800 constant CAP
\ The uncaught fault's record up to its address.
: FAULT-RECORD$ ( -- str )
   S\" error[E-MEMORY-FAULT]: a read or write faulted\n  = address: 0x" ;

create OUT CAP allot
create ERR CAP allot

: CAPTURE ( ^u8 n -- len len PROCESS:ending ) {: src:^ u:n :}
   src u OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN ;

: EXPECT ( ^u8 n n -- ) {: src:^ u:n want:n :}
   src u CAPTURE {: outu:len erru:len oc :}
   src u OUT outu LEN>N ERR erru LEN>N oc want T-ENDING-EXITED= ;

\ The memory fault is thrown, uncaught: exit 110 after its record.
: TRAPS ( ^u8 n -- ) {: src:^ u:n :}
   src u CAPTURE {: outu:len erru:len oc :}
   src u OUT outu LEN>N ERR erru LEN>N oc 110
   T-ENDING-EXITED=
   ERR erru LEN>N FAULT-RECORD$ STARTS-WITH? TTRUE ;

: ACCEPTS ( ^u8 n -- )  0 EXPECT ;

: TEST-RX-VIEW ( -- )
   s" a raw store above CP faults: the RX view is not writable" T-LABEL
   s" TRUSTED: CW-FAR ( -- ) 0 cp@ $40000 + ! ; CW-FAR" TRAPS
   s" TRUSTED: CW-PAGE ( -- ) 0 cp@ PROT-PAGE-MAX + ! ; CW-PAGE" TRAPS
   s" TRUSTED: CW-AT ( -- ) 0 cp@ ! ; CW-AT" TRAPS
   s" the same address reads, so the refusal is about writability" T-LABEL
   s" TRUSTED: CW-READ ( -- ) cp@ $40000 + @ drop ; CW-READ" ACCEPTS
   s" the live code and the dictionary below CP are not writable either" T-LABEL
   s" TRUSTED: CW-LIVE ( -- ) 0 dbase@ DICT-SIZE + ! ; CW-LIVE" TRAPS
   s" TRUSTED: CW-DICT ( -- ) 0 dbase@ ! ; CW-DICT" TRAPS
   s" nor is the control-flow stack" T-LABEL
   s" TRUSTED: CW-CFSTK ( -- ) 0 dbase@ CFSTK-OFF + ! ; CW-CFSTK" TRAPS ;

\ undefine writes a dictionary record through the RW alias: a child sharing
\ either mapping would hide CW-KEEP from the parent's next token.
: TEST-FORK-REGION ( -- )
   s" a forked child's dictionary writes leave the parent unchanged" T-LABEL
   S\" : CW-KEEP ( -- n ) 17 ;\n: CW-CHILD ( -- ) s\" undefine CW-KEEP\" evaluate-closed ;\n: CW-FORK ( -- ) PROC-FORK:CHECKED dup PID>N 0= if drop ['] CW-CHILD ERROR:CHILD-TOP then PID>N wait-status 0<> if 75 throw then ;\nCW-FORK CW-KEEP .\n"
   2dup CAPTURE {: outu:len erru:len oc :}
   OUT outu LEN>N ERR erru LEN>N oc 0 T-ENDING-EXITED=
   OUT outu LEN>N S\" 17\n" T$=
   erru LEN>N 0 T= ;

public

: RUN ( -- )
   T-RESET
   TEST-RX-VIEW
   TEST-FORK-REGION
   T-REPORT
   s" code-window: ok" type cr ;

;package

CODE-WINDOW-TEST:RUN
