\ code-window.f - the code region's RX view is never writable. The region is
\ mapped twice (src/habu/habu2.f EM-MMAP-CODE-REGION): code runs from DBASE and
\ every store goes to the RW alias at DBASE plus REGION-ALIAS:DELTA-CELL. A
\ store that misses the alias faults in the build: `install --force` compiles
\ the prefix through every definer and patch path, the AOT seed and pass 2.
\ A raw store into the RX view dies in the crash handler wherever it lands: the
\ code at and above CP, the live code and dictionary below it, the control-flow
\ stack. The read control pins that the refusal is about writability, not an
\ unmapped address.

require src/lib/errors.f
require src/lib/test.f
require src/lib/test/outcome.f
require src/lib/test/subject.f
require src/lib/process.f

package CODE-WINDOW-TEST

$800 constant CAP
20000 constant TIMEOUT-MS
\ The engine's crash handler prints the register dump and then the process
\ dies by the fault's own signal (src/habu/crash.f C-CRASH-RERAISE). A store
\ into a mapped page without write permission is SIGBUS on macOS and SIGSEGV
\ on Linux.
: STORE-FAULT ( -- n )  HB-TARGET-LINUX? if 11 else 10 then ;

create OUT CAP allot
create ERR CAP allot

: CAPTURE ( ptr u8 n -- len len outcome ) {: src:ptr u:n :}
   src u OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN ;

: EXPECT ( ptr u8 n n -- ) {: src:ptr u:n want:n :}
   src u CAPTURE {: outu:len erru:len oc :}
   src u OUT outu LEN>N ERR erru LEN>N oc want T-OUTCOME-EXITED= ;

: TRAPS ( ptr u8 n -- ) {: src:ptr u:n :}
   src u CAPTURE {: outu:len erru:len oc :}
   src u OUT outu LEN>N ERR erru LEN>N oc STORE-FAULT T-OUTCOME-SIGNALED= ;

: ACCEPTS ( ptr u8 n -- )  0 EXPECT ;

: TEST-RX-VIEW ( -- )
   s" a raw store above CP dies: the RX view is not writable" T-LABEL
   s" TRUSTED: CW-FAR ( -- ) 0 cp@ $40000 + ! ; CW-FAR" TRAPS
   s" TRUSTED: CW-PAGE ( -- ) 0 cp@ PROT-PAGE-MAX + ! ; CW-PAGE" TRAPS
   s" TRUSTED: CW-AT ( -- ) 0 cp@ ! ; CW-AT" TRAPS
   s" the same address reads, so the refusal is about writability" T-LABEL
   s" TRUSTED: CW-READ ( -- ) cp@ $40000 + @ drop ; CW-READ" ACCEPTS
   s" the live code and the dictionary below CP are not writable there either" T-LABEL
   s" TRUSTED: CW-LIVE ( -- ) 0 dbase@ DICT-SIZE + ! ; CW-LIVE" TRAPS
   s" TRUSTED: CW-DICT ( -- ) 0 dbase@ ! ; CW-DICT" TRAPS
   s" nor is the control-flow stack" T-LABEL
   s" TRUSTED: CW-CFSTK ( -- ) 0 dbase@ CFSTK-OFF + ! ; CW-CFSTK" TRAPS ;

public

: RUN ( -- )
   T-RESET
   TEST-RX-VIEW
   T-REPORT
   s" code-window: ok" type cr ;

;package

CODE-WINDOW-TEST:RUN
