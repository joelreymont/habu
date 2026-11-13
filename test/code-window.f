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
\ The uncaught fault's line up to its address; the address, " pc ", the pc
\ and a newline make 37 more bytes.
: FAULT-LINE$ ( -- ptr u8 n ) s" E-MEMORY-FAULT habu: memory fault at " ;

create OUT CAP allot
create ERR CAP allot

: CAPTURE ( ptr u8 n -- len len PROCESS:ending ) {: src:ptr u:n :}
   src u OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN ;

: EXPECT ( ptr u8 n n -- ) {: src:ptr u:n want:n :}
   src u CAPTURE {: outu:len erru:len oc :}
   src u OUT outu LEN>N ERR erru LEN>N oc want T-ENDING-EXITED= ;

\ arm64 throws the memory fault, uncaught: exit 110 after one line.
HB-TARGET-LINUX-X86-64? 0= [if]
: TRAPS ( ptr u8 n -- ) {: src:ptr u:n :}
   src u CAPTURE {: outu:len erru:len oc :}
   src u OUT outu LEN>N ERR erru LEN>N oc ENGINE-ERROR:MEMORY-FAULT
   T-ENDING-EXITED=
   ERR erru LEN>N FAULT-LINE$ STARTS-WITH? TTRUE
   erru LEN>N FAULT-LINE$ nip 37 + T= ;
[then]
\ x86-64 has no memory-fault throw: the crash handler dumps the registers and
\ the process dies by the SIGSEGV.
HB-TARGET-LINUX-X86-64? [if]
11 constant SIGSEGV
: TRAPS ( ptr u8 n -- ) {: src:ptr u:n :}
   src u CAPTURE {: outu:len erru:len oc :}
   src u OUT outu LEN>N ERR erru LEN>N oc SIGSEGV T-ENDING-SIGNALED= ;
[then]

: ACCEPTS ( ptr u8 n -- )  0 EXPECT ;

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

public

: RUN ( -- )
   T-RESET
   TEST-RX-VIEW
   T-REPORT
   s" code-window: ok" type cr ;

;package

CODE-WINDOW-TEST:RUN
