\ The public first-store path survives contention and a fresh snapshot process.
\ The image is saved from the keyed host with the saver already loaded
\ (test/app-image-engine.f).
require lib/test.f
require lib/fs-mutate.f
require lib/engine-candidate.f
require lib/codesign.f
require test/app-image-engine.f
require src/habu/address-cells.f
require src/habu/snapshot-format.f
package ADDRESS-CELL-TASK-TEST
using ADDRESS-CELLS
$10000 constant CAP
180000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
create ROOT-BUF FS-PATH-CAP allot
create IMAGE-BUF FS-PATH-CAP allot
variable ROOT-U
variable IMAGE-U
DYNAMIC-BUFFER IMAGE-STORAGE n
variable IMAGE-N
: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;
: BYTES ( -- ptr u8 ) 0 IMAGE-STORAGE BYTE-VIEW ;
: PREPARE ( -- )
   CLEANUP-RESET
   s" address-cell-tasks" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" restored" IMAGE-BUF JOIN-PATH IMAGE-U ! ;
: RESULT ( result<pcap:captured,pcap:failed> -- n n n )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N erru LEN>N 0 ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         outu LEN>N erru LEN>N rc RC>N ENDOF
   ;MATCH ;
: CLEAN ( n n n -- n ) {: outu:n erru:n rc:n :}
   rc 0<> erru 0<> or if OUT outu type ERR erru type then
   rc 0 T= erru 0 T= outu ;
: EXEC ( ptr u8 n ptr u8 n -- n )
   {: exe:ptr exeu:n input:ptr inputu:n :}
   exe exeu >LEN input inputu >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT CLEAN ;
: CHECK-OUTPUT ( n -- )
   OUT swap S\" test: ok\naddress-cell-tasks: ok\n" T$= ;
: FRESH ( -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$
   S\" require test/address-cell-tasks-subject.f\nADDRESS-CELL-TASKS:RUN\n"
   EXEC CHECK-OUTPUT ;
: BUILD ( -- )
   APP-IMAGE-ENGINE:PATH$ {: host:ptr hostu:n :}
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+ IMAGE$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   host hostu
   S\" 1 set-tier\nrequire test/address-cell-tasks-subject.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   EXEC 0 T=
   IMAGE$ EXECUTABLE? TTRUE ;
: CELL@ ( n -- n ) {: off:n :}
   0 8 0 ?do BYTES off i + + c@ i 8 * lshift or loop ;
: BAD-CELL-OFF ( -- )
   s" address-cell-tasks: DATA cell is not file backed" 74 die ;
: ELF-PH-CELL-OFF ( n n -- n ) {: ph:n va:n :}
   ph CELL@ $FFFFFFFF and 1 <> if -1 exit then
   ph 8 + CELL@ {: off:n :}
   ph 16 + CELL@ {: base:n :}
   ph 32 + CELL@ {: size:n :}
   off 0 < off IMAGE-N @ > or if -1 exit then
   size 0 < size IMAGE-N @ off - > or if -1 exit then
   va base < if -1 exit then
   va base - {: delta:n :}
   delta size > if -1 exit then
   CELL size delta - > if -1 exit then
   off delta + ;
\ The x86 saved image stores DATA in a fixed PT_LOAD, away from the trailer.
\ Resolve the actual cell VA through a file-backed segment before poisoning it.
: ELF-CELL-OFF ( n -- n ) {: cell:n :}
   IMAGE-N @ 64 < if BAD-CELL-OFF then
   0 CELL@ $FFFFFFFF and $464C457F <> if BAD-CELL-OFF then
   54 CELL@ $FFFF and 56 <> if BAD-CELL-OFF then
   32 CELL@ {: phoff:n :}
   56 CELL@ $FFFF and {: count:n :}
   phoff 64 < phoff IMAGE-N @ > or if BAD-CELL-OFF then
   count 0 <= count IMAGE-N @ phoff - 56 / > or if BAD-CELL-OFF then
   DATA-VA VA>N cell + {: va:n :}
   count 0 ?do
      phoff i 56 * + va ELF-PH-CELL-OFF
      dup -1 <> if unloop exit then drop
   loop BAD-CELL-OFF ;
: DATA-CELL-OFF ( n n -- n ) {: tr:n cell:n :}
   HB-TARGET-LINUX-X86-64? if cell ELF-CELL-OFF exit then
   tr tr SNAP-TRL-DATALEN + CELL@ - 8 + cell + ;
: POISON-CAPTURED-LOCK ( -- )
   IMAGE$ FILE-SIZE dup IMAGE-N ! 7 + CELL / IMAGE-STORAGE-RESERVE
   IMAGE$ BYTES IMAGE-N @ READ-ALL IMAGE-N @ T=
   IMAGE-TEXT-SIZE-OFF CELL@ IMAGE-TEXT-TRAILER-ADJ + SNAP-TRL-BYTES - {: tr:n :}
   tr SNAP-TRL-VERSION + CELL@ SNAPSHOT-FORMAT:VERSION T=
   tr LOCK-CELL DATA-CELL-OFF {: off:n :}
   off CELL@ 0 T=
   1 BYTES off + c!
   \ Process index pointers are never carried. A damaged incoming value must
   \ still be cleared without dereferencing it after the DATA copy.
   tr INDEX-CELL DATA-CELL-OFF {: index-off:n :}
   index-off CELL@ 0 T=
   1 BYTES index-off + c!
   IMAGE$ BYTES IMAGE-N @ WRITE-ALL
   IMAGE-STORAGE-RELEASE
   IMAGE$ CODESIGN:FORCE ;
: RESTORED ( -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   IMAGE$ S\" ADDRESS-CELL-TASKS:RUN\n" EXEC CHECK-OUTPUT ;
: CHECK ( -- )
   s" every first registration survives lookup, growth and duplicate contention" T-LABEL
   FRESH BUILD
   s" captured mutex and index cells do not retain process ownership" T-LABEL
   POISON-CAPTURED-LOCK RESTORED ;
: CLEANUP ( -- ) IMAGE-STORAGE-RELEASE CLEANUP-RUN ;
: RUN ( -- )
   T-RESET PREPARE
   [: CHECK ;] [: CLEANUP ;] finally
   T-REPORT ;
RUN
;using
;package
