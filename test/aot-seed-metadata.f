\ Mutated signed final images exercise the native packed-metadata boot reader.
\ The ordinary candidate boots first; every mutation must exit through the AOT
\ integrity boundary before the user program can print its marker.
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/codesign.f
require lib/engine-candidate.f
require tools/image-size-lib.f

package IMAGE-SIZE
private

: SEED-U64! ( n n -- ) {: v:n at:n :}
   at 8 ?RANGE
   8 0 ?do v i 8 * rshift $FF and IMG@ at i + + c! loop ;

: SEED-TABLE ( n -- n ) {: table:n :}
   table 0= if REC0 @ exit then
   table 1 = if SITE0 @ exit then
   DSITE0 @ ;

\ Keep framing and canonical ULEBs intact while violating the name/EXT
\ invariant the capture proves. This targets the native copy bound itself.
: SEED-LONG-META ( -- n )
   REC0 @ REC-PHYS @ PACK-BEGIN
   REC-N @ 0 ?do
      $FFFFFFFF PACK-V@ drop
      $FFFFFFFF PACK-V@ drop
      $FFFFFFFF PACK-V@ drop
      $FFFFFFFF PACK-V@ {: name:n :}
      PACK-CUR @ {: at:n :}
      $3FFF PACK-V@ {: meta:n :}
      NAMES0 @ name + U8@ DNAME-INL > meta 2 and 0<> and if
         at unloop exit
      then
   loop
   s" seed-metadata: candidate has no long dictionary name" 74 die ;

\ Advance a real pool entry by one byte without changing its ULEB width.
\ The offset remains in bounds but now names the entry's interior.
: SEED-INNER-NAME ( -- n )
   REC0 @ REC-PHYS @ PACK-BEGIN
   REC-N @ 0 ?do
      $FFFFFFFF PACK-V@ drop
      $FFFFFFFF PACK-V@ drop
      $FFFFFFFF PACK-V@ drop
      PACK-CUR @ {: at:n :}
      $FFFFFFFF PACK-V@ {: name:n :}
      $3FFF PACK-V@ drop
      NAMES0 @ name + U8@ 0 > name 127 and 127 < and if
         at unloop exit
      then
   loop
   s" seed-metadata: candidate has no interior name mutation" 74 die ;

\ The measured candidate's bytes. MEASURE reads and walks the whole engine, and
\ every mutation starts from the same unmodified file, so it runs once and each
\ mutation starts from this copy instead.
DYNAMIC-BUFFER PRISTINE u8

public

: SEED-KEEP ( -- )
   ILEN @ PRISTINE-RESERVE
   IMG@ 0 PRISTINE ILEN @ BYTE-COPY ;

: SEED-RESTORE ( -- ) 0 PRISTINE IMG@ ILEN @ BYTE-COPY ;

: SEED-MUTATE ( n n -- ) {: table:n mode:n :}
   mode 9 = if
      SEED-INNER-NAME {: name:n :}
      name U8@ 1+ IMG@ name + c! exit
   then
   mode 8 = if
      SEED-LONG-META {: meta:n :}
      meta U8@ $FD and IMG@ meta + c! exit
   then
   table SEED-TABLE {: at:n :}
   at 16 - U64@ $200000000 and 0<> TTRUE
   mode 0= if at 16 - U64@ $400000000 or at 16 - SEED-U64! exit then
   mode 1 = if 0 at 8 - SEED-U64! exit then
   mode 2 = if at 8 - U64@ 1- at 8 - SEED-U64! exit then
   mode 3 = if at 8 - U64@ 1+ at 8 - SEED-U64! exit then
   mode 4 = if $80 IMG@ at + c! 0 IMG@ at 1+ + c! exit then
   mode 5 = if
      5 0 ?do $80 IMG@ at i + + c! loop exit
   then
   mode 6 = if
      4 0 ?do $FF IMG@ at i + + c! loop
      $10 IMG@ at 4 + + c! exit
   then
   \ A negative first instruction gap underflows both site streams.
   table 1 = if 1 else 2 then IMG@ at + c! ;

: SEED-WRITE ( ptr u8 n -- ) IMG@ ILEN @ WRITE-ALL ;

;package

package AOT-SEED-METADATA

$1000 constant CAP
create OUT CAP allot create ERR CAP allot
create ROOT FS-PATH-CAP allot variable ROOT-U
create IMAGE FS-PATH-CAP allot variable IMAGE-U
variable OUT-U variable ERR-U variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

: RUN-IMAGE ( ptr u8 n -- )
   PROC-ARGV-RESET
   >LEN S\" .\" seed-metadata-user-ran\"" >LEN
   OUT CAP >LEN ERR CAP >LEN 10000 >MS
   RUN-ARGV-STDIN-CAPTURE
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
         o LEN>N OUT-U ! e LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
         o LEN>N OUT-U ! e LEN>N ERR-U ! c RC>N RC ! ENDOF
   ;MATCH ;

: MUTATION ( n n -- ) {: table:n mode:n :}
   IMAGE-SIZE:SEED-RESTORE
   table mode IMAGE-SIZE:SEED-MUTATE
   IMAGE$ IMAGE-SIZE:SEED-WRITE
   IMAGE$ CHMOD-X IMAGE$ CODESIGN:FORCE
   IMAGE$ RUN-IMAGE
   RC @ 82 <> if ERR ERR-U @ type cr then
   RC @ 82 T=
   ERR ERR-U @ s" hb: AOT" CONTAINS? TTRUE
   OUT OUT-U @ s" seed-metadata-user-ran" CONTAINS? TFALSE ;

: TABLE-MUTATIONS ( n -- ) {: table:n :}
   7 0 ?do table i MUTATION loop ;

: BODY ( -- )
   ENGINE-CANDIDATE:PATH$ RUN-IMAGE
   s" the unmodified candidate executes user source" T-LABEL
   RC @ 0 T= OUT OUT-U @ s" seed-metadata-user-ran" CONTAINS? TTRUE
   s" habu-seed-metadata" HB-TMP-MKDIR {: root:ptr rootu:n :}
   root ROOT rootu BYTE-COPY rootu ROOT-U ! ROOT$ CLEANUP-TREE+
   ROOT$ s" hb-corrupt" IMAGE JOIN-PATH IMAGE-U !
   ENGINE-CANDIDATE:PATH$ IMAGE-SIZE:MEASURE IMAGE-SIZE:SEED-KEEP
   3 0 ?do
      s" unknown tag, empty/truncated/trailing stream and invalid ULEBs refuse at boot" T-LABEL
      i TABLE-MUTATIONS
   loop
   s" signed site gaps cannot underflow the first offset" T-LABEL
   1 7 MUTATION 2 7 MUTATION
   s" a valid long pool name cannot be copied into an inline dictionary slot" T-LABEL
   0 8 MUTATION
   s" an in-bounds name offset must identify a pool entry" T-LABEL
   0 9 MUTATION ;

public
: RUN ( -- )
   T-RESET CLEANUP-RESET
   [: BODY ;] catch {: code:n :}
   CLEANUP-RUN code 0<> if code throw then
   T-REPORT s" aot-seed-metadata: ok" type cr ;
;package
AOT-SEED-METADATA:RUN
