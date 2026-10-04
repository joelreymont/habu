\ Application-image writer behavior through APP-IMAGE:SAVE: transient return
\ frames are cleared, protected namespaces survive restore, corrupt images
\ fail closed, an output that cannot be opened is refused by name and creates
\ nothing, a failed final close is reported and leaves the output path as it
\ was, and a capture with a definition or quotation open is refused.
require lib/test.f
require src/habu/address-cells.f
require src/habu/snapshot-format.f
require src/habu/cell-grid.f
require lib/fmt.f
require lib/memory.f
require src/habu/stack-abi.f
require test/snapshot-writer-poison-canaries.f
require lib/fs-mutate.f
require lib/process-env.f
require lib/engine-candidate.f
require lib/codesign.f
require test/app-image-engine.f
require lib/test/outcome.f

package SNAP-WRITER-TEST

$8000 constant CAP
240000 constant TIMEOUT-MS
74 constant OUTPUT-FAIL-RC       \ the writer cannot open, close or replace its output
79 constant SNAP-BAD-RC          \ EM-SNAPSHOT-RESTORE's corrupt-image exit
ENGINE-ERROR:SEAL-PACKAGE constant FORGE-RC
create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U
variable RC
variable EXITED

: ENGINE$ ( -- ptr u8 n ) ENGINE-CANDIDATE:PATH$ ;

\ ---- isolated tmp root ----
create ROOT-BUF FS-PATH-CAP allot
variable ROOT-U

: ROOT ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY
   u ROOT-U ! ;

: SETUP-ROOT ( -- )
   s" habu-snapshot-writer" HB-TMP-MKDIR ROOT!
   ROOT CLEANUP-TREE+ ;

create PATH-BUF FS-PATH-CAP allot
: PATH$ ( ptr u8 n -- ptr u8 n )
   {: name:ptr size:n :}
   ROOT name size PATH-BUF JOIN-PATH PATH-BUF swap ;
: SNAP0$ ( -- ptr u8 n ) s" application" PATH$ ;
: SNAP-SRC$ ( -- ptr u8 n ) s" probe.f" PATH$ ;
: BAD-SNAP$ ( -- ptr u8 n ) s" bad-locator" PATH$ ;
: BAD-BAND$ ( -- ptr u8 n ) s" bad-registry" PATH$ ;
create SHADOW-SRC-BUF FS-PATH-CAP allot
create SHADOW-SNAP-BUF FS-PATH-CAP allot
: SHADOW-SRC$ ( -- ptr u8 n )
   ROOT s" shadow.f" SHADOW-SRC-BUF JOIN-PATH SHADOW-SRC-BUF swap ;
: SHADOW-SNAP$ ( -- ptr u8 n )
   ROOT s" shadow-snapshot" SHADOW-SNAP-BUF JOIN-PATH SHADOW-SNAP-BUF swap ;

\ ---- snapshot image reader ----
PTR-VARIABLE IMGP
variable IMGU

: IMG ( -- ptr u8 )
   IMGP @ ;

: LOAD-IMAGE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u FILE-SIZE {: sz:n :}
   sz MEM-ALLOC-BYTES drop IMGP !
   a u IMG sz READ-ALL IMGU !
   IMGU @ sz <> if s" snapshot writer short read" 74 die then ;

: U64@ ( n -- n ) {: k:n :}
   0
   8 0 ?do
      IMG k i + + c@ i 8 * lshift or
   loop ;

: U8@ ( n -- n ) {: k:n :}
   IMG k + c@ ;

: U8! ( n n -- ) {: k:n v:n :}
   v IMG k + c! ;

\ The trailer sits at the end of the authenticated text extent; src/habu/layout.f
\ owns its size and field offsets, and the target header owns its location.
: TRAILER-OFF ( -- n )
   IMAGE-TEXT-SIZE-OFF U64@ IMAGE-TEXT-TRAILER-ADJ + SNAP-TRL-BYTES - ;

: DATA-OFF ( -- n )
   TRAILER-OFF {: tr:n :}
   tr tr SNAP-TRL-DATALEN + U64@ - ;

\ The return and loop stacks are guarded mappings outside DATA, so the image
\ carries no return-stack window at all: their base cells must be zero (each
\ run maps its own), and the values the poison fixture planted in the live
\ return stack (the canary constants inverted) must not appear anywhere in the
\ persisted DATA payload.
: STACK-BASES-ZERO? ( -- bool )
   DATA-OFF 8 + STACK-ABI:RETURN-BASE-CELL + U64@ 0=
   DATA-OFF 8 + STACK-ABI:LOOP-BASE-CELL + U64@ 0= and ;

: DATA-LEN ( -- n )
   TRAILER-OFF SNAP-TRL-DATALEN + U64@ ;

\ The trailer retains virtual region length while the file stores only live
\ dictionary rows and code. The DATA stream starts with its own virtual extent.
: REGION-LEN ( -- n )
   TRAILER-OFF SNAP-TRL-REGLEN + U64@ ;

: DICT-ROWS ( -- n )
   TRAILER-OFF SNAP-TRL-NDICT + U64@ DREC * ;

: STORED-REGION-LEN ( -- n )
   DICT-ROWS REGION-LEN DICT-SIZE - + ;

: REGION-OFF ( -- n )
   DATA-OFF STORED-REGION-LEN - ;

: MAP-SLICE-LEN ( -- n )
   REGION-LEN 31 + 32 / DICT-SIZE 32 / - ;

: DATA-HEADER-OFF ( -- n )
   DATA-OFF 8 + SNAP-RELOC:CALLMAP-OFF + MAP-SLICE-LEN 2 * + ;

: HEAP-OFF ( -- n )
   DATA-HEADER-OFF ADDRESS-CELLS:HEADER-BYTES +
   DATA-HEADER-OFF ADDRESS-CELLS:BASE-FIELD + U64@
      ADDRESS-CELLS:BOOT-OFF = if
      DATA-HEADER-OFF U64@ 8 * +
   then ;

\ ---- the heap section: its bytes, or the src/habu/cell-grid.f form ----------
: HEAP-BYTES ( -- n ) DATA-OFF U64@ DATA-START - ;
: HEAP-FORM ( -- n ) TRAILER-OFF SNAPSHOT-FORMAT:HEAP-FIELD + U64@ ;
: GRID? ( -- bool ) HEAP-FORM SNAPSHOT-FORMAT:HEAP-GRID = ;
: GRID-G ( -- n ) HEAP-OFF U64@ ;
: GRID-S ( -- n ) HEAP-OFF 8 + U64@ ;
: MAP-OFF ( -- n ) HEAP-OFF SNAPSHOT-FORMAT:GRID-FRAME + ;
: GROUPS-OFF ( -- n ) MAP-OFF GRID-G CELL-GRID:PMAP-BYTES + ;
: VALUES-OFF ( -- n ) GROUPS-OFF GRID-S + ;

: BITS ( n -- n ) {: b:n :}
   0  8 0 ?do  b i rshift 1 and +  loop ;

: PRESENT-CELLS ( -- n )
   0  GRID-S 0 ?do  GROUPS-OFF i + U8@ BITS +  loop ;

\ The width of the value at image offset `off`; the writer never stores a
\ malformed one.
: WIDTH ( n -- n ) {: off:n :}
   IMG off + TRAILER-OFF off - CELL-GRID:CELL-V@ nip {: w:n :}
   w 0= if s" snapshot writer: malformed grid value" 74 die then
   w ;

\ The offset of the first value `want` bytes wide, or -1.
: VALUE-AT ( n -- n ) {: want:n :}
   VALUES-OFF  PRESENT-CELLS 0 ?do
      dup WIDTH want = if unloop exit then
      dup WIDTH +
   loop
   drop -1 ;

: LAST-VALUE ( -- n )
   VALUES-OFF dup  PRESENT-CELLS 0 ?do  nip dup dup WIDTH +  loop drop ;

: VALUES-END ( -- n )
   VALUES-OFF  PRESENT-CELLS 0 ?do  dup WIDTH +  loop ;

\ Where the zero pad begins: after the heap's bytes, or after the grid's values.
: STREAM-END ( -- n )
   GRID? if VALUES-END exit then
   HEAP-OFF HEAP-BYTES + ;

: STREAM-SHAPE-CASE ( -- )
   s" the snapshot stores live dictionary rows and preserves the virtual code extent" T-LABEL
   TRAILER-OFF SNAP-TRL-VERSION + U64@ SNAPSHOT-FORMAT:VERSION T=
   REGION-LEN DICT-SIZE >= TTRUE
   DICT-ROWS DICT-SIZE <= TTRUE
   REGION-OFF DATA-OFF < TTRUE
   s" its DATA begins with the exact restored DP extent" T-LABEL
   DATA-OFF U64@ DATA-START >= TTRUE
   DATA-OFF U64@ DATA-SIZE <= TTRUE
   s" it carries the address-vector header after the live map slices" T-LABEL
   DATA-HEADER-OFF ADDRESS-CELLS:MAGIC-FIELD + U64@ ADDRESS-CELLS:MAGIC T= ;

: CANARIES-ABSENT? ( -- bool )
   DATA-OFF DATA-LEN + 8 - DATA-OFF ?do
      i U64@ {: v:n :}
      v SNAP-WRITER-POISON:LO-CANARY invert = v SNAP-WRITER-POISON:HI-CANARY invert = or if
         false unloop exit
      then
   8 +loop true ;

\ ---- child snapshot build with rc + stderr capture ----
: CAPTURE! ( result<pcap:captured,pcap:failed> -- )
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

: SAVE-LINE$ ( -- ptr u8 n ) s\" 0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" ;

\ The fixture is loaded on the keyed host with the saver already loaded
\ (test/app-image-engine.f), which starts at tier 0 where app-image.f set tier 1.
\ `rest` follows on stdin, where a capture must run: one inside a load is
\ refused.
: BUILD-RUNNING-TO ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: target:ptr targetu:n fixture:ptr size:n rest:ptr restu:n :}
   APP-IMAGE-ENGINE:PATH$ {: host:ptr hostu:n :}
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   target targetu >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   SB-RESET
   s\" 1 set-tier\nrequire " SB-APPEND
   fixture size SB-APPEND  s\" \n" SB-APPEND
   rest restu SB-APPEND
   host hostu >LEN SB$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE CAPTURE! ;

: BUILD-WITH-TO ( ptr u8 n ptr u8 n -- )
   {: target:ptr targetu:n fixture:ptr size:n :}
   target targetu fixture size SAVE-LINE$ BUILD-RUNNING-TO ;

: BUILD-WITH ( ptr u8 n -- ) {: fixture:ptr size:n :}
   SNAP0$ fixture size BUILD-WITH-TO ;

: ERR$ ( -- ptr u8 n )
   ERR ERR-U @ ;

: IMG-ARGV ( ptr u8 n -- ) {: path:ptr pathu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/imgdump.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" --snap" >LEN PROC-ARGV+
   path pathu >LEN PROC-ARGV+ ;

: IMG-CAPTURE ( ptr u8 n -- )
   IMG-ARGV
   ENGINE$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE CAPTURE! ;

: ASSERT-SNAPSHOT ( ptr u8 n -- )
   IMG-CAPTURE
   RC @ 0 T=
   ERR-U @ 0 T=
   OUT OUT-U @ s" ndict " CONTAINS? TTRUE ;

: ASSERT-NO-SNAPSHOT ( ptr u8 n -- )
   IMG-CAPTURE
   RC @ 0 T=
   ERR-U @ 0 T=
   OUT OUT-U @ TRIM s" no-snapshot" STR= TTRUE ;

: WRITE-BAD-SNAPSHOT ( -- )
   8 0 ?do
      0 IMG IMAGE-TEXT-SIZE-OFF i + + c!
   loop
   BAD-SNAP$ IMG IMGU @ WRITE-ALL ;

\ ---- warm snapshot probes ----
: STORE! ( len len outcome ptr u8 n -- )
   {: outu:len erru:len oc src:ptr srcu:n :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   oc MATCH outcome
     exited OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout OF src srcu OUT OUT-U @ ERR$ T-TIMED-OUT ENDOF
   ;MATCH ;

: WARM-LOAD ( ptr u8 n -- ) {: s:ptr su:n :}
   SNAP-SRC$ s su WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   SNAP-SRC$ >LEN PROC-ARGV+
   SNAP0$ >LEN  OUT 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME s su STORE! ;

: WARM-STDIN ( ptr u8 n -- ) {: s:ptr su:n :}
   PROC-ARGV-RESET
   SNAP0$ >LEN  s su >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME s su STORE! ;

: PARSE-OUT ( -- n )
   OUT OUT-U @ TRIM STR>NUMBER? MATCH option
     some OF ENDOF
     none OF T-FAIL 0 ENDOF
   ;MATCH ;

: ASSERT-REJECT ( -- )
   EXITED @ TTRUE
   RC @ FORGE-RC T=
   ERR$ s" hb: cannot publish into protected word" CONTAINS? TTRUE ;

\ ---- the persisted protected-WID bitmap ------------------------------------
\ Same shape and same rules as the engine's own read-only view
\ (tools/prot-wid-probe.f): bit w of the band at PROT-BITS-OFF is set exactly
\ when wordlist w is protected, and the two OWNER-API wordlists are protected by
\ rule on every boot path rather than by a bit.
variable BAND-WID

: BAND-TAG ( -- n )
   DATA-OFF 8 + PROT-REG-TAG-CELL + U64@ ;

: BAND-BIT? ( n -- bool ) {: wid:n :}
   DATA-OFF 8 + PROT-BITS-OFF + wid 8 / + U8@
   wid 7 and rshift 1 and 0= 0= ;

\ The lowest wordlist THIS IMAGE records as protected, skipping the two that
\ every boot path protects by rule. Membership for such a wid can only come from
\ the band the snapshot carried, which is what the warm probes below test.
: BAND-WID! ( -- )
   0 BAND-WID !
   PROT-WID-MAX FIRST-DYNAMIC-WID ?do
      BAND-WID @ 0= i BAND-BIT? and if i BAND-WID ! then
   loop ;

\ ---- warm probe sources ----------------------------------------------------
: PROBE-DECLARE$ ( -- ptr u8 n )
   s\" ENUM sw-warm 0 VARIANT sw-warm-a ;VARIANT ;ENUM\n: VIEW-SIZE ( IR-ARENA:view -- n ) NTAPE:TOKENS ;\n: INCREMENT ( n -- n ) 1+ ;\n41 INCREMENT .\n" ;

: FORGE$ ( n -- ptr u8 n ) {: wid:n :}
   SB-RESET
   wid FMT:SB-U
   s"  set-current : FOO ( -- n ) 1 ;" SB-APPEND
   SB$ ;

\ ---- doctored-band legs ----------------------------------------------------
: WRITE-BAND-COPY ( -- )
   BAD-BAND$ IMG IMGU @ WRITE-ALL
   BAD-BAND$ CHMOD-X
   BAD-BAND$ CODESIGN:FORCE ;

: RUN-BAND-COPY ( -- )
   PROC-ARGV-RESET
   BAD-BAND$ >LEN  OUT 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME BAD-BAND$ STORE! ;

\ Doctor one byte of the persisted image, run the result, restore the byte so the
\ later imgdump probes still see the image the writer produced.
: DOCTOR-BYTE ( n n -- ) {: off:n val:n :}
   off U8@ {: orig:n :}
   off val U8!
   WRITE-BAND-COPY
   RUN-BAND-COPY
   off orig U8! ;

: DOCTOR-TAG ( -- )
   DATA-OFF 8 + PROT-REG-TAG-CELL + 0 DOCTOR-BYTE ;

: DOCTOR-WID0 ( -- )
   DATA-OFF 8 + PROT-BITS-OFF + {: off:n :}
   off  off U8@ 1 or  DOCTOR-BYTE ;

: CELL! ( n n -- ) {: off:n value:n :}
   8 0 ?do off i + value i 8 * rshift $FF and U8! loop ;

\ The pad is the slack between the stream's end and the trailer that rounds
\ the image's text up to its alignment (16 KiB on Darwin, 64 KiB on ELF), so
\ an image whose stream ends on that boundary has none to doctor.
: DOCTOR-PAD ( -- )
   STREAM-END {: end:n :}
   end TRAILER-OFF < TTRUE
   end 1 DOCTOR-BYTE ;

: DOCTOR-DATA-LENGTH ( -- )
   TRAILER-OFF SNAP-TRL-DATALEN + {: off:n :}
   off U64@ {: old:n :}
   off old 1- CELL! WRITE-BAND-COPY RUN-BAND-COPY
   off old CELL! ;

: DOCTOR-DICT-COUNT ( -- )
   TRAILER-OFF SNAP-TRL-NDICT + {: off:n :}
   off U64@ {: old:n :}
   off -1 CELL! WRITE-BAND-COPY RUN-BAND-COPY
   off old CELL! ;

: DOCTOR-PARTIAL-CALL ( -- )
   TRAILER-OFF SNAP-TRL-REGLEN + {: len-off:n :}
   len-off U64@ {: old-len:n :}
   old-len 3 and 0= if old-len 1- else old-len then {: len:n :}
   len DICT-SIZE > TTRUE
   DATA-OFF 8 + SNAP-RELOC:CALLMAP-OFF +
      len 32 / DICT-SIZE 32 / - + {: map-off:n :}
   map-off U8@ {: old-map:n :}
   len-off len CELL!
   map-off old-map 1 len 4 / 7 and lshift or U8!
   WRITE-BAND-COPY RUN-BAND-COPY
   map-off old-map U8!
   len-off old-len CELL! ;

: DOCTOR-LEGACY-TRAILER ( -- )
   TRAILER-OFF {: tr:n :}
   tr U64@ {: magic:n :}
   tr SNAPSHOT-FORMAT:HEAP-FIELD + U64@ {: base:n :}
   tr 0 CELL!  tr SNAPSHOT-FORMAT:HEAP-FIELD + SNAP-MAGIC CELL!
   WRITE-BAND-COPY RUN-BAND-COPY
   tr magic CELL!  tr SNAPSHOT-FORMAT:HEAP-FIELD + base CELL! ;

: ASSERT-BAND-REFUSED ( -- )
   EXITED @ TTRUE
   RC @ SNAP-BAD-RC T=
   ERR$ s" hb: snapshot trailer corrupt" CONTAINS? TTRUE ;

: ASSERT-VERSION-REFUSED ( -- )
   EXITED @ TTRUE
   RC @ 80 T=
   ERR$ s" hb: snapshot format version unsupported" CONTAINS? TTRUE ;

\ The format version chooses the schema before any mutable header byte is read.
\ Every malformed address header must stop before restore touches its row vector.
: DOCTOR-ADDRESS-CELL ( n n -- ) {: field:n value:n :}
   DATA-HEADER-OFF field + {: off:n :}
   off U64@ {: old:n :}
   off value CELL! WRITE-BAND-COPY RUN-BAND-COPY
   off old CELL! ASSERT-BAND-REFUSED ;

: DOCTOR-FIRST-ROW ( -- )
   DATA-HEADER-OFF U64@ 0 > TTRUE
   DATA-HEADER-OFF ADDRESS-CELLS:BASE-FIELD + U64@
      ADDRESS-CELLS:BOOT-OFF T=
   DATA-HEADER-OFF ADDRESS-CELLS:HEADER-BYTES + {: off:n :}
   off U64@ {: old:n :}
   off DATA-OFF U64@ CELL!
   WRITE-BAND-COPY RUN-BAND-COPY
   off old CELL! ASSERT-BAND-REFUSED ;

: ADDRESS-HEADER-CASE ( -- )
   s" snapshots carry the address-vector header" T-LABEL
   TRAILER-OFF SNAP-TRL-VERSION + U64@ SNAPSHOT-FORMAT:VERSION T=
   DATA-HEADER-OFF ADDRESS-CELLS:MAGIC-FIELD +
      U64@ ADDRESS-CELLS:MAGIC T=
   s" malformed new headers never select the legacy row layout" T-LABEL
   ADDRESS-CELLS:MAGIC-FIELD 0 DOCTOR-ADDRESS-CELL
   ADDRESS-CELLS:MODE-FIELD 1 DOCTOR-ADDRESS-CELL
   ADDRESS-CELLS:CAP-FIELD 0 DOCTOR-ADDRESS-CELL
   ADDRESS-CELLS:CAP-FIELD -1 DOCTOR-ADDRESS-CELL
   ADDRESS-CELLS:CAP-FIELD ADDRESS-CELLS:MAX-ROWS 1+ DOCTOR-ADDRESS-CELL
   0 -1 DOCTOR-ADDRESS-CELL
   0 DATA-HEADER-OFF ADDRESS-CELLS:CAP-FIELD +
      U64@ 1+ DOCTOR-ADDRESS-CELL
   ADDRESS-CELLS:BASE-FIELD
      DATA-OFF U64@ 1+ DOCTOR-ADDRESS-CELL
   ADDRESS-CELLS:BASE-FIELD
      DATA-OFF U64@ 8 - DOCTOR-ADDRESS-CELL ;

: WARM-CASE ( -- )
   ADDRESS-HEADER-CASE
   s" an address row outside exact DP is refused before restore" T-LABEL
   DOCTOR-FIRST-ROW
   BAND-WID!
   s" persisted protected-WID band carries the bitmap shape tag" T-LABEL
   BAND-TAG PROT-REG-TAG T=
   s" persisted band leaves wid 0 clear" T-LABEL
   0 BAND-BIT? TFALSE
   s" persisted band names a wordlist protected by the image alone" T-LABEL
   BAND-WID @ 0 > TTRUE
   s" warm protected wordlist rejects publication (--load)" T-LABEL
   BAND-WID @ FORGE$ WARM-LOAD  ASSERT-REJECT
   s" warm protected wordlist rejects publication (stdin)" T-LABEL
   BAND-WID @ FORGE$ WARM-STDIN  ASSERT-REJECT
   s" band with a wrong shape tag is refused at snapshot-read" T-LABEL
   DOCTOR-TAG ASSERT-BAND-REFUSED
   s" band claiming wid 0 is refused at snapshot-read" T-LABEL
   DOCTOR-WID0 ASSERT-BAND-REFUSED
   s" a stored DATA length shorter than its sections is refused" T-LABEL
   DOCTOR-DATA-LENGTH ASSERT-BAND-REFUSED
   s" a negative dictionary count is refused before restore" T-LABEL
   DOCTOR-DICT-COUNT ASSERT-BAND-REFUSED
   s" a partial call site is refused before restore" T-LABEL
   DOCTOR-PARTIAL-CALL ASSERT-BAND-REFUSED
   s" the previous snapshot format is refused by the baked loader" T-LABEL
   TRAILER-OFF SNAP-TRL-VERSION + SNAPSHOT-FORMAT:VERSION 1- DOCTOR-BYTE
   ASSERT-VERSION-REFUSED
   s" a legacy 40-byte trailer is refused before restore" T-LABEL
   DOCTOR-LEGACY-TRAILER ASSERT-VERSION-REFUSED
   s" restored compiler accepts a fresh type and existing nominal signatures" T-LABEL
   PROBE-DECLARE$ WARM-STDIN
   EXITED @ TTRUE RC @ 0 T= PARSE-OUT 42 T=
   s" restored dictionary issues a callable local occurrence" T-LABEL
   s\" package SNAP-OCC-PROBE\nCAST: OCC-XT ( n -- [ -- n ] )\ns\q SNAP-WRITER-POISON:OCC-SUBJECT\q XREF-FIND DEF-OCC:SELECT DEF-OCC:CALLABLE OCC-XT execute .\n;package\n" WARM-STDIN
   EXITED @ TTRUE RC @ 0 T= PARSE-OUT 29 T=
   s" restored default hook refuses a mismatched stack effect" T-LABEL
   s" : BAD-EFFECT ( n -- n ) drop ;" WARM-STDIN
   EXITED @ TTRUE RC @ 70 T=
   ERR$ s" hook: non-certified definition: bad-effect" CONTAINS? TTRUE
   s" restored compiler retains current and prior literal row ownership" T-LABEL
   s" require test/compiler/native-string.f" WARM-STDIN
   EXITED @ TTRUE RC @ 0 T= ERR-U @ 0 T=
   OUT OUT-U @ s" test: ok" CONTAINS? TTRUE
   s" restored compiler rejects a scalar used as an arena view" T-LABEL
   s" : BAD-VIEW ( n -- n ) NTAPE:TOKENS ;" WARM-STDIN
   EXITED @ TTRUE RC @ 70 T=
   ERR$ s" ir-arena:view" CONTAINS? TTRUE
   s" restored compiler rejects an undefined word" T-LABEL
   s" : BAD-NAME ( -- ) NO-SUCH-IMAGE-WORD ;" WARM-STDIN
   EXITED @ TTRUE RC @ 70 T=
   ERR$ s" NO-SUCH-IMAGE-WORD" CONTAINS? TTRUE ;

\ ---- scenarios ----
: POISON-CASE ( -- )
   s" test/snapshot-writer-poison.f" BUILD-WITH
   s" poisoned snapshot builds (canaries planted and proven live)" T-LABEL
   RC @ 0<> if OUT OUT-U @ type ERR$ type then
   RC @ 0 T=
   SNAP0$ EXISTS? TTRUE
   SNAP0$ LOAD-IMAGE
   STREAM-SHAPE-CASE
   s" snapshot carries no return stack: base cells zero" T-LABEL
   STACK-BASES-ZERO? TTRUE
   s" the live return-stack canaries are absent from the image" T-LABEL
   CANARIES-ABSENT? TTRUE
   WARM-CASE
   s" imgdump accepts the production snapshot" T-LABEL
   SNAP0$ ASSERT-SNAPSHOT
   WRITE-BAD-SNAPSHOT
   s" imgdump rejects a corrupted header-owned trailer locator" T-LABEL
   BAD-SNAP$ ASSERT-NO-SNAPSHOT ;

\ ---- the heap in the cell grid ---------------------------------------------
: RELOAD ( -- )
   IMG IMGU @ munmap drop
   SNAP0$ LOAD-IMAGE ;

: BUILT-RUNNING ( ptr u8 n ptr u8 n -- )
   {: fixture:ptr size:n rest:ptr restu:n :}
   SNAP0$ fixture size rest restu BUILD-RUNNING-TO
   RC @ 0<> if OUT OUT-U @ type ERR$ type then
   RC @ 0 T=
   RELOAD ;

: BUILT ( ptr u8 n -- )
   {: fixture:ptr size:n :}
   fixture size SAVE-LINE$ BUILT-RUNNING ;

: DOCTOR-CELL ( n n -- ) {: off:n value:n :}
   off U64@ {: old:n :}
   off value CELL! WRITE-BAND-COPY RUN-BAND-COPY
   off old CELL! ;

: RUN-BAND-PROBE ( ptr u8 n -- ) {: s:ptr su:n :}
   WRITE-BAND-COPY
   PROC-ARGV-RESET
   BAD-BAND$ >LEN  s su >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME s su STORE! ;

: HOLE-PROBE$ ( -- ptr u8 n ) s\" SNAP-WRITER-HOLE:RESTORED .\n" ;

: ASSERT-HOLE-RESTORED ( -- )
   EXITED @ TTRUE  RC @ 0 T=  PARSE-OUT 0 T= ;

\ The pad moved in front of the payloads, so the stored DATA ends at the last
\ value. Everything the loader reads is placed from the trailer back, so the
\ image stays valid, and a last value whose final byte says another follows
\ now runs into the trailer.
: SHIFT-PAD ( -- )
   TRAILER-OFF STREAM-END - {: pad:n :}
   REGION-OFF {: from:n :}
   STREAM-END from - {: n:n :}
   n 0 ?do  from n + i - 1- {: at:n :}  at pad + at U8@ U8!  loop
   TRAILER-OFF SNAP-TRL-DATALEN + DATA-LEN pad - CELL! ;

\ The last group's map bit lies below G, so the map's pad stays clear and G
\ and the extent agree; the loader reads that bit before it compares S, which
\ the cleared bit also contradicts.
: GRID-FRAMING-CASE ( -- )
   s" a last group the map leaves absent is refused" T-LABEL
   GRID-G 1- {: top:n :}
   MAP-OFF top CELL-GRID:CELL-BITS / + {: at:n :}
   at  at U8@  1 top CELL-GRID:CELL-BITS mod lshift invert and  DOCTOR-BYTE
   ASSERT-BAND-REFUSED
   s" stored group bytes one group past the map's are refused" T-LABEL
   HEAP-OFF 8 + GRID-S CELL-GRID:GROUP-BYTES + DOCTOR-CELL ASSERT-BAND-REFUSED
   s" a presence bit past the last group is refused" T-LABEL
   GRID-G CELL-GRID:CELL-BITS mod 0<> TTRUE
   MAP-OFF GRID-G CELL-GRID:PMAP-BYTES + 1- {: last:n :}
   last  last U8@ $80 or  DOCTOR-BYTE ASSERT-BAND-REFUSED
   s" the form field takes no value but raw and grid" T-LABEL
   TRAILER-OFF SNAPSHOT-FORMAT:HEAP-FIELD + SNAPSHOT-FORMAT:HEAP-GRID 1+ DOCTOR-CELL
   ASSERT-BAND-REFUSED ;

: GRID-VALUE-CASE ( -- )
   2 VALUE-AT {: two:n :}  1 VALUE-AT {: one:n :}  10 VALUE-AT {: ten:n :}
   s" a value that ends in a zero byte is refused" T-LABEL
   two 0 >= TTRUE  two 1+ 0 DOCTOR-BYTE ASSERT-BAND-REFUSED
   s" a present cell that holds zero is refused" T-LABEL
   one 0 >= TTRUE  one 0 DOCTOR-BYTE ASSERT-BAND-REFUSED
   s" a tenth value byte above one is refused" T-LABEL
   ten 0 >= TTRUE  ten 9 + 2 DOCTOR-BYTE ASSERT-BAND-REFUSED ;

: GRID-TRUNCATED-CASE ( -- )
   SHIFT-PAD
   s" a grid stream with no pad restores" T-LABEL
   TRAILER-OFF STREAM-END T=
   HOLE-PROBE$ RUN-BAND-PROBE ASSERT-HOLE-RESTORED
   s" a last value that runs into the trailer is refused" T-LABEL
   LAST-VALUE WIDTH CELL-GRID:VMAX < TTRUE
   TRAILER-OFF 1- {: last:n :}
   last  last U8@ $80 or  DOCTOR-BYTE ASSERT-BAND-REFUSED
   RELOAD ;

\ A fresh process that saves the image again, with nothing new defined.
: RECAP$ ( -- ptr u8 n ) s" recapture" PATH$ ;
: RECAP-SRC$ ( -- ptr u8 n ) s\" 0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" ;

: RECAPTURE ( -- )
   PROC-ARGV-RESET
   s" --" >LEN PROC-ARGV+
   RECAP$ >LEN PROC-ARGV+
   SNAP0$ >LEN  RECAP-SRC$ >LEN
   OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME RECAP-SRC$ STORE! ;

\ The recapture's heap holds per-run input the first image's does not: the
\ output path and the line the save was read from sit in a few cells, so the
\ two streams differ in those values. What must not differ is the extent and
\ the grid's framing: a stream that embedded the previous one, or a decoder
\ that dropped a cell, would move them.
variable FIRST-EXTENT  variable FIRST-G  variable FIRST-S

: KEEP-FRAME ( -- )
   HEAP-BYTES FIRST-EXTENT !  GRID-G FIRST-G !  GRID-S FIRST-S ! ;

: RECAPTURE-CASE ( -- )
   KEEP-FRAME
   RECAPTURE
   s" the warm image saves itself again" T-LABEL
   EXITED @ TTRUE  RC @ 0 T=
   IMG IMGU @ munmap drop
   RECAP$ LOAD-IMAGE
   s" a recapture stores the same heap extent and grid framing" T-LABEL
   HEAP-FORM SNAPSHOT-FORMAT:HEAP-GRID T=
   HEAP-BYTES FIRST-EXTENT @ T=
   GRID-G FIRST-G @ T=
   GRID-S FIRST-S @ T=
   RELOAD ;

\ Where a grid stream ends against the boundary moves with every byte before
\ it, the engine's and the image's own output path, which the save keeps in
\ DATA (src/habu/snap-lib.f SNAP:OUTPUT): the hole image's ended on it at a
\ 685-byte gate HB_TMP. No one image can promise a pad, so the case owns two.
\ test/snapshot-writer-pad.f is the hole fixture and a little more, saved to
\ the same path, so its stream ends after the hole image's by far less than
\ any alignment, and the two cannot both end on a boundary. Each one that has
\ a pad refuses a nonzero byte in it.
variable PADDED

: PAD-REFUSED ( -- )
   STREAM-END TRAILER-OFF < if
      DOCTOR-PAD ASSERT-BAND-REFUSED
      1 PADDED +!
   then ;

: GRID-PAD-CASE ( -- )
   s" nonzero padding after the grid is refused" T-LABEL
   0 PADDED !
   PAD-REFUSED
   STREAM-END {: hole-end:n :}
   s" test/snapshot-writer-pad.f" BUILT
   HEAP-FORM SNAPSHOT-FORMAT:HEAP-GRID T=
   STREAM-END hole-end > TTRUE
   PAD-REFUSED
   PADDED @ 0 > TTRUE ;

: HOLE-CASE ( -- )
   s" a heap with a million-byte hole builds" T-LABEL
   s" test/snapshot-writer-hole.f" BUILT
   s" the sparse heap is stored as the cell grid, smaller than its bytes" T-LABEL
   HEAP-FORM SNAPSHOT-FORMAT:HEAP-GRID T=
   STREAM-END HEAP-OFF - HEAP-BYTES < TTRUE
   s" the warm image reads the hole and the zeroed seed cells as zero" T-LABEL
   HOLE-PROBE$ WARM-STDIN ASSERT-HOLE-RESTORED
   GRID-FRAMING-CASE
   GRID-VALUE-CASE
   GRID-TRUNCATED-CASE
   RECAPTURE-CASE
   GRID-PAD-CASE ;

\ ---- a heap that ends inside a cell -----------------------------------------
\ The tail fixture's heap ends TAIL-BYTES into the second cell of its last
\ group, the first cell zero, so that group stores one bitmap byte with one bit
\ set. The extent is stored twice, as the stream's first cell and as DP in the
\ raw prefix, and the loader requires them to agree. DOCTOR-EXTENT moves both,
\ so the stream reaches the heap decoder with a consistent shorter extent and
\ the map, G and S untouched.
3 constant TAIL-BYTES                \ test/snapshot-writer-tail.f's

: EXTENT ( -- n ) DATA-OFF U64@ ;

: DOCTOR-EXTENT ( n -- ) {: ext:n :}
   EXTENT {: old:n :}
   DATA-OFF 8 + DP-CELL + {: dp:n :}
   dp U64@ {: old-dp:n :}
   DATA-OFF ext CELL!  dp old-dp old - ext + CELL!
   WRITE-BAND-COPY RUN-BAND-COPY
   DATA-OFF old CELL!  dp old-dp CELL! ;

: LAST-GROUP-BM ( -- n ) VALUES-OFF CELL-GRID:GROUP-BYTES - ;

\ Each doctored image leaves every check the loader makes before its target
\ true, so the target is the check that refuses it. The address-cell header is
\ the engine's inline form, the one that reads no extent.
: TAIL-REFUSED-CASE ( -- )
   DATA-HEADER-OFF ADDRESS-CELLS:BASE-FIELD + U64@ ADDRESS-CELLS:BOOT-OFF T=
   \ No bit in the last group: the groups before it decode as written, then
   \ its bitmap is 64 zero bytes.
   s" a present group with no present cell is refused" T-LABEL
   LAST-GROUP-BM U8@ 2 T=               \ the group's second cell alone
   LAST-GROUP-BM 0 DOCTOR-BYTE ASSERT-BAND-REFUSED
   \ An extent at the last group's first byte needs one group fewer than G,
   \ while the map still ends at a present group and S still matches it.
   s" a group count past the groups the extent needs is refused" T-LABEL
   GRID-G 1- CELL-GRID:GROUP-SPAN * DATA-START + DOCTOR-EXTENT ASSERT-BAND-REFUSED
   \ An extent at the tail cell's first byte keeps G's groups: every earlier
   \ cell is stored whole, then the tail cell starts at the extent.
   s" a present cell at the extent is refused" T-LABEL
   EXTENT TAIL-BYTES - DOCTOR-EXTENT ASSERT-BAND-REFUSED
   \ One byte short, the tail cell still straddles the extent, its last byte
   \ above it.
   s" a last cell with a byte above the extent is refused" T-LABEL
   EXTENT 1- DOCTOR-EXTENT ASSERT-BAND-REFUSED ;

\ The tail is laid after the capture's prepare: see the fixture's header.
: TAIL-SAVE$ ( -- ptr u8 n )
   s\" NATIVE-RUNTIME:CAPTURE-PREPARE SNAP-WRITER-TAIL:LAY 0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" ;

: TAIL-CASE ( -- )
   s" a heap that ends inside a cell builds" T-LABEL
   s" test/snapshot-writer-tail.f" TAIL-SAVE$ BUILT-RUNNING
   s" the grid stores a heap that ends inside a cell" T-LABEL
   HEAP-FORM SNAPSHOT-FORMAT:HEAP-GRID T=
   EXTENT CELL-GRID:CELL-BYTES mod TAIL-BYTES T=
   s" the warm image restores the last cell's bytes and the exact extent" T-LABEL
   s\" SNAP-WRITER-TAIL:RESTORED .\n" WARM-STDIN
   EXITED @ TTRUE  RC @ 0 T=  PARSE-OUT 0 T=
   TAIL-REFUSED-CASE ;

\ The dense fixture allots 48 MiB past the engine's own heap, so it runs where
\ DATA holds twice that. Linux's DATA window is 32 MiB (src/os/linux/layout.f):
\ there the engine heap's zero cells save more than any dense heap that fits
\ could cost, so no snapshot keeps its heap's bytes.
48 1024 * 1024 * 2 * constant DENSE-DATA

: DENSE-CASE ( -- )
   s" a heap of ten-byte cells builds" T-LABEL
   s" test/snapshot-writer-dense.f" BUILT
   s" a heap the grid would enlarge keeps its bytes" T-LABEL
   HEAP-FORM SNAPSHOT-FORMAT:HEAP-RAW T=
   TRAILER-OFF STREAM-END - PROT-PAGE-MAX < TTRUE
   s" the raw heap restores" T-LABEL
   s\" SNAP-WRITER-DENSE:MISMATCHES .\n" WARM-STDIN
   EXITED @ TTRUE  RC @ 0 T=  PARSE-OUT 0 T=
   s" nonzero padding after a raw heap is refused" T-LABEL
   DOCTOR-PAD ASSERT-BAND-REFUSED ;

\ ---- a failed write leaves the output path as it was ------------------------
: CLOSE-FAIL$ ( -- ptr u8 n ) s" close-fail" PATH$ ;

variable STRAYS

\ A file a failed write could leave under the root: the close-fail target, which
\ did not exist before, or anything beside the application image or the
\ directory output named after it.
: STRAY ( ptr u8 n -- )
   BASENAME {: a:ptr u:n :}
   a u s" close-fail" STARTS-WITH?  a u s" application." STARTS-WITH?  or
   a u s" dir-out." STARTS-WITH?  or
   if 1 STRAYS +! then ;

: STRAYS@ ( -- n )
   0 STRAYS !
   ROOT [: STRAY ;] WALK-FILES
   STRAYS @ ;

\ The file holds exactly the bytes LOAD-IMAGE read last.
: HOLDS-IMG? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u FILE? 0= if false exit then
   a u FILE-SIZE IMGU @ <> if false exit then
   IMGU @ MEM-ALLOC-BYTES {: buf:ptr size:n :}
   a u buf size READ-ALL size =
   buf size IMG IMGU @ STR= and
   buf size munmap drop ;

: CLOSE-FAIL-CASE ( -- )
   CLOSE-FAIL$ s" test/snapshot-writer-close-fail.f" BUILD-WITH-TO
   s" snapshot writer fails closed when the final close fails" T-LABEL
   RC @ OUTPUT-FAIL-RC T=
   ERR$ s" snap: output close failed" CONTAINS? TTRUE
   s" a failed write leaves no image where there was none" T-LABEL
   CLOSE-FAIL$ EXISTS? TFALSE
   STRAYS@ 0 T=
   RELOAD
   s" test/snapshot-writer-close-fail.f" BUILD-WITH
   s" a failed write leaves the previous image in place" T-LABEL
   RC @ OUTPUT-FAIL-RC T=
   SNAP0$ HOLDS-IMG? TTRUE
   STRAYS@ 0 T= ;

\ ---- an output that cannot be opened dies by name ---------------------------
: MISSING$ ( -- ptr u8 n ) s" missing" PATH$ ;
: IN-MISSING$ ( -- ptr u8 n ) s" missing/application" PATH$ ;

\ The application loads nothing new: the writer refuses before it writes a byte.
: OPEN-FAIL-CASE ( -- )
   IN-MISSING$ s" lib/string.f" BUILD-WITH-TO
   s" an output in a missing directory is refused by name" T-LABEL
   RC @ OUTPUT-FAIL-RC T=
   ERR$ s" snap: cannot open output" CONTAINS? TTRUE
   s" an output that cannot be opened creates nothing" T-LABEL
   MISSING$ EXISTS? TFALSE ;

\ ---- an output the rename cannot replace dies by name -----------------------
: DIR-OUT$ ( -- ptr u8 n ) s" dir-out" PATH$ ;

\ A directory at the output path lets the sibling be written and signed, then
\ refuses the rename.
: REPLACE-FAIL-CASE ( -- )
   DIR-OUT$ MAKE-DIR
   DIR-OUT$ s" lib/string.f" BUILD-WITH-TO
   s" an output the rename cannot replace is refused by name" T-LABEL
   RC @ OUTPUT-FAIL-RC T=
   ERR$ s" snap: cannot replace output" CONTAINS? TTRUE
   s" the refused output stays as it was and the staged image goes" T-LABEL
   DIR-OUT$ DIR? TTRUE
   STRAYS@ 0 T= ;

: SHADOW$ ( -- ptr u8 n )
   SB-RESET
   s\" undefine snapshot-format\n: snapshot-format ( -- n ) " SB-APPEND
   SNAPSHOT-FORMAT:VERSION FMT:SB-U
   s\"  ;\n" SB-APPEND
   SB$ ;

\ The shadow answers the version the writer checks for, so only the capability
\ check can refuse it.
: SHADOW-CASE ( -- )
   SHADOW-SRC$ SHADOW$ WRITE-ALL
   SHADOW-SNAP$ SHADOW-SRC$ BUILD-WITH-TO
   s" a source-shadowed capability cannot authorize capture" T-LABEL
   RC @ 74 <> if OUT OUT-U @ type ERR$ type then
   RC @ 74 T=
   ERR$ s" snap: format capability is not an engine primitive" CONTAINS? TTRUE
   SHADOW-SNAP$ EXISTS? TFALSE ;

\ ---- live compiler state refuses capture -----------------------------------
: ACTIVE-SNAP$ ( -- ptr u8 n ) s" active" PATH$ ;
: QUIESCENT-SNAP$ ( -- ptr u8 n ) s" quiescent" PATH$ ;

\ `rest` reaches SNAP-WRITER-ACTIVE:SAVE, an immediate that saves.
: ACTIVE-BUILD ( ptr u8 n ptr u8 n -- )
   {: target:ptr targetu:n rest:ptr restu:n :}
   target targetu s" test/snapshot-writer-active.f" rest restu BUILD-RUNNING-TO ;

: ACTIVE-REFUSED ( ptr u8 n -- ) {: rest:ptr restu:n :}
   ACTIVE-SNAP$ rest restu ACTIVE-BUILD
   RC @ 74 <> if OUT OUT-U @ type ERR$ type then
   RC @ 74 T=
   ERR$ s" snap: active compiler state at capture" CONTAINS? TTRUE
   ACTIVE-SNAP$ EXISTS? TFALSE ;

\ Tier 1 compiles a definition as one unit, so only its pending record is live
\ when the immediate runs. Tier 0 also holds the innermost open quotation and
\ parks each enclosing one; its code has no native provenance either, so the
\ rc and the message show the quiescence check refuses before that one does.
: ACTIVE-CASE ( -- )
   QUIESCENT-SNAP$ s\" SNAP-WRITER-ACTIVE:SAVE\n" ACTIVE-BUILD
   s" a capture from the immediate with nothing open saves" T-LABEL
   RC @ 0<> if OUT OUT-U @ type ERR$ type then
   RC @ 0 T=
   QUIESCENT-SNAP$ EXISTS? TTRUE
   s" a capture with a definition open is refused" T-LABEL
   s\" : ACTIVE-DEF ( -- ) SNAP-WRITER-ACTIVE:SAVE ;\n" ACTIVE-REFUSED
   s" a capture with a quotation open is refused" T-LABEL
   s\" 0 set-tier\n: ACTIVE-DEF ( -- ) [: SNAP-WRITER-ACTIVE:SAVE ;] drop ;\n"
   ACTIVE-REFUSED
   s" a capture with an enclosing quotation parked is refused" T-LABEL
   s\" 0 set-tier\n: ACTIVE-DEF ( -- ) [: [: SNAP-WRITER-ACTIVE:SAVE ;] drop ;] drop ;\n"
   ACTIVE-REFUSED ;

\ ---- policy belongs to the sealing process, not its saved image ----------
: POLICY-SAVE$ ( -- ptr u8 n )
   S\" : SAVE-POLICY ( -- ) s\q PDEP\q POLICY:ALLOW 0 SCRIPT-ARGV$ APP-IMAGE:SAVE ; SAVE-POLICY\n" ;

: POLICY-SEALED-SAVE$ ( -- ptr u8 n )
   S\" : SAVE-POLICY ( -- ) s\q PDEP\q POLICY:ALLOW POLICY:SEAL 0 SCRIPT-ARGV$ APP-IMAGE:SAVE ; SAVE-POLICY\n" ;

: POLICY-PROBE$ ( -- ptr u8 n )
   S\" POLICY-NDICT-CELL data-base + @ .\n: POLICY-BYTES ( -- n ) 0 PROT-BITS-BYTES 0 ?do POLICY-BITS-OFF data-base + i + c@ 0<> if 1+ then loop ; POLICY-BYTES .\n" ;

: POLICY-DATA-ZERO? ( -- bool )
   DATA-OFF 8 + POLICY-NDICT-CELL + U64@ 0<> if false exit then
   PROT-BITS-BYTES 0 ?do
      DATA-OFF 8 + POLICY-BITS-OFF + i + U8@ 0<> if false unloop exit then
   loop
   true ;

: POLICY-ARTIFACT$ ( -- ptr u8 n ) s" build/policy-image-run.txt" ;

: POLICY-IMAGE-CASE ( ptr u8 n ptr u8 n -- )
   {: source:ptr sourceu:n label:ptr labelu:n :}
   s" test/policy/image-save.f" source sourceu BUILT-RUNNING
   s" saved policy DATA is clear" T-LABEL
   POLICY-DATA-ZERO? TTRUE
   s" restored policy starts clear" T-LABEL
   POLICY-PROBE$ WARM-STDIN
   EXITED @ TTRUE  RC @ 0 T=  ERR-U @ 0 T=
   OUT OUT-U @ S\" 0\n0\n" T$=
   POLICY-ARTIFACT$ label labelu APPEND-FILE
   POLICY-ARTIFACT$ OUT OUT-U @ APPEND-FILE ;

: POLICY-CASE ( -- )
   s" build" MAKE-DIRS
   POLICY-ARTIFACT$ s" " WRITE-ALL
   POLICY-SAVE$ S\" allow\n" POLICY-IMAGE-CASE
   POLICY-SEALED-SAVE$ S\" sealed\n" POLICY-IMAGE-CASE ;

: BODY ( -- )
   SETUP-ROOT
   POISON-CASE
   HOLE-CASE
   TAIL-CASE
   DATA-SIZE DENSE-DATA > if DENSE-CASE then
   CLOSE-FAIL-CASE
   OPEN-FAIL-CASE
   REPLACE-FAIL-CASE
   SHADOW-CASE
   ACTIVE-CASE
   POLICY-CASE
   IMG IMGU @ munmap drop ;

public

: RUN ( -- )
   T-RESET
   CLEANUP-RESET
   [: BODY ;] catch {: code:n :}
   CLEANUP-RUN
   code 0 <> if code throw then
   T-REPORT
   s" snapshot-writer-test: ok" type cr ;

;package

SNAP-WRITER-TEST:RUN
