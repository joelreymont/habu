\ Application-image writer behavior through APP-IMAGE:SAVE: transient return
\ frames are cleared, protected namespaces survive restore, corrupt images
\ fail closed, and a failed final close is reported.
require lib/test.f
require src/habu/address-cells.f
require lib/fmt.f
require lib/memory.f
require src/habu/stack-abi.f
require test/snapshot-writer-poison-canaries.f
require lib/fs-mutate.f
require lib/process-env.f
require lib/engine-candidate.f
require lib/codesign.f

package SNAP-WRITER-TEST

$8000 constant CAP
240000 constant TIMEOUT-MS
74 constant CLOSE-FAIL-RC
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
   s" habu-snapshot-writer" TMPDIR-MKDIR ROOT!
   ROOT CLEANUP-TREE+ ;

create PATH-BUF FS-PATH-CAP allot
: PATH$ ( ptr u8 n -- ptr u8 n )
   {: name:ptr size:n :}
   ROOT name size PATH-BUF JOIN-PATH PATH-BUF swap ;
: SNAP0$ ( -- ptr u8 n ) s" application" PATH$ ;
: SNAP-SRC$ ( -- ptr u8 n ) s" probe.f" PATH$ ;
: BAD-SNAP$ ( -- ptr u8 n ) s" bad-locator" PATH$ ;
: BAD-BAND$ ( -- ptr u8 n ) s" bad-registry" PATH$ ;

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
   DATA-OFF STACK-ABI:RETURN-BASE-CELL + U64@ 0=
   DATA-OFF STACK-ABI:LOOP-BASE-CELL + U64@ 0= and ;

: DATA-LEN ( -- n )
   TRAILER-OFF SNAP-TRL-DATALEN + U64@ ;

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

: BUILD-WITH ( ptr u8 n -- ) {: fixture:ptr size:n :}
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   SNAP0$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   SB-RESET
   s\" require src/habu/app-image.f\nrequire " SB-APPEND
   fixture size SB-APPEND
   s\" \n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" SB-APPEND
   ENGINE$ >LEN SB$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE CAPTURE! ;

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
: STORE! ( len len outcome -- )
   MATCH outcome
     exited OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout OF 0 RC ! 0 0= 0= EXITED ! ENDOF
   ;MATCH
   LEN>N ERR-U !  LEN>N OUT-U ! ;

: WARM-LOAD ( ptr u8 n -- ) {: s:ptr su:n :}
   SNAP-SRC$ s su WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   SNAP-SRC$ >LEN PROC-ARGV+
   SNAP0$ >LEN  OUT 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME STORE! ;

: WARM-STDIN ( ptr u8 n -- ) {: s:ptr su:n :}
   PROC-ARGV-RESET
   SNAP0$ >LEN  s su >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME STORE! ;

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
   DATA-OFF PROT-REG-TAG-CELL + U64@ ;

: BAND-BIT? ( n -- bool ) {: wid:n :}
   DATA-OFF PROT-BITS-OFF + wid 8 / + U8@
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
   BAD-BAND$ CODESIGN:FORCE ;

: RUN-BAND-COPY ( -- )
   PROC-ARGV-RESET
   BAD-BAND$ >LEN  OUT 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME STORE! ;

\ Doctor one byte of the persisted image, run the result, restore the byte so the
\ later imgdump probes still see the image the writer produced.
: DOCTOR-BYTE ( n n -- ) {: off:n val:n :}
   off U8@ {: orig:n :}
   off val U8!
   WRITE-BAND-COPY
   RUN-BAND-COPY
   off orig U8! ;

: DOCTOR-TAG ( -- )
   DATA-OFF PROT-REG-TAG-CELL + 0 DOCTOR-BYTE ;

: DOCTOR-WID0 ( -- )
   DATA-OFF PROT-BITS-OFF + {: off:n :}
   off  off U8@ 1 or  DOCTOR-BYTE ;

: ASSERT-BAND-REFUSED ( -- )
   EXITED @ TTRUE
   RC @ SNAP-BAD-RC T=
   ERR$ s" hb: snapshot trailer corrupt" CONTAINS? TTRUE ;

\ The format version chooses the schema before any mutable header byte is read.
\ Every malformed v9 header must stop before restore touches its row vector.
: CELL! ( n n -- ) {: off:n value:n :}
   8 0 ?do off i + value i 8 * rshift $FF and U8! loop ;

: DOCTOR-ADDRESS-CELL ( n n -- ) {: field:n value:n :}
   DATA-OFF SNAP-RELOC:XTCELL-N-CELL + field + {: off:n :}
   off U64@ {: old:n :}
   off value CELL! WRITE-BAND-COPY RUN-BAND-COPY
   off old CELL! ASSERT-BAND-REFUSED ;

: ADDRESS-HEADER-CASE ( -- )
   s" snapshots carry the v9 address-vector header" T-LABEL
   TRAILER-OFF SNAP-TRL-VERSION + U64@ ADDRESS-CELLS:SNAPSHOT-VERSION T=
   DATA-OFF SNAP-RELOC:XTCELL-N-CELL + ADDRESS-CELLS:MAGIC-FIELD +
      U64@ ADDRESS-CELLS:MAGIC T=
   s" malformed new headers never select the legacy row layout" T-LABEL
   ADDRESS-CELLS:MAGIC-FIELD 0 DOCTOR-ADDRESS-CELL
   ADDRESS-CELLS:MODE-FIELD 1 DOCTOR-ADDRESS-CELL
   ADDRESS-CELLS:CAP-FIELD 0 DOCTOR-ADDRESS-CELL
   ADDRESS-CELLS:CAP-FIELD -1 DOCTOR-ADDRESS-CELL
   ADDRESS-CELLS:CAP-FIELD ADDRESS-CELLS:MAX-ROWS 1+ DOCTOR-ADDRESS-CELL
   0 -1 DOCTOR-ADDRESS-CELL
   0 DATA-OFF SNAP-RELOC:XTCELL-N-CELL + ADDRESS-CELLS:CAP-FIELD +
      U64@ 1+ DOCTOR-ADDRESS-CELL
   ADDRESS-CELLS:BASE-FIELD
      TRAILER-OFF SNAP-TRL-DATALEN + U64@ 1+ DOCTOR-ADDRESS-CELL
   ADDRESS-CELLS:BASE-FIELD
      TRAILER-OFF SNAP-TRL-DATALEN + U64@ 8 - DOCTOR-ADDRESS-CELL ;

: WARM-CASE ( -- )
   ADDRESS-HEADER-CASE
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
   s" restored compiler accepts a fresh type and existing nominal signatures" T-LABEL
   PROBE-DECLARE$ WARM-STDIN
   EXITED @ TTRUE RC @ 0 T= PARSE-OUT 42 T=
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

: CLOSE-FAIL-CASE ( -- )
   s" test/snapshot-writer-close-fail.f" BUILD-WITH
   s" snapshot writer fails closed when the final close fails" T-LABEL
   RC @ CLOSE-FAIL-RC T=
   ERR$ s" snap: output close failed" CONTAINS? TTRUE ;

: BODY ( -- )
   SETUP-ROOT
   POISON-CASE
   CLOSE-FAIL-CASE
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
