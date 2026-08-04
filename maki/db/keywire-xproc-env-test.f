\ maki/db/keywire-xproc-env-test.f - the DECISIVE cross-process ENVELOPE + TRANSACTION
\ identity test (dot habu-wire-content-key-e5efaa74, § 23.9; the "encode in one process
\ image, decode in a fresh one" requirement applied to the composite codecs).
\
\ This process (the PARENT) BUILDs an artifact envelope (self artifact-id + a two-element
\ dependency set + the four 32-byte foreign ids + a source-revision + an 8-byte event) and a
\ transaction (base revision + read/write/dep), ENCODEs both to a fixed-layout file, and
\ spawns a FRESH bin/hb (the child, maki/db/keywire-xproc-env-child.f) that registers DECOYS
\ FIRST - so its real ids get raw indices that differ from this process's - reproduces the
\ journal depth, reads the file, and DECODEs both blobs. The child prints XPROC-OK iff the
\ envelope decodes ok AND the transaction decodes ok with its base revision resolving to the
\ same content.
\
\ Why it is decisive: envelope identity (and the transaction Merkle base) survives PROCESS
\ DEATH only because the id / dependency / source-revision / foreign-id wire forms are now
\ 32-byte content keys, not process-local registry raws. A raw-index form would
\ make the child resolve a decoy (or go out of range) because its raw indices are shifted; the
\ content key resolves correctly regardless of registration order.
\
\ File layout: [event-seq u32][env-len u32][env bytes][txn-len u32][txn bytes] (LE). The
\ spawn/capture is the maki/db/keywire-xproc-test.f fresh-process replay pattern.

require lib/test.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f          \ RUN-ARGV-CAPTURE: the fresh-process decode child
require lib/engine-candidate.f
require maki/journal.f              \ JOURNAL:APPEND / SEQ (the parent's event)
require maki/db/artifact.f          \ ARTIFACT: envelope build/encode (public build protocol)
require maki/db/transaction.f       \ TX: transaction build/encode
require maki/device-artifacts.f     \ MAKI-GRADE: private tmp driver path (PREPARE/DRIVER$/CLEAN)
require maki/db/keywire-xproc-env-child.f  \ KWXPC-ENV: shared descriptors + REG-* + RESOLVE-ENV

package KWXPC-ENV-TEST

create ENVBUF 1024 allot                 \ encoded envelope bytes
create TXNBUF 1024 allot                 \ encoded transaction bytes
create FILEBUF $2000 allot               \ assembled blob written to the shared path
variable FO                              \ assembly cursor
create KPATH FS-PATH-CAP allot           \ key-file path (sibling of the spawn driver)
variable KPATH-U
create XP-OUT $2000 allot   create XP-ERR $1000 allot

\ ---- fixed-layout blob assembly -----------------------------------------------
: LE32! ( n ptr u8 -- ) {: v:n a:ptr :}
   v $FF and a c!  v 8 rshift $FF and a 1+ c!
   v 16 rshift $FF and a 2 + c!  v 24 rshift $FF and a 3 + c! ;
: PUT32 ( n -- )   FILEBUF FO @ + LE32!  FO @ 4 + FO ! ;
: PUT-BYTES ( ptr u8 n -- ) {: a:ptr u:n :}
   a FILEBUF FO @ + u BYTE-COPY  FO @ u + FO ! ;

\ Key-file path: the spawn tmp root's "keys.bin" (swap the driver basename - both are 8
\ bytes: "driver.f" -> "keys.bin"), so parent write and child read share one absolute path.
: KPATH! ( -- )
   MAKI-GRADE:DRIVER$ {: a:ptr u:n :}
   a KPATH u BYTE-COPY
   s" keys.bin" {: kp:ptr ku:n :}
   kp  KPATH u 8 - +  ku  BYTE-COPY
   u KPATH-U ! ;
: KPATH$ ( -- ptr u8 n )   KPATH KPATH-U @ ;

\ ---- build the envelope + transaction in THIS process image --------------------
\ BUILD-ENV populates the pending dependency + source-revision sets through the public
\ ARTIFACT build protocol, then builds and encodes the weight envelope; returns its length.
: BUILD-ENV ( CAD-KIND:audit-event-id -- n ) {: event:CAD-KIND:audit-event-id :}
   ARTIFACT:DEPS-RESET  KWXPC-ENV:REG-DEP1 ARTIFACT:DEP+  KWXPC-ENV:REG-DEP2 ARTIFACT:DEP+
   ARTIFACT:SREVS-RESET  KWXPC-ENV:REG-SREV ARTIFACT:SREV+
   1 KWXPC-ENV:REG-ID
      KWXPC-ENV:REG-SCHEMA KWXPC-ENV:REG-PRODUCER KWXPC-ENV:REG-CONFIG
      KWXPC-ENV:REG-NPOL KWXPC-ENV:REG-TARGET  event  ARTIFACT:BUILD-WEIGHT
   ENVBUF 1024 ARTIFACT:ENCODE-WEIGHT ;

\ BUILD-TXN builds a valid transaction (dep on the read object) on the shared base revision
\ and encodes it; returns its length.
: BUILD-TXN ( -- n )
   KWXPC-ENV:REG-REV TX:OPEN
   KWXPC-ENV:REG-TXR TX:PRESENT TX:READ+
   KWXPC-ENV:REG-TXW TX:WRITE+
   KWXPC-ENV:REG-TXR TX:DEP+
   TX:BUILD  TXNBUF 1024 TX:ENCODE ;

\ ENCODE-BLOB assembles [event-seq][env-len][env][txn-len][txn] into FILEBUF.
: ENCODE-BLOB ( -- )
   s" xpe-env-event" JOURNAL:APPEND {: event:CAD-KIND:audit-event-id :}
   event BUILD-ENV {: envlen:n :}
   BUILD-TXN {: txnlen:n :}
   0 FO !
   event JOURNAL:SEQ PUT32
   envlen PUT32  ENVBUF envlen PUT-BYTES
   txnlen PUT32  TXNBUF txnlen PUT-BYTES ;

\ ---- spawn the fresh-process decode child -------------------------------------
: DRIVER! ( -- )
   SB-RESET
   s" require maki/db/keywire-xproc-env-child.f" SB-APPEND  $0A SB-APPEND-C
   s" s" SB-APPEND  $22 SB-APPEND-C  $20 SB-APPEND-C     \ emit:  s"<space>
   KPATH$ SB-APPEND  $22 SB-APPEND-C                     \ emit:  <keypath>"
   s"  KWXPC-ENV:RESOLVE-ENV" SB-APPEND  $0A SB-APPEND-C
   MAKI-GRADE:DRIVER$ SB$ WRITE-ALL ;

: CHILD-VERDICT$ ( -- ptr u8 n )
   PROC-ARGV-RESET
   s" --load"             >LEN PROC-ARGV+
   MAKI-GRADE:DRIVER$     >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN  XP-OUT $2000 >LEN  XP-ERR $1000 >LEN  30000 >MS  RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 0 >RC ENDOF               \ clean child exit -> rc 0
     err OF PCAP-FAILED:UNMAKE ENDOF                       \ nonzero child: (out err code) on stack
   ;MATCH
   {: outu:len erru:len rc:rc :}
   rc RC>N 0 <> if XP-ERR erru LEN>N type cr then        \ surface child stderr on failure
   rc RC>N 0 T=
   XP-OUT outu LEN>N ;

T-RESET

s" habu-keywire-xproc-env" MAKI-GRADE:PREPARE
KPATH!
ENCODE-BLOB
KPATH$ FILEBUF FO @ WRITE-ALL
DRIVER!
CHILD-VERDICT$ s" XPROC-OK" T$=       \ fresh process decoded the envelope + txn by content
MAKI-GRADE:CLEAN

;package

T-REPORT
