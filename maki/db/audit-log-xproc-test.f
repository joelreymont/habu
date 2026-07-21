\ maki/db/audit-log-xproc-test.f - the DECISIVE cross-process byte-stability test for the
\ canonical audit event log (maki/db/audit-log.f, dot habu-v2-deterministic-audit-428d27c2;
\ the maki/db/keywire-xproc-test.f fresh-process replay precedent).
\
\ This process (the PARENT) builds the canonical event log from content-addressed identities,
\ serializes it with ENCODE-LOG, and writes the frame to a file. It then spawns a FRESH bin/hb
\ (the child, maki/db/audit-log-xproc-child.f) that reads the frame, VERIFY-LOGs its chain, and
\ INDEPENDENTLY rebuilds the SAME logical log from an EMPTY store under a decoy-shifted registry.
\ The child prints XPROC-OK iff its rebuilt frame is BYTE-IDENTICAL to the parent's - proving the
\ log replays byte-stably across fresh processes because every event is keyed by a cross-process
\ content key, not a process-local registry raw.

require lib/test.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/engine-candidate.f
require maki/device-artifacts.f              \ MAKI-GRADE: private tmp driver path (PREPARE/DRIVER$/CLEAN)
require maki/db/audit-log-xproc-child.f      \ AUDIT-XPROC: shared BUILD-LOG + RUN-CHILD

package AUDIT-XPROC-TEST

create XFRAME $4000 allot                    \ the parent's serialized log frame
create FPATH FS-PATH-CAP allot               \ frame-file path (sibling of the spawn driver)
variable FPATH-U
create XP-OUT $2000 allot   create XP-ERR $1000 allot

\ Frame-file path: the spawn tmp root's "alog.bin" (swap the driver basename - both are 8
\ bytes: "driver.f" -> "alog.bin"), so parent write and child read share one absolute path.
: FPATH! ( -- )
   MAKI-GRADE:DRIVER$ {: a:ptr u:n :}
   a FPATH u BYTE-COPY
   s" alog.bin" {: kp:ptr ku:n :}
   kp  FPATH u 8 - +  ku  BYTE-COPY
   u FPATH-U ! ;
: FPATH$ ( -- ptr u8 n )   FPATH FPATH-U @ ;

\ WRITE-FRAME builds the canonical log, encodes it, and writes the frame to the shared path.
: WRITE-FRAME ( -- )
   AUDIT-XPROC:BUILD-LOG
   FPATH$ XFRAME  XFRAME $4000 AUDIT:ENCODE-LOG  WRITE-ALL ;

\ DRIVER!: the tiny spawn driver - load the child fixture, resolve the frame file.
: DRIVER! ( -- )
   SB-RESET
   s" require maki/db/audit-log-xproc-child.f" SB-APPEND  $0A SB-APPEND-C
   s" s" SB-APPEND  $22 SB-APPEND-C  $20 SB-APPEND-C     \ emit:  s"<space>
   FPATH$ SB-APPEND  $22 SB-APPEND-C                     \ emit:  <framepath>"
   s"  AUDIT-XPROC:RUN-CHILD" SB-APPEND  $0A SB-APPEND-C
   MAKI-GRADE:DRIVER$ SB$ WRITE-ALL ;

\ CHILD-VERDICT$: spawn the fresh bin/hb child, return its captured stdout.
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

s" habu-audit-xproc" MAKI-GRADE:PREPARE
FPATH!
WRITE-FRAME
DRIVER!
CHILD-VERDICT$ s" XPROC-OK" T$=       \ fresh process rebuilt a byte-identical frame from content keys
MAKI-GRADE:CLEAN

;package

T-REPORT
