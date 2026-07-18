\ maki/db/diff-case-store-xproc-test.f - the DECISIVE cross-process durability test for the
\ per-case output store (maki/db/diff-case-store.f; dot habu-v2-differential-runner-13359019).
\
\ This process (the PARENT) points the store at a private dir, runs the shared deterministic
\ cases, and PUTs a durable record per case (STORE-ALL). It then spawns a FRESH bin/hb (the
\ child, maki/db/diff-case-store-xproc-child.f) that points the SAME store dir, registers DECOYS
\ first so its real ids get shifted raw indices, rebuilds each case, LOADs, and byte-matches the
\ rehydrated record against a re-derived expected record. The child prints CSXP-OK iff every
\ case's full record (input identity, subject + reference outputs, classified outcome, and
\ environment digest) survived process death and byte-matches.
\
\ Why it is decisive: the record survives process death only because the store key composes
\ CONTENT keys (subject / suite-digest / environment) plus the deterministic case-id, all
\ registration-order-independent. A registry-raw key would make the shifted child miss every
\ record; the content key resolves regardless of registration order. The spawn/capture is the
\ maki/db/keywire-xproc-test fresh-process replay pattern.

require lib/test.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f          \ RUN-ARGV-CAPTURE
require maki/device-artifacts.f     \ MAKI-GRADE: private tmp driver path
require maki/db/diff-case-store-xproc-child.f   \ CSXP: STORE-ALL / VERIFY-ALL + shared builders

package CASESTORE-XPROC-TEST

create SROOT FS-PATH-CAP allot           \ the shared store root (absolute), embedded in the child driver
variable SROOT-U
create XP-OUT $2000 allot   create XP-ERR $1000 allot

: SROOT! ( -- )
   s" hb-casestore-xproc" TMPDIR-MKDIR {: a:ptr u:n :}
   a SROOT u BYTE-COPY  u SROOT-U !
   SROOT SROOT-U @ CASESTORE:ROOT! ;
: SROOT$ ( -- ptr u8 n )   SROOT SROOT-U @ ;

\ DRIVER!: the tiny spawn driver - load the child fixture, VERIFY-ALL against the store dir.
: DRIVER! ( -- )
   SB-RESET
   s" require maki/db/diff-case-store-xproc-child.f" SB-APPEND  $0A SB-APPEND-C
   s" s" SB-APPEND  $22 SB-APPEND-C  $20 SB-APPEND-C     \ emit:  s"<space>
   SROOT$ SB-APPEND  $22 SB-APPEND-C                     \ emit:  <storeroot>"
   s"  CSXP:VERIFY-ALL" SB-APPEND  $0A SB-APPEND-C
   MAKI-GRADE:DRIVER$ SB$ WRITE-ALL ;

\ CHILD-VERDICT$: spawn the fresh bin/hb child, return its captured stdout.
: CHILD-VERDICT$ ( -- ptr u8 n )
   PROC-ARGV-RESET
   s" --load"          >LEN PROC-ARGV+
   MAKI-GRADE:DRIVER$  >LEN PROC-ARGV+
   s" bin/hb" >LEN  XP-OUT $2000 >LEN  XP-ERR $1000 >LEN  30000 >MS  RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
        XP-OUT outu LEN>N ENDOF
     err OF PCAP-FAILED:UNMAKE {: outu:len erru:len c:rc :}
        XP-ERR erru LEN>N type cr                        \ surface child stderr on failure
        c RC>N 0 T=
        XP-OUT outu LEN>N ENDOF
   ;MATCH ;

T-RESET

s" habu-casestore-xproc" MAKI-GRADE:PREPARE
SROOT!
CSXP:STORE-ALL                        \ parent: durable records under the store dir
DRIVER!
CHILD-VERDICT$ s" CSXP-OK" T$=        \ fresh process rehydrated + byte-matched every record
MAKI-GRADE:CLEAN
SROOT$ REMOVE-TREE

;package

T-REPORT
