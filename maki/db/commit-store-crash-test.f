\ maki/db/commit-store-crash-test.f - the DECISIVE cross-process crash + recovery test for
\ the commit slice (dot habu-v2-atomic-txn-a3c26066; plan § 23 "Crash injection before the
\ commit marker publishes nothing; recovery either observes the old revision or the complete
\ new revision"; V2-2 exit "crash injection cannot publish partial state").
\
\ This process (the PARENT) writes a genesis head into a private SHARED store dir, then spawns
\ a FRESH bin/hb (the child, maki/db/commit-store-crash-child.f) that performs a PREFIX of the
\ real production publish (STAGE-REV / ADVANCE-HEAD / WRITE-IDEM) against that store and lets
\ the process DIE at that boundary. After the child exits, the parent reads the store in its
\ own image and asserts the recover-old-or-complete-new invariant by CONTENT KEY (the child's
\ proposed revision key equals the parent's, so both name the same revs/<revhex> and HEAD):
\   after-rev  (1 publish step)  -> HEAD still genesis (OLD); the staged object is a complete
\                                   orphan; the head was never advanced over it.
\   after-head (2 publish steps) -> HEAD is the new revision (COMPLETE NEW); its object is
\                                   complete on disk - never a partial revision.
\
\ The spawn/capture is the maki/db/keywire-xproc-env-test.f fresh-process pattern. There is
\ NO failpoint branch in the production commit path: the injection surface is the public
\ publish steps and the child running a prefix of them.

require lib/test.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f          \ RUN-ARGV-CAPTURE: the fresh-process crash child
require maki/device-artifacts.f     \ MAKI-GRADE: private tmp driver path (PREPARE/DRIVER$/CLEAN)
require maki/db/commit-store.f
require maki/db/transaction.f
require maki/artifact.f
require maki/rev.f
require maki/db/commit-store-crash-child.f  \ CSCRASH: shared fixtures + RUN-PARTIAL

package CSCRASH-TEST

create STOREPATH FS-PATH-CAP allot   variable STOREPATH-U
create XP-OUT $2000 allot   create XP-ERR $1000 allot

: STOREPATH$ ( -- ptr u8 n )   STOREPATH STOREPATH-U @ ;

: STOREPATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   a STOREPATH u BYTE-COPY  u STOREPATH-U ! ;

\ ---- write the child driver: require the child, run `steps` of the publish on the store ----
: DRIVER! ( n -- ) {: steps:n :}
   SB-RESET
   s" require maki/db/commit-store-crash-child.f" SB-APPEND  $0A SB-APPEND-C
   s" s" SB-APPEND  $22 SB-APPEND-C  $20 SB-APPEND-C     \ emit:  s"<space>
   STOREPATH$ SB-APPEND  $22 SB-APPEND-C                 \ emit:  <storepath>"
   $20 SB-APPEND-C  steps $30 + SB-APPEND-C              \ emit:  <space><digit steps>
   s"  CSCRASH:RUN-PARTIAL" SB-APPEND  $0A SB-APPEND-C
   MAKI-GRADE:DRIVER$ SB$ WRITE-ALL ;

\ ---- spawn the fresh-process crash child, return its stdout ------------------------------
: CHILD$ ( -- ptr u8 n )
   PROC-ARGV-RESET
   s" --load"             >LEN PROC-ARGV+
   MAKI-GRADE:DRIVER$     >LEN PROC-ARGV+
   s" bin/hb" >LEN  XP-OUT $2000 >LEN  XP-ERR $1000 >LEN  30000 >MS  RUN-ARGV-CAPTURE
   {: outu:len erru:len rc:rc :}
   rc RC>N 0 <> if XP-ERR erru LEN>N type cr then        \ surface child stderr on failure
   rc RC>N 0 T=
   XP-OUT outu LEN>N ;

\ ---- run the child at `steps` boundary and assert it completed the prefix ----------------
: CRASH-AT ( n -- )   DRIVER!  CHILD$ s" CSCRASH-DONE" T$= ;

: GENESIS ( -- )   CSTORE:RESET  CSCRASH:G0 CSTORE:INIT-HEAD ;

T-RESET

s" habu-cstore-crash" MAKI-GRADE:PREPARE
s" hb-cstore-crash-store" TMPDIR-MKDIR STOREPATH!
STOREPATH$ CSTORE:ROOT!

\ ---- boundary after-rev: child stages the object then dies -> recovery sees OLD -----------
GENESIS
1 CRASH-AT
CSCRASH:MK1 TX:PROPOSE                       \ the child's proposed revision (same key)
CSTORE:REV-COMPLETE?  TTRUE                  \ staged object is a complete orphan
CSCRASH:G0 CSTORE:HEAD-IS?  TTRUE            \ head still genesis (OLD)
CSCRASH:MK1 TX:PROPOSE CSTORE:HEAD-IS? 0=  TTRUE   \ head was NOT advanced -> no partial

\ ---- boundary after-head: child stages + advances the marker then dies -> COMPLETE NEW ----
GENESIS
2 CRASH-AT
CSCRASH:MK1 TX:PROPOSE CSTORE:HEAD-IS?  TTRUE      \ head is the new revision
CSCRASH:MK1 TX:PROPOSE CSTORE:REV-COMPLETE?  TTRUE \ new object complete -> no partial

CSTORE:RESET
MAKI-GRADE:CLEAN

T-REPORT

;package
