\ maki/experiment/run-lineage.f - the per-run lineage log, keyed by run identity and
\ composing with the append-only journal (MODEL-CAD-V2-PLAN.md § 23.4, plan:3296-3298
\ "a fresh agent process can resume a stopped experiment ... explain every input"; dot
\ habu-v2-experiment-run-7c1d1906).
\
\ CONCERN: an append-only association from a CAD-KIND:run-id to the ordered audit events
\ of its lineage. A SEPARATE concern from the run IDENTITY (maki/experiment/run.f, which
\ owns only the immutable content-addressed id) and from the raw occurrence JOURNAL
\ (maki/journal.f, which mints the events): this file is the run -> events index.
\
\ ---- RESUME THE SAME LINEAGE (intern semantics) --------------------------------------
\ The lineage bucket is keyed by the run's 32-byte CROSS-PROCESS content key (RUN:KEY>WIRE).
\ Because equal run keys intern to ONE run-id with ONE content key, two equal builds - even
\ across a process restart - resolve to the SAME bucket, so LINEAGE+ after a "resume"
\ appends to the existing history and LINEAGE-COUNT reflects every append. A DIFFERENT run
\ key digests differently, so its lineage is a distinct bucket.
\
\ ---- COMPOSES WITH THE JOURNAL --------------------------------------------------------
\ LINEAGE+ records the event through JOURNAL:APPEND, which mints the NEXT monotonic
\ audit-event-id (occurrence-identified). Two appends - even of an identical descriptor -
\ are two DISTINCT events, so the lineage is a genuine ordered occurrence log, never a
\ content-collapsed set. Each event is held WHOLE as a typed CAD-KIND:audit-event-id in
\ the bucket (never a raw), retrievable in order by LINEAGE-AT.
\
\ FIRST-SLICE POOL: a bounded set of live run buckets with a fixed per-run event cap; a
\ durable lineage store is a later dot. maki -> habu only; run-lineage owns -5621.

require lib/prelude.f
require maki/cad-kinds.f
require maki/journal.f              \ JOURNAL:APPEND mints the monotonic audit-event-id
require maki/experiment/run.f       \ RUN:KEY>WIRE - the run's cross-process content key

-5621 constant E-RLINEAGE-CAP       \ lineage buckets / per-run events over cap, or an event index out of range

package RLINEAGE
public

private

64 constant LIN-CAP                 \ max distinct live run buckets (first-slice pool)
64 constant EVT-CAP                 \ max lineage events per run
32 constant CKW                     \ run content-key width (RUN:KEY>WIRE)

create LK-KEYS LIN-CAP CKW * allot                          \ per-bucket run content key
LIN-CAP EVT-CAP * TYPED-BUFFER LK-EVT CAD-KIND:audit-event-id  \ per-(bucket,event) audit-event-id
create LK-N LIN-CAP cells allot                             \ per-bucket event count
variable LK-COUNT                                            \ live bucket count
create RKBUF CKW allot                                      \ scratch: one run's content key

: LK-KEY-AT ( n -- ptr u8 )   CKW * LK-KEYS + ;
: LK-N@ ( n -- n )            cells LK-N + @ ;
: LK-N! ( n n -- )            cells LK-N + ! ;
: LK-EVT-AT ( n n -- ptr CAD-KIND:audit-event-id ) {: b:n k:n :}   b EVT-CAP * k + LK-EVT ;

: CK-EQ? ( ptr u8 ptr u8 -- bool ) {: pa:ptr pb:ptr :}
   0 begin dup CKW < while
      dup {: k:n :}
      pa k + c@  pb k + c@  <> if drop false exit then
      1+
   repeat drop true ;

\ RUNKEY caches the run's content key in RKBUF (fail-closed via RUN:KEY>WIRE for a valid id).
: RUNKEY ( CAD-KIND:run-id -- ) {: id:CAD-KIND:run-id :}
   id RKBUF CKW RUN:KEY>WIRE drop ;

: BUCKET-FIND ( -- n )                            \ RKBUF's bucket index, or -1
   LK-COUNT @ 0 ?do
      RKBUF i LK-KEY-AT CK-EQ? if i unloop exit then
   loop -1 ;

: BUCKET-NEW ( -- n )                             \ append a fresh bucket for RKBUF
   LK-COUNT @ LIN-CAP >= if E-RLINEAGE-CAP throw then
   LK-COUNT @ {: b:n :}
   RKBUF  b LK-KEY-AT  CKW BYTE-COPY
   0 b LK-N!
   b 1+ LK-COUNT !
   b ;

: BUCKET-OF ( -- n )                              \ find-or-create RKBUF's bucket
   BUCKET-FIND dup 0 >= if exit then
   drop BUCKET-NEW ;

public

\ ---- LINEAGE+: record a lineage event for a run through the journal --------------
\ Mints the next monotonic audit-event-id, appends it to the run's ordered lineage, and
\ returns it. Equal run keys share one bucket, so appends compose across a resume.
: LINEAGE+ ( CAD-KIND:run-id ptr u8 n -- CAD-KIND:audit-event-id ) {: id:CAD-KIND:run-id a:ptr u:n :}
   id RUNKEY
   a u JOURNAL:APPEND {: ev:CAD-KIND:audit-event-id :}
   BUCKET-OF {: b:n :}
   b LK-N@ {: k:n :}
   k EVT-CAP >= if E-RLINEAGE-CAP throw then
   ev  b k LK-EVT-AT  !
   k 1+ b LK-N!
   ev ;

\ ---- LINEAGE-COUNT: the number of lineage events recorded for a run --------------
\ Zero for a run with no recorded lineage (never a throw).
: LINEAGE-COUNT ( CAD-KIND:run-id -- n ) {: id:CAD-KIND:run-id :}
   id RUNKEY
   BUCKET-FIND dup 0 < if drop 0 exit then
   LK-N@ ;

\ ---- LINEAGE-AT: the k-th recorded lineage event of a run (in append order) -------
: LINEAGE-AT ( CAD-KIND:run-id n -- CAD-KIND:audit-event-id ) {: id:CAD-KIND:run-id k:n :}
   id RUNKEY
   BUCKET-FIND dup 0 < if E-RLINEAGE-CAP throw then
   {: b:n :}
   k b LK-N@ >= if E-RLINEAGE-CAP throw then
   b k LK-EVT-AT @ ;

private

: RLINEAGE-INIT ( -- )   0 LK-COUNT ! ;
RLINEAGE-INIT

;package
