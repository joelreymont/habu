\ Run in a child: trimming and rewinding reuse ordinals and code addresses, and
\ each report must charge only the live owner.

variable TRIM-IX
variable TRIM-PC

\ HIDE-DEFS-FROM reaches the ordinary ndict! lowering boundary. Its old
\ identity must leave the index before reuse, and the replacement starts fresh.
: VERIFY-HIDDEN ( -- )
   TRIM-PC @ prof-pc>rec -1 <> if
      s" profiler trim: hidden pc has a live record" 74 die then ;
: PROF-TRIM-OLD ( -- ) 100000000 0 ?do i drop loop ;
s" PROF-TRIM-OLD" XREF-FIND-INDEX TRIM-IX !
s" PROF-TRIM-OLD" XREF-FIND XREF-START TRIM-PC !
0 prof-on
PROF-TRIM-OLD
prof-off
s" PROF-TRIM-OLD" HIDE-DEFS-FROM
: PROF-TRIM-NEW ( -- n ) 7 ;
: VERIFY-TRIM ( -- )
   s" PROF-TRIM-NEW" XREF-FIND-INDEX TRIM-IX @ <> if
      s" profiler trim: ordinal was not reused" 74 die then
   prof-json
   s" PROF-TRIM-NEW" XREF-FIND XREF-START prof-pc>rec TRIM-IX @ <> if
      s" profiler trim: new pc has no live record" 74 die then
   TRIM-IX @ prof-row ;
VERIFY-TRIM
s" PROF-TRIM-NEW" HIDE-DEFS-FROM
prof-json
VERIFY-HIDDEN

\ Ordinary trimming must name pending samples before the dictionary rows go.
\ Otherwise the next report loses the only owner of these deferred PCs.
0 prof-on
: PROF-LATE-TRIM ( -- ) 100000000 0 ?do i drop loop ;
PROF-LATE-TRIM
prof-off
s" PROF-LATE-TRIM" HIDE-DEFS-FROM
prof-json

\ A CP rewind after an NDICT lowering releases the donor's code; the word that
\ reuses its address must not charge the donor's identity in the next report.
variable REUSE-CP
variable REUSE-ND
variable REUSE-PC
cp@ REUSE-CP !
ndict@ REUSE-ND !
: PROF-RECLAIM-DONOR ( -- ) 100000000 0 ?do i drop loop ;
s" PROF-RECLAIM-DONOR" XREF-FIND XREF-START REUSE-PC !
0 prof-on
REUSE-ND @ ndict!
REUSE-CP @ cp!
: PROF-RECLAIM-TARGET ( -- n ) 100000000 0 ?do i drop loop 42 ;
: VERIFY-RECLAIM ( -- )
   s" PROF-RECLAIM-TARGET" XREF-FIND XREF-START REUSE-PC @ <> if
      s" profiler reclaim: code address was not reused" 74 die then
   PROF-RECLAIM-TARGET 42 <> if
      s" profiler reclaim: target returned wrong value" 74 die then
   prof-off
   prof-json ;
VERIFY-RECLAIM
