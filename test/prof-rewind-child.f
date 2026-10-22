\ Run in a child: the seed rewind removes the donor's package and long-name
\ record while its code and samples survive. The target takes the same ordinal.
0 set-check

variable REWIND-AT
variable DONOR-PC
variable DONOR-IX
TYPED-VARIABLE DONOR-CALLER-XT [ -- n ]
TYPED-VARIABLE RETIRED-XT [ -- ]
variable TRIM-IX
variable TRIM-PC
ndict@ REWIND-AT !

package PROF-REWIND-DONOR-QUALIFIER-LONG
public
: DONOR-HOT-WITH-EXTERNAL-NAME ( -- ) 100000000 0 ?do i drop loop ;
: DONOR-CALLER-WITH-EXTERNAL-NAME ( -- n ) DONOR-HOT-WITH-EXTERNAL-NAME 42 ;
: RETIRED-HOT ( -- ) 100000000 0 ?do i drop loop ;
;package
s" PROF-REWIND-DONOR-QUALIFIER-LONG:DONOR-HOT-WITH-EXTERNAL-NAME" XREF-FIND XREF-START DONOR-PC !
s" PROF-REWIND-DONOR-QUALIFIER-LONG:DONOR-HOT-WITH-EXTERNAL-NAME" XREF-FIND-INDEX DONOR-IX !
' PROF-REWIND-DONOR-QUALIFIER-LONG:DONOR-CALLER-WITH-EXTERNAL-NAME DONOR-CALLER-XT !
' PROF-REWIND-DONOR-QUALIFIER-LONG:RETIRED-HOT RETIRED-XT !

\ Starting a second session must release the first session's name chunks.
0 prof-on
prof-off
0 prof-on
PROF-REWIND-DONOR-QUALIFIER-LONG:DONOR-HOT-WITH-EXTERNAL-NAME

\ An ordinary retired word must lose its stored span before a later seed
\ rewind retains the surviving donors. Running its old xt must not revive it.
package PROF-REWIND-DONOR-QUALIFIER-LONG
public
undefine RETIRED-HOT
;package

REWIND-AT @ seed-ndict!
package PROF-REWIND-TARGET-QUALIFIER-LONG
public
: TARGET-NEW ( -- n ) 7 ;
;package
: VERIFY ( -- )
   s" PROF-REWIND-TARGET-QUALIFIER-LONG:TARGET-NEW" XREF-FIND-INDEX DONOR-IX @ <> if
      s" profiler rewind: ordinal was not reused" 74 die then
   RETIRED-XT @ execute
   DONOR-CALLER-XT @ execute 42 <> if
      s" profiler rewind: retained donor caller returned wrong value" 74 die then
   prof-off
   prof-json
   DONOR-PC @ prof-pc>rec -1 <> if
      s" profiler rewind: retired pc has a live record" 74 die then
   s" PROF-REWIND-TARGET-QUALIFIER-LONG:TARGET-NEW" XREF-FIND XREF-START prof-pc>rec DONOR-IX @ <> if
      s" profiler rewind: target pc has no live record" 74 die then
   DONOR-IX @ prof-row
   s" retained donor caller ran" type cr ;
VERIFY

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
1 set-tier
0 prof-on
: PROF-LATE-TRIM ( -- ) 100000000 0 ?do i drop loop ;
PROF-LATE-TRIM
prof-off
s" PROF-LATE-TRIM" HIDE-DEFS-FROM
prof-json

\ A seed rewind retains runnable donor code, but a later CP rewind releases
\ that code. Reuse must stop charging its old identity before the next report.
variable REUSE-CP
variable REUSE-ND
variable REUSE-PC
cp@ REUSE-CP !
ndict@ REUSE-ND !
: PROF-RECLAIM-DONOR ( -- ) 100000000 0 ?do i drop loop ;
s" PROF-RECLAIM-DONOR" XREF-FIND XREF-START REUSE-PC !
0 prof-on
REUSE-ND @ seed-ndict!
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
