\ region-room-suite.f - the region's two headroom guards.
\
\ tools/region-room-probe.f answers one question for the dictionary band and the
\ code band: has this run left enough room that the cap still meets the sizing
\ rule layout.f states? The answer is a decision over two numbers (ROOM-OK? /
\ ?BAND) and a reading of the live engine (DICT-USED / CODE-USED), and this suite
\ tests them separately, because only the first can be driven to its boundary
\ without filling 52428 dictionary slots.
\
\ WHAT MAKES THIS MORE THAN A RESTATEMENT. The floor is not a number this file
\ pins; it is SOURCE-HEADROOM-PCT, the same constant layout.f sizes DICT-CAP,
\ CODE-BAND:BYTES and SOURCE-ARENA-CAP by. So the guard and the sizing rule are
\ tested
\ against EACH OTHER: for a measured composite, the smallest cap the RULE admits
\ - computed here from the composite, never by calling the probe's own FLOOR-OF -
\ must be exactly the smallest cap the GUARD admits. That pair reds if either side
\ drifts, including if SOURCE-HEADROOM-PCT moves and only one of them follows.
\
\ THE HISTORICAL CASE IS THE FALSIFICATION. The same guard, asked about the caps
\ master carried before dot habu-seeded-words-invisible-c7505a49, REFUSES: 26419
\ records of a 32768 cap is under the floor, and that is the debt the dictionary
\ carried unseeded, one lane before an AOT seed turned it into `hb: dictionary
\ full at: DLT-ROOT-U`. A guard that passed everything anyone had ever shipped
\ would prove nothing; this one reds on the tree it was written for.
\
\ THE REFUSAL LINES BELOW ARE EXPECTED OUTPUT. Every case that asserts a refusal
\ drives the production word, so the production message prints; that it prints is
\ part of what is being tested.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f test/region-room-suite.f

require lib/errors.f
require lib/string.f
require lib/test.f
require src/habu/layout.f
require tools/region-room-probe.f

package REGION-ROOM-TEST
private

\ The composite every number below is measured against: the monolithic maki
\ inventory, `bin/hb --load maki/test.f`, all 193 files in one image.
26419 constant MAKI-UNSEEDED       \ records, measured on bin/hb-host
33131 constant MAKI-SEEDED         \ records, measured on the seeded bin/hb
4047032 constant MAKI-UNSEEDED-CODE   \ code bytes, same run, same engine
5234436 constant MAKI-SEEDED-CODE     \ code bytes, same run, seeded
32768 constant OLD-CAP             \ DICT-CAP before this dot
$800000 constant OLD-REGION        \ REGION before this dot

\ Spelled apart from the locals below on purpose: names fold case, so a local
\ `used:n` IS the variable `USED` and the body would silently read the local.
variable RR-U
variable RR-C

\ Drive the production refusal and answer with the code it threw, or 0. The two
\ numbers go through variables because a quotation does not capture locals.
: FIRED ( n n -- n ) {: used:n cap:n :}
   used RR-U !  cap RR-C !
   [: RR-U @ RR-C @ s" band" REGION-ROOM:?BAND ;] catch ;

: REFUSED ( -- n ) REGION-ROOM:E-REGION-ROOM ;

\ The sizing rule itself: the smallest cap that clears a composite by
\ SOURCE-HEADROOM-PCT, rounded up. This is the arithmetic a lane does when it
\ picks a cap, written out here so the guard has something independent to agree
\ with.
: RULE-MIN-CAP ( n -- n ) {: composite:n :}
   composite 100 SOURCE-HEADROOM-PCT + *  99 +  100 / ;

\ ---- the guard against the sizing rule ---------------------------------------
: BOUNDARY-CASE ( -- )
   MAKI-UNSEEDED RULE-MIN-CAP {: umin:n :}
   MAKI-SEEDED   RULE-MIN-CAP {: smin:n :}

   s" the cap the rule admits for the unseeded composite, the guard admits"
   T-LABEL   MAKI-UNSEEDED umin FIRED 0 T=

   s" one below it, the guard refuses - the two boundaries are the same one"
   T-LABEL   MAKI-UNSEEDED umin 1 - FIRED REFUSED T=

   s" the same holds for the seeded composite, so it is not one lucky number"
   T-LABEL   MAKI-SEEDED smin FIRED 0 T=

   s" and one below that boundary refuses too" T-LABEL
   MAKI-SEEDED smin 1 - FIRED REFUSED T= ;

\ ---- the debt the guard was written to have caught ---------------------------
: HISTORICAL-CASE ( -- )
   s" master's unseeded dictionary was already under the floor at the old cap"
   T-LABEL   MAKI-UNSEEDED OLD-CAP FIRED REFUSED T=

   s" the raised cap admits the seeded composite this run now reaches" T-LABEL
   MAKI-SEEDED DICT-CAP FIRED 0 T=

   s" and it would have admitted the unseeded one - the cap was short, not the seed"
   T-LABEL   MAKI-UNSEEDED DICT-CAP FIRED 0 T= ;

\ ---- fixtures built to fool a numeric guard ----------------------------------
: ADVERSARIAL-CASE ( -- )
   s" an empty band has all of its room" T-LABEL
   0 DICT-CAP FIRED 0 T=

   s" a band used exactly to its cap has none" T-LABEL
   DICT-CAP DICT-CAP FIRED REFUSED T=

   s" one record past the cap refuses: the remainder is negative, not large"
   T-LABEL   DICT-CAP 1 +  DICT-CAP FIRED REFUSED T=

   s" twice the cap refuses for the same reason" T-LABEL
   DICT-CAP 2 *  DICT-CAP FIRED REFUSED T=

   s" a zero cap says nothing at zero use and refuses the first record" T-LABEL
   0 0 FIRED 0 T=

   s" use past a zero cap refuses rather than dividing its way to a pass" T-LABEL
   1 0 FIRED REFUSED T= ;

\ ---- the live reading ---------------------------------------------------------
\ The half a test cannot drive to its boundary: that the numbers handed to the
\ decision are the engine's own. Each assertion is a partition or an origin, and
\ an ordinary edit to the probe falsifies it - reading a cap where a count belongs,
\ or forgetting that the code band starts at DICT-SIZE and not at the region base.
: LIVE-CASE ( -- )
   s" the dictionary reading is the engine's own record count" T-LABEL
   REGION-ROOM:DICT-USED ndict@ T=

   s" dictionary use plus room is the cap" T-LABEL
   REGION-ROOM:DICT-USED REGION-ROOM:DICT-ROOM + DICT-CAP T=

   s" the code band is measured from DICT-SIZE, not from the region base" T-LABEL
   REGION-ROOM:CODE-USED DICT-SIZE + cp@ dbase@ - T=

   s" code use plus room is the code band" T-LABEL
   REGION-ROOM:CODE-USED REGION-ROOM:CODE-ROOM + CODE-BAND:BYTES T=

   s" this engine is above both floors, so REQUIRE-ROOM returns" T-LABEL
   [: REGION-ROOM:REQUIRE-ROOM ;] catch 0 T= ;

\ ---- the layout identities the two bands rest on ------------------------------
\ A lane that grows one of these and forgets another reds here, rather than in
\ whatever suite happens to run longest.
: LAYOUT-CASE ( -- )
   s" the record slots end exactly where the control-flow stack begins" T-LABEL
   DICT-CAP DREC * CFSTK-OFF T=

   s" the hash index still holds two slots per record slot" T-LABEL
   HIDX-SLOTS DICT-CAP 2 * T=

   s" and four bytes per slot" T-LABEL
   HIDX-BYTES HIDX-SLOTS 4 * T=

   s" the control-flow stack keeps its own page above the record slots" T-LABEL
   DICT-SIZE CFSTK-OFF - $1000 T=

   s" the compaction line is above the record cap" T-LABEL
   HIDX:LOAD-MAX DICT-CAP > T-ASSERT

   s" and below the table, so an insert always meets an empty slot" T-LABEL
   HIDX:LOAD-MAX HIDX-SLOTS < T-ASSERT ;

\ ---- what actually binds REGION ----------------------------------------------
\ CODE-BAND:BYTES is REGION minus DICT-SIZE, so asserting the three add up would
\ restate
\ its definition and constrain nothing - REGION could go back to $800000 and pass.
\ What binds REGION is the same rule that binds DICT-CAP, applied to the code
\ band's own measured composite. This case is also the lane's near miss, kept as
\ a test: the dictionary lift ALONE, with the old region, leaves a code band that
\ this very guard refuses.
: CODE-CASE ( -- )
   s" the code band clears the seeded composite by the sizing rule" T-LABEL
   MAKI-SEEDED-CODE CODE-BAND:BYTES FIRED 0 T=

   s" and the unseeded one, which the old region also cleared" T-LABEL
   MAKI-UNSEEDED-CODE OLD-REGION DICT-SIZE - FIRED 0 T=

   s" the grown dictionary inside the OLD region refuses - the near miss" T-LABEL
   MAKI-SEEDED-CODE OLD-REGION DICT-SIZE - FIRED REFUSED T= ;

public

: RUN ( -- )
   BOUNDARY-CASE
   HISTORICAL-CASE
   ADVERSARIAL-CASE
   LIVE-CASE
   LAYOUT-CASE
   CODE-CASE
   T-REPORT
   s" region-room: ok" type cr ;

;package

REGION-ROOM-TEST:RUN
