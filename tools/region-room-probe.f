\ region-room-probe.f - read-only view of the headroom in the region's two bands.
\
\ The region mapping holds the dictionary records at [DBASE, DBASE+CFSTK-OFF) and
\ the emitted code at [DBASE+DICT-SIZE, DBASE+REGION). They share one budget, and
\ each has a hard engine guard at its far end: a publication past DICT-CAP dies
\ `hb: dictionary full at: <token>` rc 77, and an emit past the code band dies
\ `hb: code space full` - both at whatever the process happened to be compiling.
\
\ NEITHER GUARD NAMES THE RUN THAT SPENT THE SPACE. The dictionary filled during
\ maki/examples/nanogpt/tokenizer-test.f and the message named DLT-ROOT-U in the
\ NEXT file, which runs green standalone (dot habu-seeded-words-invisible-c7505a49);
\ before that, the same shape blamed an innocent enum for two days when the
\ protected-wordlist registry filled. This is the same guard tools/prot-wid-probe.f
\ is for, over the other two capacities, and a long-running suite ends with both.
\
\ It reads and never writes. REQUIRE-ROOM reports whether or not the floor holds,
\ so a run that is merely approaching a wall still leaves the trend in its log.

require lib/errors.f
require lib/string.f
require lib/fmt.f

package REGION-ROOM

public

\ What the two bands hold right now. Records come from the engine's own count;
\ code bytes are CP measured from the band's start, not from DBASE, so the number
\ is what the emitter has spent rather than what the layout reserved ahead of it.
: DICT-USED ( -- n ) ndict@ ;
: CODE-USED ( -- n ) cp@ dbase@ - DICT-SIZE - ;

: DICT-ROOM ( -- n ) DICT-CAP DICT-USED - ;
: CODE-ROOM ( -- n ) CODE-BAND:BYTES CODE-USED - ;

\ The floors, stated as a fraction of each bound rather than as a measured
\ number. SOURCE-HEADROOM-PCT is the rule layout.f already sizes DICT-CAP,
\ CODE-BAND:BYTES and SOURCE-ARENA-CAP by: a cap must exceed its largest
\ composite by
\ that much. A run therefore has room left when it has not spent more than
\ 100/(100+PCT) of the bound, and the floor is the rest of it. Deriving both
\ floors from that one constant is what keeps this guard and the sizing rule the
\ same rule - a hand-picked floor would drift from the caps it is guarding, and a
\ measured one would have to be re-pinned by every lane that adds a definition.
: FLOOR-OF ( n -- n ) {: cap:n :}
   cap  cap 100 * 100 SOURCE-HEADROOM-PCT + /  - ;

: DICT-FLOOR ( -- n ) DICT-CAP FLOOR-OF ;
: CODE-FLOOR ( -- n ) CODE-BAND:BYTES FLOOR-OF ;

\ THE DECISION, taken apart from the reading so a test can drive it with numbers
\ instead of having to fill a band to see it refuse. Both bands ask it the same
\ question, and the historical composites answer it: (26410, 32768) - the maki
\ inventory against the cap this dot raised - is FALSE, which is the debt master
\ carried unseeded before anything was seeded into it.
: ROOM-OK? ( n n -- bool ) {: used:n cap:n :}
   cap used -  cap FLOOR-OF  >= ;

\ One line of evidence per run: what each band holds, what is left, and the bound
\ and floor that make those numbers mean something.
: REPORT ( -- )
   s" region headroom: dict " type DICT-USED FMT:.U s" /" type DICT-CAP FMT:.U
   s"  records, " type DICT-ROOM FMT:.U s"  left (floor " type DICT-FLOOR FMT:.U
   s" ); code " type CODE-USED FMT:.U s" /" type CODE-BAND:BYTES FMT:.U
   s"  bytes, " type CODE-ROOM FMT:.U s"  left (floor " type CODE-FLOOR FMT:.U
   s" )" type cr ;

\ Public so a caller that wants to catch the guard can name the code rather than
\ match a message.
-7211 constant E-REGION-ROOM   \ a region band's headroom fell below its floor

\ The refusal, one word for both bands and named by the band it refuses for. Takes
\ the numbers rather than reading them, so a test drives the real refusal instead
\ of having to fill 52428 dictionary slots to watch it happen.
: ?BAND ( n n ptr u8 n -- ) {: used:n cap:n name:ptr nameu:n :}
   used cap ROOM-OK? if exit then
   s" region-room-probe: " type name nameu type
   s"  headroom below the floor" type cr
   E-REGION-ROOM throw ;

: REQUIRE-ROOM ( -- )
   REPORT
   DICT-USED DICT-CAP s" dictionary" ?BAND
   CODE-USED CODE-BAND:BYTES s" code" ?BAND ;

;package
