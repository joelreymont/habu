\ image-grant.f - the keyed images a gate settled for this process's row.
\
\ A gate settles each keyed image once, in a build row of its pool
\ (test/gate-images.f), and starts a row only after every image the row needs
\ is settled. It names those images to the row in HABU_GATE_IMAGES, which the
\ row's own children inherit: each family name between spaces, with one space
\ before the first and after the last, so a row granted nothing still carries
\ the variable.
\
\ A family module asks CHECK before it settles its image. Outside a gate there
\ is no variable and any image may be settled: a standalone `bin/hb --load
\ <row>` builds whatever it needs. Under a gate only a granted image may be, so
\ a row that reaches an image the gate did not settle for it dies here, naming
\ the image, instead of building it beside the gate's build row.

require lib/errors.f
require lib/string.f

package IMAGE-GRANT

128 constant GRANT-CAP

create GRANT-BUF GRANT-CAP allot
variable GRANT-U

: APPEND ( ptr u8 n -- ) {: a:ptr u:n :}
   GRANT-U @ u + GRANT-CAP > if E-STR-CAPACITY throw then
   a GRANT-BUF GRANT-U @ + u BYTE-COPY
   GRANT-U @ u + GRANT-U ! ;

public

: NAME$ ( -- ptr u8 n )
   s" HABU_GATE_IMAGES" ;

\ The exit status of a row that settles an image it was not granted.
69 constant UNGRANTED-RC

\ Compose a grant for a child: RESET, then ADD each family, then VALUE$.
: RESET ( -- )
   0 GRANT-U !
   s"  " APPEND ;

: ADD ( ptr u8 n -- )
   APPEND
   s"  " APPEND ;

: VALUE$ ( -- ptr u8 n )
   GRANT-BUF GRANT-U @ ;

\ Die unless this process may settle the family's image.
: CHECK ( ptr u8 n -- ) {: fam:ptr famu:n :}
   NAME$ GETENV {: g:ptr gu:n :}
   gu 0 = if exit then
   SB-RESET
   s"  " SB-APPEND
   fam famu SB-APPEND
   s"  " SB-APPEND
   g gu SB$ CONTAINS? if exit then
   SB-RESET
   fam famu SB-APPEND
   s" : keyed image not granted to this gate row (" SB-APPEND
   NAME$ SB-APPEND
   s" =" SB-APPEND
   g gu SB-APPEND
   s" ); the gate grants a row the images its load closure reaches (test/gate-images.f)" SB-APPEND
   SB$ UNGRANTED-RC die ;

;package
