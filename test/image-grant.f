\ image-grant.f - the keyed images a gate settled for this process's row.
\ A gate settles each keyed image once, in a build row of its pool
\ (test/gate-images.f), and starts a row only after its images are settled. It
\ names them in HABU_GATE_IMAGES, inherited by the row's children: each family
\ name between spaces, with one space before the first and after the last, so a
\ row granted nothing still carries the variable.

require src/lib/errors.f
require src/lib/string.f

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

\ A family module calls this before it settles its image. Outside a gate there
\ is no variable and any image may be settled; under one, an image not granted
\ dies here, naming it, instead of being built beside the gate's build row.
: CHECK ( ptr u8 n -- ) {: fam:ptr famu:n :}
   NAME$ GETENV {: g:ptr gu:n :}
   gu 0 = if exit then
   0 begin dup gu < while
      {: at:n :}
      g gu 32 at SPLIT-NEXT drop {: token:ptr tokenu:n next:n :}
      at 0 > next gu <= and token tokenu fam famu STR= and if exit then
      next
   repeat drop
   SB-RESET
   fam famu SB-APPEND
   s" : keyed image not granted to this gate row (" SB-APPEND
   NAME$ SB-APPEND
   s" =" SB-APPEND
   g gu SB-APPEND
   s" ); the gate grants a row the images its load closure reaches (test/gate-images.f)" SB-APPEND
   SB$ UNGRANTED-RC die ;

;package
