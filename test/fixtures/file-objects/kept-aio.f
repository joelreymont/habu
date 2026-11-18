split-str
\ kept-aio.f - keeps an aio ticket in OLD, which a build saves: the build is rejected E-BAD-STORAGE
\ naming OLD, since a ticket names a record only the process that made it holds.

require src/lib/aio.f

package FILE-OBJECTS-KEPT-AIO
TYPED-VARIABLE OLD AIO:ticket
;package
