split-str
\ kept-aio.f - keeps an aio ticket in OLD, which a build saves: the build is rejected LINKER:E-ADDRESS
\ naming OLD, since a ticket is the address of a record only the process that made it holds. The
\ ticket is cancelled and awaited and the loop stopped, so no task runs at the file's end.

require src/lib/aio.f

package FILE-OBJECTS-KEPT-AIO
TYPED-VARIABLE OLD AIO:ticket
AIO:START  1000000 >MS AIO:TIMEOUT OLD !
OLD @ AIO:CANCEL  OLD @ AIO:AWAIT drop  AIO:STOP
;package

: MAIN ( -- ) ;
