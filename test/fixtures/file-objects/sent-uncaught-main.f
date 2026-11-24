\ sent-uncaught-main.f - its top level catches a throw and hands the code to
\ ERROR:UNCAUGHT, whose record fails the build.

: TRY ( -- ) [: 12345 throw ;] catch {: rc:n :} rc 0<> if rc ERROR:UNCAUGHT then ;
TRY

: MAIN ( -- ) ;
