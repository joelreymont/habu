\ app-end.f - a stripped program's entry, loaded after the program's source.
\ The entry runs the program's global MAIN, whose effect is ( -- ): the checker
\ rejects a MAIN that is not ( -- ) at its declaration, and the build rejects a
\ program that defines none. The entry ends a program as hb ends a source
\ (src/habu/main.f RUN-SOURCE): a code from 1 to 255 is the exit status the
\ program chose; a throw that carries a record prints it and exits UNCAUGHT-RC;
\ any other throw goes on to the kernel, which reports its code.
package APP-END
private

: ENTRY ( -- )
   ['] MAIN catch {: rc:n :}
   rc 0= if exit then
   rc 0 >  rc 256 <  and if rc throw then
   REJECTION:CARRIED? 0= if rc throw then
   REJECTION:SEND-SAVED
   UNCAUGHT-RC throw ;

' ENTRY APP-ENTRY:XT!
;package
