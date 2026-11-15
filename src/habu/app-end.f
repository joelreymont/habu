\ app-end.f - a stripped program's entry, loaded after the program's source.
\ The entry is the program's global MAIN, whose effect APP-ENTRY:XT! holds to
\ ( -- ). The build has checked both before this file loads: the checker
\ rejects a MAIN that is not ( -- ) at its declaration, and the build rejects a
\ program that defines none, so this line cannot fail.
' MAIN APP-ENTRY:XT!
