\ engine-size.f - the command line over tools/image-size-lib.f, whose header says
\ what the report means. Run: <engine> --load tools/engine-size.f -- <image>
\ The image may be a baked engine, a --repl snapshot or a stripped application.
\ The measurement lives in the library because tools/hb-build-lib.f requires it,
\ and a file that measures argv[0] as it loads cannot be required by a program
\ with arguments of its own.

require tools/image-size-lib.f

\ Loading with no argument defines the tool without measuring anything, so a
\ test can drive it; tools/imgdump.f has the same entry shape.
: ENGINE-SIZE-MAIN? ( -- )
   SCRIPT-ARGC 0 > if IMAGE-SIZE:RUN then ;

ENGINE-SIZE-MAIN?
undefine ENGINE-SIZE-MAIN?
