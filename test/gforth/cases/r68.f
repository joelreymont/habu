\ A local declared between begin and while is gone after repeat (checked).
: W ( n -- n ) begin dup {: k :} k 0 > while 1 - repeat drop k ;
4 W . cr
