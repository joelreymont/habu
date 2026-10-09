\ A local declared between begin and while is gone after repeat (trusted).
trusted: W ( n -- n ) begin {: k :} k 0 > while k 1 - repeat k ;
4 W . cr
