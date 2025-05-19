let gensym prefix =
  let count = ref (-1) in
  fun () ->
    incr count;
    prefix ^ string_of_int !count

