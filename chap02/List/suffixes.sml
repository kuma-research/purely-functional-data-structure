
(* file: chap02/suffixes.sml - Answer to exercise 2.1 *)

fun suffixes nil = [[]]
    (* If it takes nil, return the initial list of lists *)
  | suffixes xs = xs :: (suffixes (tl xs))
    (* If it takes a list, pass the tail to ther next iteration and prepend
    the current one *)

(* Time: This function is O(n) as it will only execute for n times when the list
has length n.
  Space: The result is a list of n pointers, to each of n positions in the list.
*)
