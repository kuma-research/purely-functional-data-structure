
(* Solution to exercise 2.2 *)

signature SET =
sig
  type Elem
  type Set

  val empty   : Set
  val insert  : Elem * Set -> Set
  val member  : Elem * Set -> bool
end

(* Function member has been rewritten *)
structure CharSet : SET =
struct
  type Elem = char
  datatype Tree = E | T of Tree * Elem * Tree
  type Set = Tree

  val empty = E

  (*
    memberHelper: take an additional t as the candidate
    why it will work? t will be updated only when x >= y, t could equal x or not
    if t equals x, then t will hold until x hits the bottom.
      Proof(informal): in the next layer:
        - if it's the bottom, then return t = x
        - else if x >= y, then y should equal t. Actually this is a set, impossible
        - else if x <  y, then t will not be updated.
    if t not equals x, then t will be updated hereafter.
  *)
  fun memberHelper (x, E, t) = t = x
    | memberHelper (x, T (a, y, b), t) =
      if x >= y then memberHelper (x, b, y)
      else memberHelper (x, a, t)

  fun member (x, E) = false
    | member (x, s as T (_, y, _)) = memberHelper (x, s, y)
    (* however y is useless here *)

  fun insert (x, E) = T (E, x, E)
    | insert (x, s as T (a, y, b)) =
      if x > y then T (a, y, insert (x, b))
      else if x < y then T (insert (x, a), y, b)
      else s
end

val s  = CharSet.empty;
val s1 = CharSet.insert (#"A", s);
val s2 = CharSet.insert (#"A", s1);
val s3 = CharSet.insert (#"C", s2);

CharSet.member (#"A", s1); (* true *)
CharSet.member (#"A", s2); (* true *)
CharSet.member (#"C", s3); (* true *)
CharSet.member (#"B", s3); (* false *)
