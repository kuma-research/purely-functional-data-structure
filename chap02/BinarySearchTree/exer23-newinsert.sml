
(* Solution for exercise 2.3 *)

signature SET =
sig
  type Elem
  type Set

  val empty   : Set
  val insert  : Elem * Set -> Set
  val member  : Elem * Set -> bool
end

structure CharSet : SET =
struct
  type Elem = char
  datatype Tree = E | T of Tree * Elem * Tree
  type Set = Tree

  val empty = E

  fun member (x, E) = false
    | member (x, T (a, y, b)) =
      if x > y then member (x, b)
      else if x < y then member (x, a)
      else true

  exception EXISTELEM

  fun insertHelper (x, E) = T (E, x, E)
    | insertHelper (x, s as T (a, y, b)) =
      if x > y then T (a, y, insertHelper (x, b))
      else if x < y then T (insertHelper (x, a), y, b)
      else raise EXISTELEM

  (* New insert, raise exception to break *)
  fun insert (x, E) = T (E, x, E)
    | insert (x, s as T (a, y, b)) = insertHelper (x, s)
    handle EXISTELEM => s;
end

(* Example code *)
val s = CharSet.empty;
val s1 = CharSet.insert (#"A", s);
val s2 = CharSet.insert (#"A", s1); (* should return only one "A" with no exception *)
