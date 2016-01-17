(* file: chap02/BinarySearchTree/set.sml - Set data structure *)

signature SET =
sig
  type Elem
  type Set

  val empty   : Set
  val insert  : Elem * Set -> Set
  val member  : Elem * Set -> bool
end

(* A basic implementation for SET, every element is a char *)
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
  fun insert (x, E) = T (E, x, E)
    | insert (x, s as T (a, y, b)) =
      if x > y then T (a, y, insert (x, b))
      else if x < y then T (insert (x, a), y, b)
      else s
end

(* Example code *)
val s = CharSet.empty;
val s1 = CharSet.insert (#"A", s);
val s2 = CharSet.insert (#"B", s1);
val s3 = CharSet.insert (#"C", s2);
