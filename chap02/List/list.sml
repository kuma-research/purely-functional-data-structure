(* file: chap02/List/list.sml - implementation of list and related functions *)

(* type 'a Stack is a parameterised type, 'a is pronounced as Alpha *)
signature STACK =
sig
  type 'a Stack

  val empty   : 'a Stack
  val isEmpty : 'a Stack -> bool

  val cons    : 'a * 'a Stack -> 'a Stack
  val head    : 'a Stack -> 'a       (* raise EMPTY if stack is empty *)
  val tail    : 'a Stack -> 'a Stack (* raise EMPTY if stack is empty *)
end

exception EMPTY

(*
  type 'a Stack has been implemented differently in two structures: 
  type 'a Stack = 'a list is an abbreviation
  datatype 'a Stack will declare the constructors
*)
structure List : STACK =
struct
  type 'a Stack = 'a list

  val empty = []
  fun isEmpty s = null s

  fun cons (x, s) = x :: s
  fun head s = hd s
  fun tail s = tl s
end

structure CustomStack : STACK =
struct
  datatype 'a Stack = NIL | CONS of 'a * 'a Stack

  val empty = NIL
  fun isEmpty NIL = true
    | isEmpty _   = false

  fun cons (x, s) = CONS (x, s)
  fun head NIL    = raise EMPTY
    | head (CONS (x, s)) = x
  fun tail NIL    = raise EMPTY
    | tail (CONS (x, s)) = s
end

exception SUBSCRIPT

fun update ([], _, _) = raise SUBSCRIPT
  | update ((x::xs), 0, y) = y::xs
  | update ((x::xs), i, y) = x::(update (xs, i-1, y))
