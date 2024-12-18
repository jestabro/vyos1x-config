module type T =
    sig
        type t

        val name_of : t -> string
        val children_of : t -> t list
        val find_child : t -> t -> t option
        val insert_child : t -> t -> t
        val replace_child : t -> t -> t
        val (^~) : t -> t -> bool
    end

module type TA = functor (M : T) ->
    sig
      val tree_union : (M.t -> M.t -> M.t) -> M.t -> M.t -> M.t
    end

module Make : TA
