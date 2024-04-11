val len : 'a list -> int
val empty : unit -> 'a list
val isempty : 'a list -> bool
val fst : 'a list -> 'a
val lst : 'a list -> 'a
val nth : 'a list * int -> 'a
val add_fst : 'a list * 'a -> 'a list
val add_lst : 'a list * 'a -> 'a list
val add_nth : 'a list * 'a * int -> 'a list
val rem_fst : 'a list -> 'a list
val rem_lst : 'a list -> 'a list
val rem_nth : 'a list * int -> 'a list
val concat : 'a list * 'a list -> 'a list
type 'a t_btree = Vide | B_RT of ('a * 'a t_btree * 'a t_btree)
type boolean = FAUX | VRAI
val bt_empty : unit -> 'a t_btree
val bt_rooting : 'a * 'a t_btree * 'a t_btree -> 'a t_btree
val bt_isempty : 'a t_btree -> bool
val bt_root : 'a t_btree -> 'a
val bt_subleft : 'a t_btree -> 'a t_btree
val bt_subright : 'a t_btree -> 'a t_btree
val bt_size : int t_btree -> int
val bt_height : int t_btree -> int
val bst_seek : 'a t_btree * 'a -> boolean
val bst_insert : 'a t_btree * 'a -> 'a t_btree
val delete_min : 'a t_btree -> 'a t_btree * 'a
val bst_delete : 'a t_btree * 'a -> 'a t_btree
val bst_lbuild : 'a list -> 'a t_btree
val ab_desequilibre : int t_btree -> int
val bst_cut : 'a t_btree * 'a -> 'a t_btree * 'a t_btree
val print_val : int * int -> unit
val see_btree : int t_btree * int -> unit
val list_rnd_create : int -> int list
val bst_rnd_create : int -> int t_btree
val experimentation_2 : int -> unit
val suite_ordonne : int * int -> int list
val generate_n_suite : int * int -> int list
val experimentation_3 : int -> unit
val ab_rg : 'a t_btree -> 'a t_btree
val ab_rd : 'a t_btree -> 'a t_btree
val ab_rgd : 'a t_btree -> 'a t_btree
val ab_rdg : 'a t_btree -> 'a t_btree
val ab_reequilibre : int t_btree -> int t_btree
val ab_inserer : int t_btree * int -> int t_btree
val ab_supmax : int t_btree -> int t_btree
val ab_sup : int t_btree * int -> int t_btree
val create_aux : int t_btree * int -> int t_btree
val avl_rnd_create : int -> int t_btree
val avl_linsert : int list -> int t_btree
val avl_compte_rotations : int -> float
val experimentation_RT : int -> unit
