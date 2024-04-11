open Printf


(* ------------------------ *)
(*        listes            *)
(* ------------------------ *)


let len(l : 'a list) : int = List.length l ;;

let empty() : 'a list = 
  []
;;

let isempty(l : 'a list) : bool =
  l = []
;;

let fst(l : 'a list) : 'a =
  match l with
    [] -> failwith "error fst : list is empty"
  | hd::_ -> hd
;;

let rec lst(l : 'a list) : 'a =
  match l with
    [] -> failwith "error lst : list is empty"
  | hd::[] -> hd
  | _::tail -> lst(tail)
;;

let nth(l, k : 'a list * int) : 'a = 
  let rec nth1(l, k) =
    match l with
      []->  failwith "error nth : index out of bounds"
    | hd::tail -> if k = 0 then hd else nth1(tail,k-1)
  in
  if k < 0
  then failwith "error  nth : index must be positive"
  else nth1(l,k)
;;

let add_fst(l, e : 'a list * 'a) : 'a list = e::l ;;

let rec add_lst(l, e : 'a list * 'a) : 'a list =
  match l with
    [] -> [e]
  | hd::tail -> hd::add_lst(tail,e)
;;

let add_nth(l, e, k  : 'a list * 'a * int) : 'a list =
  let rec add_nth1(l, e, k) =
    match l with
      [] -> [e]
    | hd ::tail -> if k = 0 then e::l else hd::add_nth1(tail, e, k-1)
  in 
  if k < 0
  then failwith "error add_nth : index must be positive"
  else
  if k > len(l)
  then failwith "error add_nth : index out of bounds"
  else add_nth1(l,e,k)
;;

let rem_fst(l : 'a list) : 'a list = 
  match l with
    [] -> failwith "error rem_fst : list is empty"
  | _::tail -> tail
;;

let rec rem_lst(l : 'a list) : 'a list =
  match l with
    [] -> failwith "error rem_lst : list is empty"
  | [x] -> []
  | x::tail -> x::rem_lst(tail)
;;

let rem_nth(l, k : 'a list * int) : 'a list =
  let rec rem_nth1(l, k) =
    match l with
    | [] -> failwith "error rem_nth : index out of bounds"
    | hd:: tail -> if k = 0 then tail else hd::rem_nth1(tail, k-1)
  in
  if k < 0 
  then failwith "error rem_nth : index must be positive"
  else rem_nth1(l,k)
;;

let concat(l1, l2 : 'a list * 'a list) = l1 @ l2 ;;


(* ------------------------ *)
(*    Module des arbres     *)
(* ------------------------ *)


type 'a t_btree =  Vide  | B_RT of ('a * 'a t_btree * 'a t_btree);;
type boolean = FAUX|VRAI;;

let bt_empty() : 'a t_btree = Vide;;


let bt_rooting(x,g,d: 'a * 'a t_btree * 'a t_btree): 'a t_btree =
  B_RT(x,g,d)
;;


let bt_isempty( a: 'a t_btree) : bool =
  a = Vide
;;
let bt_root (a: 'a t_btree ) : 'a =
  match a with
    Vide -> failwith (" erreur : l'arbre est vide ")
  |B_RT(v,g,d) -> v
;; 


let bt_subleft(a: 'a t_btree) : 'a t_btree =
  match a with
    Vide -> failwith ("erreur : l'arbre est vide ")
  |B_RT(v,g,d)  -> g
;;

let bt_subright( a: 'a t_btree) : 'a t_btree =
  match a with
    Vide         -> failwith ("erreur : l'arbre est vide ")
  |B_RT(v,g,d)   -> d
;;

let rec bt_size (a : 'a t_btree) : 'a =
  match a with
    Vide -> 0
  |B_RT (x,g,d) -> 1 + bt_size (g) + bt_size (d)
;;

let rec bt_height(a: 'a t_btree): 'a =
  match a with
    Vide -> 0;
  |B_RT(x,g,d) -> 1 + max(bt_height g) (bt_height d)
;;


let rec bst_seek(a,e: 'a t_btree * 'a) : boolean =
  match a with
    Vide -> FAUX
  |B_RT(v,g,d) ->
      if e = v
      then VRAI
      else
      if e < v
      then bst_seek(g, e)
      else bst_seek(d,e)
;;

let rec bst_insert(a, e: 'a t_btree * 'a ) : 'a t_btree =
  match a with
    Vide -> bt_rooting(e,bt_empty(), bt_empty())
  |B_RT(v,g,d) ->
      if v > e
      then bt_rooting(v, bst_insert(g,e),d)
      else bt_rooting(v, g,bst_insert(d,e))
;;

let rec delete_min (a: 'a t_btree) : 'a t_btree * 'a =
  match a with
    Vide -> failwith "delete_min: Empty tree"
  | B_RT (v, Vide, d) -> (d,v)
  | B_RT (v, g, d) ->
      let (new_left, min_val : 'a t_btree * 'a) = delete_min (g) in
      (B_RT (v, new_left, d), min_val)
;;

let rec bst_delete(a,e : 'a t_btree * 'a) : 'a t_btree =
  match a with
    Vide -> Vide
  |B_RT(v,g,d) ->
      if e = v then
        match g, d with
        | Vide, _ -> d
        | _, Vide -> g
        | _, _ ->
            let (new_right, min_val: 'a t_btree * 'a) = delete_min (d) in
            B_RT(min_val, g, new_right)
      else if e < v
      then B_RT (v, bst_delete (g,e), d)
      else B_RT (v, g, bst_delete (d, e))
;;


let rec bst_lbuild(a : 'a list) : 'a t_btree =
  match a with
    [] -> bt_empty()
  |hd :: l -> bst_insert (bst_lbuild(l), hd)
;;

let ab_desequilibre (t: 'a t_btree) : int =
  if (bt_isempty(t))
  then 0
  else bt_height(bt_subleft(t)) - bt_height(bt_subright(t))
;;

let rec bst_cut(a,v : 'a t_btree * 'a): 'a t_btree * 'a t_btree =
  if (bt_isempty(a))
  then (bt_empty(), bt_empty())
  else
    let (r,fg,fd): 'a * 'a t_btree *'a t_btree = (bt_root(a), bt_subleft(a), bt_subright(a)) in
    if (v < r)
    then
      let (g1, d1) : 'a t_btree * 'a t_btree = bst_cut(fg,v) in
      (g1,bt_rooting(r,d1,fd))
    else
      let (g1, d1) : 'a t_btree * 'a t_btree = bst_cut(fd,v) in
      (bt_rooting(r,fg,g1), d1)
;;

let print_val(lvl, v : int * int) : unit =
  Printf.printf "LVL %d : %d\n%!" lvl v
;;

let rec see_btree(a, niveau : 'a t_btree * int) : unit =
  match a with
  | Vide -> ()
  | B_RT(v, g, d) -> (
    print_val(niveau, v);
    see_btree(g, niveau+1);
    see_btree(d, niveau+1);
  )
;;



(* ------------------------ *)
(*     DEBUT DU PROJET      *)
(* ------------------------ *)


(*****     ARBRE BINAIRE DE RECHERCHE    *****)

(* 1.1 *)

let rec list_rnd_create(n : 'a) : 'a list =
  Random.self_init();
  if n = 0
  then []
  else add_lst(list_rnd_create(n-1), Random.int(100))
;;

let rec bst_rnd_create(n) : 'a t_btree =
  if n = 0
  then Vide
  else bst_lbuild(list_rnd_create(n-1))
;;


(* 1.2 *)

let experimentation_2(t) : unit  =
  let suite : 'a list ref = ref [] in
  let i : int ref = ref 0 in
  let count : int ref = ref 0 in
  
  print_endline("Tableau de déqulibres des arbres binaires de recherche contruits à partir de suites de nombres entiers aléatoires: ");
  print_newline();
  
  while (!i) < 100  do(
    let bt : 'a t_btree = bst_rnd_create(t) in
  let deseq : int = ab_desequilibre(bt) in
  print_int(deseq);
  suite := add_fst(!suite, deseq);
  i:=(!i)+1;
  count := (!count) + 1;

  if (!count) = 10 then (
      print_newline ();
      count := 0;
   )
) done;

let size : int = len(!suite) in
let total : int ref = ref 0 in
while not(isempty(!suite)) do(
  total := (!total)+fst(!suite);
  suite := rem_fst(!suite);
)done;
    print_newline();
    print_string("total: ");
    print_int(!total);
    print_newline();
    print_string("size: ");
    print_int(size);
    print_newline();
    print_string("resultat: ");
    print_int((!total)/size)
;;



(* 1.3 *)

let rec suite_ordonne(n, rnd : int * int) : 'a list =
  if n = 0
  then []
  else add_lst(suite_ordonne(n-1,  rnd), rnd+n)
;;

let generate_n_suite(n,t : int * int) : 'a list =
 let size : int ref = ref (n+1+Random.int(5)) in
 let count : int ref = ref n in
 let newList : 'a list ref = ref [] in
 while (!count) <> 0 do(
   let suite : 'a list = suite_ordonne(!size, Random.int(20)) in
   newList := (!newList)@suite;
   count := !count -1;
   if t == 0 then ()
   else
     if t == 1
        then size := Random.int(5)
              else if t == 2
                      then size := (!size)+1
                                   else size := (!size)-1
 )
done;
 !newList;;


let experimentation_3(t : int) : unit  =
  let suite : 'a list ref = ref [] in
  let i : int ref = ref 0 in
  let count : int ref = ref 0 in
  
  print_endline("Tableau de déqulibres des arbres binaires de recherche contruits à partir de suites ordonnés: ");
  print_newline();
  
  while (!i) < 100  do(
  let ls : int list = generate_n_suite(5, t) in
  let bt : 'a t_btree = bst_lbuild(ls) in
  let deseq : int = ab_desequilibre(bt) in
  print_int(deseq);
  print_char(' ');
  suite := add_fst(!suite, deseq);
  i:=(!i)+1;
  count := (!count) + 1;

  if (!count) = 10 then (
      print_newline ();
      count := 0;
   )
  ) done;

  let size : int = len(!suite) in
  let total : int ref = ref 0 in
   while not(isempty(!suite)) do(
      total := (!total)+fst(!suite);
      suite := rem_fst(!suite);
    )done;
    print_newline();
    print_string("total: ");
    print_int(!total);
    print_newline();
    print_string("size: ");
    print_int(size);
    print_newline();
    print_string("resultat: ");
    print_int((!total)/size)
;;


(*******      ARBRE AVL     ******** *)

(* 2.1.1 *)



let ab_rg( a : 'a t_btree) : 'a t_btree =
  match a with
    Vide -> failwith ("Erreur")
  |B_RT(_,_,Vide) -> failwith ("Erreur")
  |B_RT(p, u, B_RT(q,v,w)) -> B_RT(q, B_RT(p,u,v), w)
;;

let ab_rd( a : 'a t_btree) : 'a t_btree =
  match a with
    Vide -> failwith ("Erreur")
  |B_RT(_, Vide, _) -> failwith ("Erreur")
  |B_RT(q, B_RT(p,u,v), w) -> B_RT(p, u, B_RT(q,v,w))
;;

let ab_rgd( a : 'a t_btree) : 'a t_btree =
  match a with
    Vide -> failwith ("Erreur")
  |B_RT(_,Vide,_) -> failwith ("Erreur")
  |B_RT(_,B_RT(_,_,Vide),_) -> failwith ("Erreur")
  |B_RT(r, B_RT(p,t,B_RT(q,u,v)),w) -> B_RT(q, B_RT(p,t,u), B_RT(r,v,w))
;;

let ab_rdg( a : 'a t_btree) : 'a t_btree =
  match a with
    Vide -> failwith ("Erreur")
  |B_RT(_,_,Vide) -> failwith ("Erreur")
  |B_RT(_,_,B_RT(_,Vide,_)) -> failwith ("Erreur")
  |B_RT(r, t, B_RT(p,B_RT(q,u,v),w)) -> B_RT(q, B_RT(r,t,u), B_RT(p,v,w))
;;

(* 2.1.2 *)

let rec ab_reequilibre(t: 'a t_btree) : 'a t_btree =
  if (ab_desequilibre(t) == 0) || (ab_desequilibre(t) == 1) || (ab_desequilibre(t) == -1)
  then t
  else
  if (ab_desequilibre(t) == 2) && (ab_desequilibre(bt_subleft(t)) == 1)
  then ab_rd(t)
  else
  if (ab_desequilibre(t) == 2) && (ab_desequilibre(bt_subleft(t)) == -1)
  then ab_rgd(t)
  else
  if (ab_desequilibre(t) == -2) && (ab_desequilibre(bt_subright(t)) == -1)
  then ab_rg(t)
  else ab_rdg(t)
;;


(* 2.1.3 *)

let rec ab_inserer(t, e : 'a t_btree * 'a ) : 'a t_btree =
  if (bt_isempty(t))
  then bt_rooting (e, bt_empty(), bt_empty())
  else
    let (r,g,d : 'a * 'a t_btree * 'a t_btree) = (bt_root(t), bt_subleft(t), bt_subright(t)) in
    if (e < r)
    then ab_reequilibre (bt_rooting(r, ab_inserer(g,e),d))
    else
    if (e > r)
    then ab_reequilibre (bt_rooting(r, g, ab_inserer(d,e)))
    else t
;;

let rec ab_supmax(t: 'a t_btree) : 'a t_btree =
  let (r,g,d : 'a * 'a t_btree * 'a t_btree) = (bt_root(t), bt_subleft(t), bt_subright(t))
  in
  if (bt_isempty(d))
  then g
  else ab_reequilibre (bt_rooting(r,g,ab_supmax(d)))
;;

let rec ab_sup(t, e : 'a t_btree * 'a) : 'a t_btree =
  let suppAbr : 'a t_btree = bst_delete(t, e) in
  ab_reequilibre(suppAbr)
;;

(*  2.1.4  *

On a utiliser l'opération bst_delete du module bst pour la fonction suppession dans un arbre AVL 

 *)


(*  2.2.1  *)

let rec create_aux (t, n : 'a t_btree * int) =
  if (n = 0 )
  then t
  else
    let random_value = Random.int(100) in create_aux(ab_inserer(t,random_value),n-1);;

let avl_rnd_create(n : int) : 'a t_btree = create_aux(bt_empty(), n)
;;

let avl_linsert (lis : 'a list) : 'a t_btree =
  let abr : 'a t_btree ref = ref (bt_empty()) in
  let curList : 'a list ref = ref lis in
  while not(List.is_empty (!curList)) do(
    abr := ab_inserer(!abr, (List.hd (!curList)));
    curList := List.tl (!curList);
    )done;
(!abr);;

(*  2.2.2  *)

let rec avl_compte_rotations (n: int) : float =
  if n = 0
  then 0.0
  else
    let t : 'a t_btree = avl_rnd_create (n) in
    let r : float = float_of_int(ab_desequilibre(t)) in
      (avl_compte_rotations (n - 1) +. r) /. float_of_int(n)
;;

let experimentation_RT(t : int) : unit  =
  let suite : 'a list ref = ref [] in
  let i : int ref = ref 0 in
  let count : int ref = ref 0 in
  
  print_endline("Tableau de rotaitons: ");
  print_newline();
  
  while (!i) < 100  do(
  let ls : int list = generate_n_suite(5, t) in
  let bt : 'a t_btree = avl_linsert(ls) in
  let deseq : int = ab_desequilibre(bt) in
  print_int(deseq);
  print_char(' ');
  suite := add_fst(!suite, deseq);
  i:=(!i)+1;
  count := (!count) + 1;

   if (!count) = 10 then (
      print_newline ();
      count := 0;
   )
  ) done;

  let size : int = len(!suite) in
  let total : int ref = ref 0 in
    while not(isempty(!suite)) do(
      total := (!total)+fst(!suite);
      suite := rem_fst(!suite);
    )done;
    print_newline();
    print_string("total: ");
    print_int(!total);
    print_newline();
    print_string("size: ");
    print_int(size);
    print_newline();
    print_string("resultat: ");
    print_int((!total)/size)
;;
