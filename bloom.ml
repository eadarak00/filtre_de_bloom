(* Type pour représenter le filtre de Bloom *)
type bloom = {
  bits: int32 array;        (* Tableau d'entiers 32 bits pour représenter les bits *)
  size: int;                (* Taille totale en bits *)
  k: int;                   (* Nombre de fonctions de hachage *)
  hash_seeds: int array;    (* Graines pour générer différentes fonctions de hachage *)
}

(* Calcul du nombre de bits m *)
let m n p =
  let n = float_of_int n in
  let log2 = log 2.0 in
  int_of_float (-.(n *. log p) /. (log2 ** 2.0))

(* Calcul du nombre de fonctions de hachage k *)
let k n m =
  let n = float_of_int n in
  let m = float_of_int m in
  int_of_float (log 2.0 *. m /. n)

(* Vérifie si le bit à la position [i] est activé (1) dans le filtre *)
let get bloom i =
  let index = i / 32 in
  let offset = i mod 32 in
  let mask = Int32.shift_left Int32.one offset in
  Int32.logand bloom.bits.(index) mask <> Int32.zero

(* Active le bit à la position [i] (le met à 1). *)
let set bloom i =
  let index = i / 32 in
  let offset = i mod 32 in
  let mask = Int32.shift_left Int32.one offset in
  bloom.bits.(index) <- Int32.logor bloom.bits.(index) mask


(* Fonction de test *)
let () =
  let n = 10_000 in  (* nombre estimé de mots *)
  let p = 0.01 in    (* taux de faux positifs (1%) *)

  let m_bits = m n p in
  let k_hashes = k n m_bits in

  Printf.printf "Pour n = %d mots et p = %.2f (faux positif) :\n" n p;
  Printf.printf "- Taille du filtre (m) : %d bits\n" m_bits;
  Printf.printf "- Nombre de hachages (k) : %d fonctions\n" k_hashes
