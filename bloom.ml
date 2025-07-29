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

(* Fonction de test *)
let () =
  let n = 10_000 in  (* nombre estimé de mots *)
  let p = 0.01 in    (* taux de faux positifs (1%) *)

  let m_bits = m n p in
  let k_hashes = k n m_bits in

  Printf.printf "Pour n = %d mots et p = %.2f (faux positif) :\n" n p;
  Printf.printf "- Taille du filtre (m) : %d bits\n" m_bits;
  Printf.printf "- Nombre de hachages (k) : %d fonctions\n" k_hashes
