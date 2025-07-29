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

(* Génère un tableau de graines pour les fonctions de hachage. *)
let generate_seeds k =
  Array.init k (fun i -> 17 + i * 23)

(* Crée un nouveau filtre de Bloom vide avec les paramètres optimaux. *)
let create_bloom n p =
  let m_bits = m n p in
  let k_hashes = k n m_bits in
  let int32_size = (m_bits + 31) / 32 in
  {
    bits = Array.make int32_size Int32.zero;
    size = m_bits;
    k = k_hashes;
    hash_seeds = generate_seeds k_hashes;
  }

(* Fonction de hachage pour le filtre de Bloom *)
let hash_i seed x size =
  let base = Hashtbl.hash (seed, x) in
  (abs base) mod size

(* Ajoute un élément au filtre de Bloom *)
let add bloom x =
  for i = 0 to bloom.k - 1 do
    let index = hash_i bloom.hash_seeds.(i) x bloom.size in
    set bloom index
  done

(* Vérifie si un élément est probablement dans le filtre *)
let check bloom x =
  let rec loop i =
    if i = bloom.k then true
    else
      let index = hash_i bloom.hash_seeds.(i) x bloom.size in
      if get bloom index then loop (i + 1) else false
  in
  loop 0


let () =
  let bloom = create_bloom 10000 0.01 in

  (* Ajouter des mots *)
  let words = ["chat"; "chien"; "oiseau"; "tigre"; "lion"] in
  List.iter (add bloom) words;

  (* Tester des mots présents *)
  Printf.printf "\n--- Tests d'inclusion ---\n";
  List.iter (fun w ->
    Printf.printf "Est-ce que \"%s\" est reconnu ? %b\n" w (check bloom w)
  ) words;

  (* Tester des mots absents *)
  let absent = ["dragon"; "requin"; "poule"] in
  Printf.printf "\n--- Tests de faux positifs ---\n";
  List.iter (fun w ->
    Printf.printf "Est-ce que \"%s\" est reconnu ? %b\n" w (check bloom w)
  ) absent
