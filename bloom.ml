 (* === Sprint 1 – Base du filtre === *)

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

(* ===================== FIN SPRINT 1 ============================== *)


(* === Sprint 2 – Ajout et vérification === *)

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

(* ===================== FIN SPRINT 2 ============================== *)


(* === Sprint 3 – Tests réels et comparaison === *)
(* Lit un fichier et retourne la liste de ses lignes/mots *)
let read_words filename =
  let ic = open_in filename in
  let acc = ref [] in
  try
    while true do
      acc := input_line ic :: !acc
    done;
    assert false (* Unreachable *)
  with End_of_file ->
    close_in ic;
    List.rev !acc

(* Ajoute tous les éléments d'une liste au filtre de Bloom *)
let add_all bloom words =
  List.iter (add bloom) words

(* Recherche dichotomique dans un tableau trié *)
let binary_search arr x =
  let rec aux low high =
    if low > high then false
    else
      let mid = (low + high) / 2 in
      let cmp = compare x arr.(mid) in
      if cmp = 0 then true
      else if cmp < 0 then aux low (mid - 1)
      else aux (mid + 1) high
  in
  aux 0 (Array.length arr - 1)

(* Mesure le temps d'exécution d'une fonction *)
let measure f x =
  let start = Sys.time () in
  let result = f x in
  let stop = Sys.time () in
  (result, stop -. start)
(* ===================== FIN SPRINT 3 ============================== *)


(* Programme de test principal *)
let () =
  (* Crée un filtre pour 10,000 éléments avec 1% de faux positifs *)
  let bloom = create_bloom 10000 0.01 in
  
  (* Ajoute quelques animaux *)
  let animals = ["chat"; "chien"; "oiseau"; "tigre"; "lion"] in
  List.iter (add bloom) animals;

  (* Teste les animaux connus *)
  Printf.printf "\n=== Tests des éléments ajoutés ===\n";
  List.iter (fun animal ->
    let present = check bloom animal in
    Printf.printf "\"%s\" dans le filtre? %b (devrait être vrai)\n" animal present;
    assert present (* Vérifie qu'ils sont tous reconnus *)
  ) animals;

  (* Teste des animaux non ajoutés *)
  let not_present = ["dragon"; "requin"; "poule"] in
  Printf.printf "\n=== Tests des éléments absents ===\n";
  List.iter (fun animal ->
    let present = check bloom animal in
    Printf.printf "\"%s\" dans le filtre? %b (devrait être faux)\n" animal present;
    if present then 
      Printf.printf "  ^ Faux positif! (acceptable avec 1%% de probabilité)\n"
  ) not_present;

  (* Affiche les statistiques *)
  Printf.printf "\n=== Statistiques du filtre ===\n";
  Printf.printf "Taille du filtre: %d bits\n" bloom.size;
  Printf.printf "Nombre de fonctions de hachage: %d\n" bloom.k;
  Printf.printf "Capacité estimée: 10,000 éléments\n";
  Printf.printf "Taux de faux positifs théorique: 1%%\n";

  (* test du dictionnaire *)
  Printf.printf "\n=== Test avec le dictionnaire réel ===\n";
  let words = read_words "./dictionary.txt" in
  let bloom_dict = create_bloom (List.length words) 0.01 in
  List.iter (add bloom_dict) words;

  let array_words = Array.of_list words in
  Array.sort compare array_words;

  let test_cases = ["tiger"; "banana"; "fantôme"; "asdfgh"; "zebre"; "dragon"] in
  List.iter (fun word ->
    let (b_res, b_time) = measure (check bloom_dict) word in
    let (s_res, s_time) = measure (fun w -> binary_search array_words w) word in
    Printf.printf "Mot : %-10s | Bloom: %b (%.6fs) | Binaire: %b (%.6fs)\n"
      word b_res b_time s_res s_time
  ) test_cases