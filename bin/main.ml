open Correcteur_lib.Correcteur

let counter = ref 0 ;;
let next () = counter := !counter + 1; !counter ;;

let index = charger_index "data/index.msh" ;;

let phrases_possibles orig =
	(* Liste des phrases possibles (produit cartesien) *)
	let choix_mots = List.map (trouver_mots index) orig in
	cartesian choix_mots

let arbre_to_graphviz ast =
	let create_node label =
		let i = next () in
		let name = Printf.sprintf "node_%d" i in
		Printf.printf "%s[label=\"%s\"]\n" name label;
		name
	in

	let create_leaf label =
		let i = next () in
		let name = Printf.sprintf "leaf_%d" i in
		Printf.printf "%s[label=\"%s\",shape=\"plaintext\"]\n" name label;
		name
	in

	let wire_nodes node_from node_to =
		Printf.printf "%s->%s\n" node_from node_to
	in

	let rec traverse = function
	| Feuille (nat, m) ->
		let node_nat = create_node (nat_to_cat nat) in
		let leaf_m = create_leaf m.forme in
		wire_nodes node_nat leaf_m;
		node_nat
	| Noeud (syn, children) ->
		let node_syn = create_node (string_of_syntagme syn) in
		List.iter (fun child ->
			let node_child = traverse child in
			wire_nodes node_syn node_child
		) children;
		node_syn

	in
	Printf.printf "subgraph {\n" ;
	let _ = traverse ast in
	Printf.printf "}\n" ;
	()

let () =
	(* let fichier_index = "data/index.msh" in
	let index =
		if Sys.file_exists fichier_index then
			charger_index fichier_index
		else
			let idx = construire_index () in
			sauvegarder_index idx fichier_index;
			idx
	in *)
	

	let phrase = "la bestiole mange une crêpe" in


	let phrase_orig = String.split_on_char ' ' phrase in
	let phrases = phrases_possibles phrase_orig in

	let nombre_success = ref 0 in

	Printf.printf "digraph {\n" ;
	List.iter (fun ph ->
		let arbres = phrase_to_arbres ph in
		List.iter (fun ast ->
			arbre_to_graphviz ast;
			nombre_success := !nombre_success + 1;
		) arbres;
		print_newline();
	) phrases ;
	Printf.printf "}\n" ;

	if !nombre_success = 1 then
		Printf.eprintf "\nJ'ai reconnu un arbre syntaxique.\n"
	else
		Printf.eprintf "\nJ'ai reconnu %d arbres syntaxiques.\n" (!nombre_success)
