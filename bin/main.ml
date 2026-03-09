open Correcteur_lib.Correcteur

let counter = ref 0 ;;
let next () = counter := !counter + 1; !counter ;;

let index = charger_index "data/index.msh" ;;

let phrases_possibles orig =
	List.map (fun w -> { flexions = trouver_mots index w }) orig
	

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


let nb_arbres_phrase_string phrase =
	try 
		let mots = String.split_on_char ' ' (String.trim (String.lowercase_ascii phrase)) in
		let phrase_flex = phrases_possibles mots in
		let arbres = phrase_to_arbres phrase_flex in
		List.iter arbre_to_graphviz arbres;
		List.length arbres
	with Categorie_Inconnue _ -> 0
;;

let () =
	Printf.printf "digraph {\n" ;
	[
		"le plancher est sorti de la presse";
	]
	|> List.filter_map (fun phrase -> 
		Some (Printf.sprintf "(%d) %s" (nb_arbres_phrase_string phrase) phrase);
	)
	|> List.iter (fun s -> Printf.eprintf "%s\n" s); 
	Printf.printf "}\n" ;