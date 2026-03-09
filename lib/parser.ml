open Types 
open Rules

let sym_to_cat (x:symbole) = match x with
	| Nature nat -> nat_to_cat nat
	| Syntagme syn -> string_of_syntagme syn

let rec applique_regle_ast (p: phrase_flex) (r:symbole list): (ast list * phrase_flex) list =
	match r with
	| [] -> [[], p]
	| sym :: r' -> 
		applique_symbole_ast p sym
		|> List.map (fun (ast, p') ->
			applique_regle_ast p' r'
			|> List.map (fun (asts, p'') -> (ast :: asts, p''))
		)
		|> List.concat
and applique_symbole_ast (p:phrase_flex) (sym:symbole) : (ast * phrase_flex) list =
	match sym with
	| Nature nat -> (
		match p with
		| [] -> []
		| flex :: p' -> (
			match List.find_opt (fun m -> cat_to_nat m.cat = nat) flex.flexions with
			| None -> []
			| Some m -> [Feuille (nat, m), p']
		)
	)
	| Syntagme syn -> (
		rules syn
		|> List.map (fun r ->
			applique_regle_ast p r 
			|> List.map (fun (l, p') -> (Noeud (syn, l), p'))
		)
		|> List.concat
	)


let phrase_to_arbres p =
	applique_symbole_ast p (Syntagme S)
		|> List.filter_map (fun (ast, p') -> match p' with
			| [] -> Some ast
			| _ -> None
		)
let arbre_to_phrase _ = ();;


let est_correct_arbre _ = true ;;

let est_correct_phrase (p:phrase_flex) = 
	(phrase_to_arbres p) <> []

let verif_flexion_arbre _ = true ;;

let verif_flexion_phrase _ = true ;;