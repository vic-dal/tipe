open Types 
open Rules

let sym_to_cat (x:symbole) = match x with
	| Nature N -> "N"
	| Nature Adj -> "A"
	| Nature Adv -> "ADV"
	| Nature Det -> "DET"
	| Nature V -> "V" 
	| Nature Pro -> "PRO" 
	| Nature Intj -> "INTJ" 
	| Nature Pfx  -> "PFX" 
	| Nature ConjC -> "CONJC" 
	| Nature ConjS -> "CONJS" 
	| Nature Prep -> "PREP" 
	| Nature Num -> "NUM" 
	| Nature PDet -> "PDET" 
	| Nature Pres -> "PRES"
	| Syntagme GN -> "GN"
	| Syntagme GV -> "GV"
	| Syntagme GAdj -> "GAdj"
	| Syntagme GP -> "GP"
	| Syntagme S -> "S"

let rec applique_regle_ast (p: phrase) (r:symbole list): (ast list * phrase) list =
	match r with
	| [] -> [[], p]
	| sym :: r' -> 
		applique_symbole_ast p sym
		|> List.map (fun (ast, p') ->
			applique_regle_ast p' r'
			|> List.map (fun (asts, p'') -> (ast :: asts, p''))
		)
		|> List.concat
and applique_symbole_ast (p:phrase) (sym:symbole) : (ast * phrase) list =
	match sym with
	| Nature nat -> (
		match p with
		| [] -> []
		| m :: p' -> if cat_to_nat m.cat = nat then
			[Feuille (nat, m), p']
		else
			[]
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

let est_correct_phrase (p:phrase) = 
	(phrase_to_arbres p) <> []

let verif_flexion_arbre _ = true ;;

let verif_flexion_phrase _ = true ;;