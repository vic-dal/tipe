type mot = {
	forme : string;
	lemme : string;
	cat : string;
	genre : string;
	nb : string;
	pers : string;
	temps : string;
	spec : string;
	fonc : string;
	gvn_dist_aux : string;
	graph : string;
	lang : string;
	cmp : string;
	flx : string;
	drv : string;
	unamb : string;
	ref : string;
	sem : string;
	info_accent : string;
}

type phrase = mot list

type nature =
	| Det | N | Adj | V | Adv | Pro | Intj
	| Pfx | ConjC | ConjS | Prep | Num | PDet | Pres

type syntagme =
	| S | GN | GV | GAdj | GP

type symbole =
	| Nature of nature
	| Syntagme of syntagme

type ast =
	| Noeud of syntagme * ast list
	| Feuille of nature * mot

	
exception Categorie_Inconnue of string
let cat_to_nat = function
	| "N" -> N
	| "A" -> Adj
	| "ADV" -> Adv
	| "DET" -> Det
	| "V" -> V
	| "PRO" -> Pro
	| "INTJ" -> Intj
	| "PFX" -> Pfx
	| "CONJC" -> ConjC
	| "CONJS" -> ConjS
	| "PREP" -> Prep
	| "NUM" -> Num
	| "PDET" -> PDet
	| "PRES" -> Pres
	| s -> raise (Categorie_Inconnue s)

let nat_to_cat = function
	| N -> "N"
	| Adj -> "A"
	| Adv -> "ADV"
	| Det -> "DET"
	| V -> "V" 
	| Pro -> "PRO" 
	| Intj -> "INTJ" 
	| Pfx  -> "PFX" 
	| ConjC -> "CONJC" 
	| ConjS -> "CONJS" 
	| Prep -> "PREP" 
	| Num -> "NUM" 
	| PDet -> "PDET" 
	| Pres-> "PRES"

let string_of_syntagme = function
	| S -> "S"
	| GN -> "GN"
	| GV -> "GV"
	| GAdj -> "GAdj"
	| GP -> "GP"

let print_mot_detail mot =
	Printf.printf
	"\n forme\t\t %s \n lemme\t\t %s \n cat\t\t %s \n genre\t\t %s \n nb\t\t %s \n pers\t\t %s \n temps\t\t %s \n spec\t\t %s \n fonc\t\t %s \n gvn_dist_aux\t\t %s \n graph\t\t %s \n lang\t\t %s \n cmp\t\t %s \n flx\t\t %s \n drv\t\t %s \n unamb\t\t %s \n ref\t\t %s \n sem\t\t %s \n info_accent%s\n\n"
	mot.forme
	mot.lemme
	mot.cat
	mot.genre
	mot.nb
	mot.pers
	mot.temps
	mot.spec
	mot.fonc
	mot.gvn_dist_aux
	mot.graph
	mot.lang
	mot.cmp
	mot.flx
	mot.drv
	mot.unamb
	mot.ref
	mot.sem
	mot.info_accent ;;

let print_mot m =
	Printf.printf "%s (%s - %s %s %s %s)\n" m.forme m.cat m.genre m.nb m.pers m.temps

let print_phrase = List.iter print_mot

let print_arbre =
	let print_indent n = for _ = 1 to n do
		print_string "│ "
	done in
	let rec print_arbre indent = function
	| Feuille (nat, mot) -> 
			print_indent indent;
			print_string (nat_to_cat nat);
			print_newline ();
			print_indent (indent);
			print_string "└ ";
			print_mot mot; 
	| Noeud (syn, children) -> 
			print_indent indent;
			print_string (string_of_syntagme syn);
			print_newline ();
			List.iter (print_arbre (indent + 1)) children 
	in
	print_arbre 0