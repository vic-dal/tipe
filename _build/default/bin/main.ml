open Correcteur_lib.Correcteur

let rechercher_mots (index : (string, mot list) Hashtbl.t) (cle : string) : mot list =
	Hashtbl.find_opt index cle |> Option.value ~default:[]


let rec print_phrase (phrase:mot list) = match phrase with
	| m::q -> Printf.printf "%s (%s - %s %s %s %s)\n" m.forme m.cat m.genre m.nb m.pers m.temps; print_phrase q
	| _ -> ();;

let () =
	let fichier_index = "index.msh" in
	let index =
		if Sys.file_exists fichier_index then
			charger_index fichier_index
		else
			let idx = construire_index () in
			sauvegarder_index idx fichier_index;
			idx
	in

	let mots = rechercher_mots index "souris" in
	print_phrase mots ;
