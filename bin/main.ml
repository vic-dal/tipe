open Correcteur_lib.Correcteur

let rechercher_mots (index : (string, mot list) Hashtbl.t) (cle : string) : mot list =
	Hashtbl.find_opt index cle |> Option.value ~default:[]

let print_mot mot =
	Printf.printf "\n forme\t\t %s \n lemme\t\t %s \n cat\t\t %s \n genre\t\t %s \n nb\t\t %s \n pers\t\t %s \n temps\t\t %s \n spec\t\t %s \n fonc\t\t %s \n gvn_dist_aux\t\t %s \n graph\t\t %s \n lang\t\t %s \n cmp\t\t %s \n flx\t\t %s \n drv\t\t %s \n unamb\t\t %s \n ref\t\t %s \n sem\t\t %s \n info_accent%s\n\n" mot.forme mot.lemme mot.cat mot.genre mot.nb mot.pers mot.temps mot.spec mot.fonc mot.gvn_dist_aux mot.graph mot.lang mot.cmp mot.flx mot.drv mot.unamb mot.ref mot.sem mot.info_accent ;;

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
