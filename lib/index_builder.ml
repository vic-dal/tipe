open Types

let ligne_vers_mot ligne =
	let rec pad acc l n =
		match n with
		| 0 -> List.rev acc
		| _ -> match l with
			| x :: xs -> pad (x :: acc) xs (n - 1)
			| [] -> pad ("" :: acc) [] (n - 1)
	in
	match pad [] ligne 19 with
	| [f; l; c; g; n; p; t; s; fo; gda; gr; la; cm; fl; d; u; r; se; ia] ->
			{
				forme = f; lemme = l; cat = c; genre = g; nb = n;
				pers = p; temps = t; spec = s; fonc = fo; gvn_dist_aux = gda;
				graph = gr; lang = la; cmp = cm; flx = fl; drv = d;
				unamb = u; ref = r; sem = se; info_accent = ia;
			}
	| _ -> failwith "Ligne mal formée"

let construire_index () : (string, mot list) Hashtbl.t =
	let table = Hashtbl.create 5000 in
	let ic = open_in "le_dm.csv" in
	let csv = Csv.of_channel ~has_header:true ic in
	let lignes = Csv.input_all csv in
	close_in ic;
	List.iter (fun ligne ->
		match ligne with
		| forme :: _ ->
				let m = ligne_vers_mot ligne in
				let anciens = Hashtbl.find_opt table forme |> Option.value ~default:[] in
				Hashtbl.replace table forme (m :: anciens)
		| [] -> ()
	) lignes;
	Hashtbl.iter (fun k v -> Hashtbl.replace table k (List.rev v)) table;
	table

let sauvegarder_index (index : (string, mot list) Hashtbl.t) (fichier : string) : unit =
	let oc = open_out_bin fichier in
	Marshal.to_channel oc index [];
	close_out oc

let charger_index (fichier : string) : (string, mot list) Hashtbl.t =
	let ic = open_in_bin fichier in
	let index : (string, mot list) Hashtbl.t = Marshal.from_channel ic in
	close_in ic;
	index