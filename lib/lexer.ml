open Types 

let trouver_mots (index : (string, mot list) Hashtbl.t) (cle : string) : mot list =
	Hashtbl.find_opt index cle |> Option.value ~default:[]