let rec cartesian ll = match ll with
  | [] -> [[]]
  | m1 :: ll' ->
    let possibles = cartesian ll' in
    let llls = List.map (fun m -> 
      List.map (fun s ->
          m :: s
        ) possibles
      ) m1 in
    List.concat llls