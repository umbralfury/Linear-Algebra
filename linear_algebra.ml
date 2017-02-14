(*An array is a list of lists, in row major order
  That is, each subordinate list is a row of the array*)

(*This function only checks for square arrays*)
(*Not fully tail recursive, uses maps to keep list order*)
  let is_array ary =
    let lens = List.map (List.fold_left (fun a x -> a+1) 0) ary in
    let eqs = List.map (fun x -> x = (List.hd lens)) lens in
    List.fold_left (fun a x -> a && x) true eqs;;

  (*A vector is simply a list of numbers*)

  (*Multiplies two vectors to get a dot product*)
  (*Not tail recursive*)
  let rec vec_mult v1 v2 =
    match v1 with
      h::t ->
        (match v2 with
            h1::t1 -> (h*.h1)+.(vec_mult t t1)
          | [] -> failwith "Vectors are not the same length")
    | [] ->
      (match v2 with
          [] -> 0.0
        | _::_ -> failwith "Vectors are not the same length");;

  (*Stitches together each row of the array times the vector*)
  (*Not tail recursive*)
  let rec mult_helper ary vec =
    match ary with
      h::t -> (vec_mult h vec)::(mult_helper t vec)
    | [] -> [];;

  (*Relies on non-tail recursive functions*)
  let mult ary vec=
    if is_array ary && ((List.fold_left (fun a x -> a+1) 0 ary) =
      (List.fold_left (fun a x -> a+1) 0 vec)) then
      mult_helper ary vec
      else failwith "Invalid array/vector combination";;

  (*Tail recursive on k, O(k*ary)*)
  let rec markov k ary vec =
    if k = 0 then vec else
    markov (k-1) ary (mult ary vec);;
