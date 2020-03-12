let get_indentation depth =
  let rec _get_indentation depth =
    if depth = 0 then
      ""
    else
      "    " ^ _get_indentation (depth - 1)
  in if depth < 0 then
    failwith "Internal error: get_indentation: depth can't be negative"
  else
    _get_indentation depth

let rec find_assoc key = function
    | (k, value) :: t -> if key = k then Some value else find_assoc key t
    | [] -> None

let rec find_bad_elt default expected = function
  | [] -> default
  | h :: t when h <> expected -> h
  | _ :: t -> find_bad_elt default expected t

let rec is_prefix word pref =
  let w_len = String.length word
  and p_len = String.length pref in
  p_len = 0 || w_len <> 0 && word.[0] = pref.[0]
               && is_prefix (String.sub word 1 (w_len - 1)) (String.sub pref 1 (p_len - 1))

(* Returns true if one word in the list starts with prefix *)
let rec is_list_prefix list pref = match list with
  | w :: t -> is_prefix w pref || is_list_prefix t pref
  | _ -> false ;;
