module StringMap = Map.Make(String)
module StringSet = Set.Make(String)


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
  | h :: _ when h <> expected -> h
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

let rec append_rev l1 l2 =
  match l2 with
  | [] -> l1
  | h :: t -> append_rev (h :: l1) t

let split_list ?(from = 0) ?(to_ = max_int) sep list =
  let rec _split_list ?(current = []) ?(i = 0) list =
    match list with
    | [] -> if current = [] then [] else [current]
    | _ when i > to_ -> if current = [] then [] else [current]
    | _ :: t when i < from -> _split_list t ~i: (i + 1)
    | h :: t when h = sep -> current :: _split_list t ~i: (i + 1)
    | h :: t -> _split_list t ~current: (current @ [h]) ~i: (i + 1)
  in _split_list list

let list_of_queue queue = List.init (Queue.length queue) (fun _ -> Queue.take queue)

(* Create a map from a list of tuples *)
let rec string_map_of_list = function
  | [] -> StringMap.empty
  | (key, value) :: t -> StringMap.add key value (string_map_of_list t)


let fstring format =
  let f = format_of_string format in
  Printf.sprintf f

let fprint ?(oc=stdout) format =
  let f = format_of_string format in
  Printf.fprintf oc f
