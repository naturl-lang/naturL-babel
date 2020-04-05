open Utils
open Errors
open Structures

type builtin =
  {
    typer: Type.t list -> Type.t;
    translator: string list -> string;
    imports: string list
  }

let functions =
  let assoc = [
    (* Basic functions *)
    "afficher", {
      typer = (fun _ -> `None);
      translator = (fun s -> "print(" ^ (String.concat ", " s) ^ ")");
      imports = []
    };
    (* List functions *)
    "taille", {
      typer = (function
            `List _ :: [] -> `Int
          | t -> raise_unexpected_type_error_with_name "taille"
                   (Type.to_string (`Function ([`List `Any], `Int)))
                   (Type.to_string (`Function (t, `Int))));
      translator = (function
            l :: [] -> "len(" ^ l ^ ")"
          | _ -> assert false);
      imports = []
    };
    "ajouter", {
      typer = (function
            `List t1 :: t2 :: [] when Type.is_compatible t1 t2 -> `None
          | t -> raise_unexpected_type_error_with_name "ajouter"
                   (Type.to_string (`Function ([`List `Any; `Any], `None)))
                   (Type.to_string (`Function (t, `Any))));
      translator = (function
          | l :: x :: [] -> l ^ ".append(" ^ x ^ ")"
          | _ -> assert false);
      imports = []
    }
  ]
  in string_map_of_list assoc

let add_builtins map =
  StringMap.fold (function name -> function value -> StringMap.add name value) map functions

let imports = ref StringSet.empty
