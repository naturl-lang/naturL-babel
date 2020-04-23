open Utils
open Imports
open Errors
open Structures

type builtin =
  {
    typer: Type.t list -> Type.t;
    translator: string list -> string;
    import: unit -> unit
  }

let functions =
  let assoc = [
    (* Basic functions *)
    "afficher", {
      typer = (fun _ -> `None);
      translator = (fun s -> "print(" ^ (String.concat ", " s) ^ ")");
      import = (fun () -> ())
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
      import = (fun () -> ())
    };
    "ajouter", {
      typer = (function
            `List t1 :: t2 :: [] when Type.is_compatible t1 t2 -> `None
          | t -> raise_unexpected_type_error_with_name "ajouter"
                   (Type.to_string (`Function ([`List `Any; `Any], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (function
          | l :: x :: [] -> l ^ ".append(" ^ x ^ ")"
          | _ -> assert false);
      import = (fun () -> ())
    };
    (* Cast functions *)
    "reel", {
      typer = (function
            `Int :: [] -> `Float
          | t -> raise_unexpected_type_error_with_name "reel"
                   (Type.to_string (`Function ([`Int], `Float)))
                   (Type.to_string (`Function (t, `Float))));
      translator = (fun s -> "float(" ^ (List.hd s) ^ ")");
      import = (fun () -> ())
    };
    "entier", {
      typer = (function
            `Float :: [] -> `Int
          | t -> raise_unexpected_type_error_with_name "reel"
                   (Type.to_string (`Function ([`Float], `Int)))
                   (Type.to_string (`Function (t, `Int))));
      translator = (fun s -> "int(" ^ (List.hd s) ^ ")");
      import = (fun () -> ())
    };
    "decimal", {
      typer = (function
            `Int :: [] -> `Float
          | `Float :: [] -> `Float
          | t -> raise_unexpected_type_error_with_name "decimal"
                   (Type.to_string (`Function ([`Any], `Float)))
                   (Type.to_string (`Function (t, `Float))));
      translator = (fun s -> "Decimal(" ^ (List.hd s) ^ ")");
      import = (fun () -> add_import "decimal" (Some "Decimal"))
    };
  ]
  in string_map_of_list assoc

let add_builtins map =
  StringMap.fold (function name -> function value -> StringMap.add name value) map functions
