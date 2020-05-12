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
                   (Type.to_string (`Function ([`Int], `Float)))
                   (Type.to_string (`Function (t, `Float))));
      translator = (fun s -> "Decimal(" ^ (List.hd s) ^ ")");
      import = (fun () -> add_import "decimal" (Some "Decimal"))
    };

    (* Graphical functions *)
    "reinitialiser_crayon", {
      typer = (function
            [] -> `None
          | t -> raise_unexpected_type_error_with_name "reinitialiser_crayon"
                   (Type.to_string (`Function ([], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun _ -> "turtle.reset()");
      import = (fun () -> add_import "turtle" None)
    };
    "avancer_crayon", {
      typer = (function
            (`Int | `Float) :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "avancer"
                   (Type.to_string (`Function ([`Int], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun s -> "turtle.forward(" ^ List.hd s ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "reculer_crayon", {
      typer = (function
            (`Int | `Float) :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "reculer_crayon"
                   (Type.to_string (`Function ([`Int], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun s -> "turtle.backward(" ^ List.hd s ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "tourner_crayon", {
      typer = (function
            (`Int | `Float) :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "tourner_crayon"
                   (Type.to_string (`Function ([`Int], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun s -> "turtle.left(" ^ List.hd s ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "abaisser_crayon", {
      typer = (function
            [] -> `None
          | t -> raise_unexpected_type_error_with_name "abaisser_crayon"
                   (Type.to_string (`Function ([], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun _ -> "turtle.down()");
      import = (fun () -> add_import "turtle" None)
    };
    "lever_crayon", {
      typer = (function
            [] -> `None
          | t -> raise_unexpected_type_error_with_name "lever_crayon"
                   (Type.to_string (`Function ([], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun _ -> "turtle.up()");
      import = (fun () -> add_import "turtle" None)
    };
    "crayon_a", {
      typer = (function
            (`Int | `Float) :: (`Int | `Float) :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "crayon_a"
                   (Type.to_string (`Function ([`Int; `Int], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun s -> "turtle.goto(" ^ (List.hd s) ^ ", " ^ (List.hd (List.tl s)) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "masquer_crayon", {
      typer = (function
            [] -> `None
          | t -> raise_unexpected_type_error_with_name "masquer_crayon"
                   (Type.to_string (`Function ([], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun _ -> "turtle.ht()");
      import = (fun () -> add_import "turtle" None)
    };
    "afficher_crayon", {
      typer = (function
            [] -> `None
          | t -> raise_unexpected_type_error_with_name "afficher_crayon"
                   (Type.to_string (`Function ([], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun _ -> "turtle.st()");
      import = (fun () -> add_import "turtle" None)
    };
    "couleur_crayon", {
      typer = (function
            `String :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "couleur_crayon"
                   (Type.to_string (`Function ([`String], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun s -> "turtle.color(" ^ (List.hd s) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "couleur_fond", {
      typer = (function
            `String :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "couleur_fond"
                   (Type.to_string (`Function ([`String], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun s -> "turtle.bgcolor(" ^ (List.hd s) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "taille_crayon", {
      typer = (function
            (`Int | `Float) :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "taille_crayon"
                   (Type.to_string (`Function ([`Int], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun s -> "turtle.width(" ^ (List.hd s) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "vitesse_crayon", {
      typer = (function
            `String :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "vitesse_crayon"
                   (Type.to_string (`Function ([`String], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun s -> "turtle.speed(" ^ (List.hd s) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "dessiner_cercle", {
      typer = (function
            (`Int | `Float) :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "dessiner_cercle"
                   (Type.to_string (`Function ([`Int], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun s -> "turtle.circle(" ^ (List.hd s) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "dessiner_point", {
      typer = (function
            [] -> `None
          | t -> raise_unexpected_type_error_with_name "dessiner_point"
                   (Type.to_string (`Function ([], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun _ -> "turtle.dot()");
      import = (fun () -> add_import "turtle" None)
    };
    "dessiner_texte", {
      typer = (function
            `String :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "dessiner_texte"
                   (Type.to_string (`Function ([`String], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun s -> "turtle.write(" ^ (List.hd s) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "definir_titre", {
      typer = (function
            `String :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "definir_titre"
                   (Type.to_string (`Function ([`String], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun s -> "turtle.title(" ^ (List.hd s) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "commencer_remplissage", {
      typer = (function
            [] -> `None
          | t -> raise_unexpected_type_error_with_name "commencer_remplissage"
                   (Type.to_string (`Function ([], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun _ -> "turtle.begin_fill()");
      import = (fun () -> add_import "turtle" None)
    };
    "finir_remplissage", {
      typer = (function
            [] -> `None
          | t -> raise_unexpected_type_error_with_name "commencer_remplissage"
                   (Type.to_string (`Function ([], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun _ -> "turtle.end_fill()");
      import = (fun () -> add_import "turtle" None)
    };
    "mettre_a_jour_ecran", {
      typer = (function
            [] -> `None
          | t -> raise_unexpected_type_error_with_name "mettre_a_jour_ecran"
                   (Type.to_string (`Function ([], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun _ -> "turtle.update()");
      import = (fun () -> add_import "turtle" None)
    };
    "definir_delai", {
      typer = (function
            (`Int | `Float) :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "definir_delai"
                   (Type.to_string (`Function ([`Int], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun s -> "turtle.delay(" ^ (List.hd s) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "lancer_boucle", {
      typer = (function
            [] -> `None
          | t -> raise_unexpected_type_error_with_name "lancer_boucle"
                   (Type.to_string (`Function ([], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun _ -> "turtle.tracer(False); turtle.mainloop()");
      import = (fun () -> add_import "turtle" None)
    };
    (* Dealing with events *)
    "detecter_clic", {
      typer = (function
            (`Function ([`Int; `Int], `None) | `Function ([`Float; `Float], `None)) :: `Int :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "detecter_clic"
                   (Type.to_string (`Function ([`Function ([`Int; `Int], `None); `Int], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun s -> "turtle.onscreenclick(" ^ (List.hd s) ^ ", " ^ (List.hd (List.tl s)) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "detecter_touche", {
      typer = (function
            `Function ([], `None) :: (`Char | `String) :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "detecter_touche"
                   (Type.to_string (`Function ([`Function ([], `None); `String], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun s -> "turtle.onkey(" ^ (List.hd s) ^ ", " ^ (List.hd (List.tl s)) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "ecouter_clavier", {
      typer = (function
            [] -> `None
          | t -> raise_unexpected_type_error_with_name "ecouter_clavier"
                   (Type.to_string (`Function ([], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun _ -> "turtle.listen()");
      import = (fun () -> add_import "turtle" None)
    };
    "executer_apres", {
      typer = (function
            `Function ([], `None) :: (`Int | `Float) :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "executer_apres"
                   (Type.to_string (`Function ([`Function ([], `None); `Int], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun s -> "turtle.ontimer(" ^ (List.hd s) ^ ", " ^ (List.hd (List.tl s)) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
  ]
  in string_map_of_list assoc

let add_builtins map =
  StringMap.fold (function name -> function value -> StringMap.add name value) map functions
