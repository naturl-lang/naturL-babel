open Utils
open Imports
open Errors

type builtin =
  {
    typer: Type.t list -> Type.t;
    translator: string list -> string;
    import: unit -> unit
  }

let raise_function_error name args user_args =
  raise_function_error name
    (args |> List.map (function s, t -> s, Type.to_string t))
    (user_args |> List.map Type.to_string)

let functions =
  let assoc = [
    (* Basic functions *)
    "afficher", {
      typer = (fun _ -> None);
      translator = (fun s -> "print(" ^ (String.concat ", " s) ^ ")");
      import = (fun () -> ())
    };
    (* List functions *)
    "taille", {
      typer = (function
            List _ :: [] -> Int
          | t -> raise_function_error "taille" ["liste", List Any] t);
      translator = (function
            l :: [] -> "len(" ^ l ^ ")"
          | _ -> assert false);
      import = (fun () -> ())
    };
    "supprimer", {
      typer = (function
            List t :: Int :: [] -> t
          | t -> raise_function_error "supprimer" ["liste", List Any; "élément", Int] t);
      translator = (function
            l :: x :: [] -> l ^ ".pop(" ^ (string_of_int (int_of_string x - 1)) ^ ")"
          | _ -> assert false);
      import = (fun () -> ())
    };
    "ajouter", {
      typer = (function
            List t1 :: t2 :: [] when Type.equal t1 t2 -> None
          | t -> raise_function_error "ajouter" ["liste", List Any; "élément", Any] t);
      translator = (function
          | l :: x :: [] -> l ^ ".append(" ^ x ^ ")"
          | _ -> assert false);
      import = (fun () -> ())
    };
    (* Math functions *)
    "min", {
      typer = (function
          | Float :: Float :: [] -> Float
          | Int :: Int :: [] -> Int
          | t -> raise_function_error "min" ["a", Union [Int; Float]; "b", Union [Int; Float]] t);
      translator = (fun s -> "min(" ^ (List.hd s) ^ ", " ^ (List.hd (List.tl s)) ^ ")");
      import = (fun () -> ())
    };
    "max", {
      typer = (function
          | Float :: Float :: [] -> Float
          | Int :: Int :: [] -> Int
          | t -> raise_function_error "max" ["a", Union [Int; Float]; "b", Union [Int; Float]] t);
      translator = (fun s -> "max(" ^ (List.hd s) ^ ", " ^ (List.hd (List.tl s)) ^ ")");
      import = (fun () -> ())
    };
    "abs", {
      typer = (function
          | Int :: [] -> Int
          | Float :: [] -> Float
          | t -> raise_function_error "abs" ["x", Union [Int; Float]] t);
      translator = (fun s -> "abs(" ^ (List.hd s) ^ ")");
      import = (fun () -> ())
    };
    "cos", {
      typer = (function
            Float :: [] -> Float
          | t -> raise_function_error "cos" ["x", Float] t);
      translator = (fun s -> "math.cos(" ^ (List.hd s) ^ ")");
      import = (fun () ->  Imports.add_import "math" None)
    };
    "sin", {
      typer = (function
            Float :: [] -> Float
          | t -> raise_function_error "sin" ["x", Float] t);
      translator = (fun s -> "math.sin(" ^ (List.hd s) ^ ")");
      import = (fun () ->  Imports.add_import "math" None)
    };
    "arccos", {
      typer = (function
            Float :: [] -> Float
          | t -> raise_function_error "arccos" ["x", Float] t);
      translator = (fun s -> "math.acos(" ^ (List.hd s) ^ ")");
      import = (fun () ->  Imports.add_import "math" None)
    };
    "arcsin", {
      typer = (function
            Float :: [] -> Float
          | t -> raise_function_error "arcsin" ["x", Float] t);
      translator = (fun s -> "math.asin(" ^ (List.hd s) ^ ")");
      import = (fun () ->  Imports.add_import "math" None)
    };
    "arctan", {
      typer = (function
            Float :: [] -> Float
          | t -> raise_function_error "arcsin" ["x", Float] t);
      translator = (fun s -> "math.atan(" ^ (List.hd s) ^ ")");
      import = (fun () ->  Imports.add_import "math" None)
    };
    "racine", {
      typer = (function
            Float :: [] -> Float
          | t -> raise_function_error "racine" ["x", Float] t);
      translator = (fun s -> "math.sqrt(" ^ (List.hd s) ^ ")");
      import = (fun () ->  Imports.add_import "math" None)
    };
    (* Cast functions *)
    "chaine", {
      typer = (function
            _ :: [] -> String
          | t -> raise_function_error "chaine" ["x", Any] t);
      translator = (fun s -> "str(" ^ (List.hd s) ^ ")");
      import = (fun () -> ())
    };
    "réel", {
      typer = (function
          | Int :: [] -> Float
          | String :: [] -> Float
          | t -> raise_function_error "réel" ["x", Union [Int; String]] t);
      translator = (fun s -> "float(" ^ (List.hd s) ^ ")");
      import = (fun () -> ())
    };
    "entier", {
      typer = (function
          | Float :: [] -> Int
          | String :: [] -> Int
          | t -> raise_function_error "entier" ["x", Union [Float; String]] t);
      translator = (fun s -> "int(" ^ (List.hd s) ^ ")");
      import = (fun () -> ())
    };
    "décimal", {
      typer = (function
            Int :: [] -> Float
          | Float :: [] -> Float
          | t -> raise_function_error "décimal" ["x", Union [Int; Float]] t);
      translator = (fun s -> "Decimal(" ^ (List.hd s) ^ ")");
      import = (fun () -> add_import "decimal" (Some "Decimal"))
    };

    (* Graphical functions *)
    "reinitialiser_crayon", {
      typer = (function
            [] -> None
          | t -> raise_unexpected_type_error_with_name "reinitialiser_crayon"
                   (Type.to_string (Function ([], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun _ -> "turtle.reset()");
      import = (fun () -> add_import "turtle" None)
    };
    "reinitialiser_geometrie", {
      typer = (function
            [] -> None
          | t -> raise_unexpected_type_error_with_name "reinitialiser_geometrie"
                   (Type.to_string (Function ([], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun _ -> "turtle.home()");
      import = (fun () -> add_import "turtle" None)
    };
    "avancer_crayon", {
      typer = (function
            (Int | Float) :: [] -> None
          | t -> raise_unexpected_type_error_with_name "avancer"
                   (Type.to_string (Function ([Int], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun s -> "turtle.forward(" ^ List.hd s ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "reculer_crayon", {
      typer = (function
            (Int | Float) :: [] -> None
          | t -> raise_unexpected_type_error_with_name "reculer_crayon"
                   (Type.to_string (Function ([Int], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun s -> "turtle.backward(" ^ List.hd s ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "tourner_crayon", {
      typer = (function
            (Int | Float) :: [] -> None
          | t -> raise_unexpected_type_error_with_name "tourner_crayon"
                   (Type.to_string (Function ([Int], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun s -> "turtle.left(" ^ List.hd s ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "abaisser_crayon", {
      typer = (function
            [] -> None
          | t -> raise_unexpected_type_error_with_name "abaisser_crayon"
                   (Type.to_string (Function ([], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun _ -> "turtle.down()");
      import = (fun () -> add_import "turtle" None)
    };
    "lever_crayon", {
      typer = (function
            [] -> None
          | t -> raise_unexpected_type_error_with_name "lever_crayon"
                   (Type.to_string (Function ([], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun _ -> "turtle.up()");
      import = (fun () -> add_import "turtle" None)
    };
    "crayon_a", {
      typer = (function
            (Int | Float) :: (Int | Float) :: [] -> None
          | t -> raise_unexpected_type_error_with_name "crayon_a"
                   (Type.to_string (Function ([Int; Int], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun s -> "turtle.goto(" ^ (List.hd s) ^ ", " ^ (List.hd (List.tl s)) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "masquer_crayon", {
      typer = (function
            [] -> None
          | t -> raise_unexpected_type_error_with_name "masquer_crayon"
                   (Type.to_string (Function ([], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun _ -> "turtle.ht()");
      import = (fun () -> add_import "turtle" None)
    };
    "afficher_crayon", {
      typer = (function
            [] -> None
          | t -> raise_unexpected_type_error_with_name "afficher_crayon"
                   (Type.to_string (Function ([], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun _ -> "turtle.st()");
      import = (fun () -> add_import "turtle" None)
    };
    "couleur_crayon", {
      typer = (function
            String :: [] -> None
          | t -> raise_unexpected_type_error_with_name "couleur_crayon"
                   (Type.to_string (Function ([String], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun s -> "turtle.color(" ^ (List.hd s) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "couleur_fond", {
      typer = (function
            String :: [] -> None
          | t -> raise_unexpected_type_error_with_name "couleur_fond"
                   (Type.to_string (Function ([String], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun s -> "turtle.bgcolor(" ^ (List.hd s) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "taille_crayon", {
      typer = (function
            (Int | Float) :: [] -> None
          | t -> raise_unexpected_type_error_with_name "taille_crayon"
                   (Type.to_string (Function ([Int], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun s -> "turtle.width(" ^ (List.hd s) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "vitesse_crayon", {
      typer = (function
            String :: [] -> None
          | t -> raise_unexpected_type_error_with_name "vitesse_crayon"
                   (Type.to_string (Function ([String], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun s -> "turtle.speed(" ^ (List.hd s) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "dessiner_cercle", {
      typer = (function
            (Int | Float) :: [] -> None
          | t -> raise_unexpected_type_error_with_name "dessiner_cercle"
                   (Type.to_string (Function ([Int], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun s -> "turtle.circle(" ^ (List.hd s) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "dessiner_point", {
      typer = (function
            [] -> None
          | t -> raise_unexpected_type_error_with_name "dessiner_point"
                   (Type.to_string (Function ([], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun _ -> "turtle.dot()");
      import = (fun () -> add_import "turtle" None)
    };
    "dessiner_texte", {
      typer = (function
            String :: String :: Int :: [] -> None
          | t -> raise_unexpected_type_error_with_name "dessiner_texte"
                   (Type.to_string (Function ([String; String; Int], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun s ->
          "Thread(target=lambda: turtle.write(" ^ (List.hd s) ^ ", align='center', font=(" ^ (List.nth s 1) ^ ", " ^ (List.nth s 2) ^ ", 'normal'))).start()" ^
          "  # This action takes too long so it is executed in a thread");
      import = (fun () -> add_import "turtle" None; add_import "threading" (Some "Thread"))
    };
    "definir_titre", {
      typer = (function
            String :: [] -> None
          | t -> raise_unexpected_type_error_with_name "definir_titre"
                   (Type.to_string (Function ([String], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun s -> "turtle.title(" ^ (List.hd s) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "commencer_remplissage", {
      typer = (function
            [] -> None
          | t -> raise_unexpected_type_error_with_name "commencer_remplissage"
                   (Type.to_string (Function ([], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun _ -> "turtle.begin_fill()");
      import = (fun () -> add_import "turtle" None)
    };
    "finir_remplissage", {
      typer = (function
            [] -> None
          | t -> raise_unexpected_type_error_with_name "commencer_remplissage"
                   (Type.to_string (Function ([], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun _ -> "turtle.end_fill()");
      import = (fun () -> add_import "turtle" None)
    };
    "mettre_a_jour_ecran", {
      typer = (function
            [] -> None
          | t -> raise_unexpected_type_error_with_name "mettre_a_jour_ecran"
                   (Type.to_string (Function ([], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun _ -> "turtle.update()");
      import = (fun () -> add_import "turtle" None)
    };
    "effacer_ecran", {
      typer = (function
            [] -> None
          | t -> raise_unexpected_type_error_with_name "effacer_ecran"
                   (Type.to_string (Function ([], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun _ -> "turtle.clear()");
      import = (fun () -> add_import "turtle" None)
    };
    "delai_ecran", {
      typer = (function
            (Int | Float) :: [] -> None
          | t -> raise_unexpected_type_error_with_name "delai_ecran"
                   (Type.to_string (Function ([Int], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun s -> "turtle.delay(" ^ (List.hd s) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "lancer_boucle", {
      typer = (function
            [] -> None
          | t -> raise_unexpected_type_error_with_name "lancer_boucle"
                   (Type.to_string (Function ([], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun _ -> "turtle.tracer(False); turtle.mainloop()");
      import = (fun () -> add_import "turtle" None)
    };
    (* Dealing with events *)
    "_detecter_clic", {
      typer = (function
            Function ([Float; Float], None) :: Int :: [] -> None
          | t -> raise_unexpected_type_error_with_name "_detecter_clic"
                   (Type.to_string (Function ([Function ([Float; Float], None); Int], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun s -> "turtle.onscreenclick(" ^ (List.hd s) ^ ", " ^ (List.hd (List.tl s)) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "_detecter_touche_pressee", {
      typer = (function
            Function ([], None) :: (Char | String) :: [] -> None
          | t -> raise_unexpected_type_error_with_name "_detecter_touche_pressee"
                   (Type.to_string (Function ([Function ([], None); String], None)))
                   (Type.to_string (Function (t, None))));
      translator = (function s -> "turtle.onkeypress(" ^ (List.hd s) ^ ", " ^ (List.hd (List.tl s)) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "_detecter_touche_levee", {
      typer = (function
            Function ([], None) :: (Char | String) :: [] -> None
          | t -> raise_unexpected_type_error_with_name "_detecter_touche_levee"
                   (Type.to_string (Function ([Function ([], None); String], None)))
                   (Type.to_string (Function (t, None))));
      translator = (function s -> "turtle.onkeyrelease(" ^ (List.hd s) ^ ", " ^ (List.hd (List.tl s)) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "_ecouter_clavier", {
      typer = (function
            [] -> None
          | t -> raise_unexpected_type_error_with_name "_ecouter_clavier"
                   (Type.to_string (Function ([], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun _ -> "turtle.listen()");
      import = (fun () -> add_import "turtle" None)
    };
    "_executer_apres", {
      typer = (function
            Function ([], None) :: Int :: [] -> None
          | t -> raise_unexpected_type_error_with_name "_executer_apres"
                   (Type.to_string (Function ([Function ([], None); Int], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun s -> "turtle.ontimer(" ^ (List.hd s) ^ ", " ^ (List.hd (List.tl s)) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    (* Geometry functions *)
    "largeur_ecran", {
      typer = (function
             [] -> Int
          | t -> raise_unexpected_type_error_with_name "largeur_ecran"
                   (Type.to_string (Function ([], Int)))
                   (Type.to_string (Function (t, Int))));
      translator = (fun _ -> "turtle.window_width()");
      import = (fun () -> add_import "turtle" None)
    };
    "hauteur_ecran", {
      typer = (function
             [] -> Int
          | t -> raise_unexpected_type_error_with_name "hauteur_ecran"
                   (Type.to_string (Function ([], Int)))
                   (Type.to_string (Function (t, Int))));
      translator = (fun _ -> "turtle.window_height()");
      import = (fun () -> add_import "turtle" None)
    };
    "definir_taille_ecran", {
      typer = (function
            (Int | Float) :: (Int | Float) :: [] -> None
          | t -> raise_unexpected_type_error_with_name "definir_taille_ecran"
                   (Type.to_string (Function ([Int; Int], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun s -> "turtle.setup(" ^ (List.hd s) ^ ", " ^ (List.hd (List.tl s)) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    (* Time function *)
    "attendre", {
      typer = (function
             Int :: [] -> None
          | t -> raise_unexpected_type_error_with_name "hauteur_ecran"
                   (Type.to_string (Function ([Int], None)))
                   (Type.to_string (Function (t, None))));
      translator = (fun s -> "time.sleep(" ^ (string_of_float (float_of_string (List.hd s) /. 1000.)) ^ ")");  (* The time is given in ms*)
      import = (fun () -> add_import "time" None)
    }
  ]
  in string_map_of_list assoc

let accessible_keywords: (string * Type.t) list = [
  "afficher", Function ([Any], None);
  "taille", Function ([List Any], Int);
  "supprimer", Function ([List Any; Int], Any);
  "ajouter", Function ([List Any; Any], None);
  "min", Function ([Union [Int; Float]; Union [Int; Float]], Union [Int; Float]);
  "max", Function ([Union [Int; Float]; Union [Int; Float]], Union [Int; Float]);
  "abs", Function ([Union [Int; Float]], Union [Int; Float]);
  "cos", Function ([Union [Int; Float]], Union [Int; Float]);
  "sin", Function ([Union [Int; Float]], Union [Int; Float]);
  "arccos", Function ([Union [Int; Float]], Union [Int; Float]);
  "arcsin", Function ([Union [Int; Float]], Union [Int; Float]);
  "arctan", Function ([Union [Int; Float]], Union [Int; Float]);
  "racine", Function ([Union [Int; Float]], Union [Int; Float]);
  "chaine", Function ([Any], String);
  "entier", Function ([Union [Float]; String], Int);
  "réel", Function ([Union [Int; String]], Float);
  "décimal", Function ([Union [Int; Float]], Float)
]

let add_builtins map =
  StringMap.fold (function name -> function value -> StringMap.add name value) map functions
