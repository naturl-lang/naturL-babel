open Utils
open Imports
open Errors

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
    "supprimer", {
      typer = (function
            `List t :: `Int :: [] -> t
          | t -> raise_unexpected_type_error_with_name "supprimer"
                   (Type.to_string (`Function ([`List `Any], `Any)))
                   (Type.to_string (`Function (t, `Any))));
      translator = (function
            l :: x :: [] -> l ^ ".pop(" ^ (string_of_int (int_of_string x - 1)) ^ ")"
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
    (* Math functions *)
    "min", {
      typer = (function
            `Float :: `Float :: [] -> `Float
          | t -> raise_unexpected_type_error_with_name "min"
                   (Type.to_string (`Function ([`Float; `Float], `Float)))
                   (Type.to_string (`Function (t, `Float))));
      translator = (fun s -> "min(" ^ (List.hd s) ^ ", " ^ (List.hd (List.tl s)) ^ ")");
      import = (fun () -> ())
    };
    "max", {
      typer = (function
            `Float :: `Float :: [] -> `Float
          | t -> raise_unexpected_type_error_with_name "max"
                   (Type.to_string (`Function ([`Float; `Float], `Float)))
                   (Type.to_string (`Function (t, `Float))));
      translator = (fun s -> "max(" ^ (List.hd s) ^ ", " ^ (List.hd (List.tl s)) ^ ")");
      import = (fun () -> ())
    };
    "abs", {
      typer = (function
            `Float :: [] -> `Float
          | t -> raise_unexpected_type_error_with_name "abs"
                   (Type.to_string (`Function ([`Float], `Float)))
                   (Type.to_string (`Function (t, `Float))));
      translator = (fun s -> "abs(" ^ (List.hd s) ^ ")");
      import = (fun () -> ())
    };
    "cos", {
      typer = (function
            `Float :: [] -> `Float
          | t -> raise_unexpected_type_error_with_name "cos"
                   (Type.to_string (`Function ([`Float], `Float)))
                   (Type.to_string (`Function (t, `Float))));
      translator = (fun s -> "math.cos(" ^ (List.hd s) ^ ")");
      import = (fun () ->  Imports.add_import "math" None)
    };
    "sin", {
      typer = (function
            `Float :: [] -> `Float
          | t -> raise_unexpected_type_error_with_name "sin"
                   (Type.to_string (`Function ([`Float], `Float)))
                   (Type.to_string (`Function (t, `Float))));
      translator = (fun s -> "math.sin(" ^ (List.hd s) ^ ")");
      import = (fun () ->  Imports.add_import "math" None)
    };
    "arccos", {
      typer = (function
            `Float :: [] -> `Float
          | t -> raise_unexpected_type_error_with_name "arccos"
                   (Type.to_string (`Function ([`Float], `Float)))
                   (Type.to_string (`Function (t, `Float))));
      translator = (fun s -> "math.acos(" ^ (List.hd s) ^ ")");
      import = (fun () ->  Imports.add_import "math" None)
    };
    "arcsin", {
      typer = (function
            `Float :: [] -> `Float
          | t -> raise_unexpected_type_error_with_name "arcsin"
                   (Type.to_string (`Function ([`Float], `Float)))
                   (Type.to_string (`Function (t, `Float))));
      translator = (fun s -> "math.asin(" ^ (List.hd s) ^ ")");
      import = (fun () ->  Imports.add_import "math" None)
    };
    "arctan", {
      typer = (function
            `Float :: [] -> `Float
          | t -> raise_unexpected_type_error_with_name "arcsin"
                   (Type.to_string (`Function ([`Float], `Float)))
                   (Type.to_string (`Function (t, `Float))));
      translator = (fun s -> "math.atan(" ^ (List.hd s) ^ ")");
      import = (fun () ->  Imports.add_import "math" None)
    };
    "racine", {
      typer = (function
            `Float :: [] -> `Float
          | t -> raise_unexpected_type_error_with_name "racine"
                   (Type.to_string (`Function ([`Float], `Float)))
                   (Type.to_string (`Function (t, `Float))));
      translator = (fun s -> "math.sqrt(" ^ (List.hd s) ^ ")");
      import = (fun () ->  Imports.add_import "math" None)
    };
    (* Cast functions *)
    "chaine", {
      typer = (function
            #Type.t :: [] -> `String
          | t -> raise_unexpected_type_error_with_name "chaine"
                   (Type.to_string (`Function ([`Any], `String)))
                   (Type.to_string (`Function (t, `String))));
      translator = (fun s -> "str(" ^ (List.hd s) ^ ")");
      import = (fun () -> ())
    };
    "reel", {
      typer = (function
            #Type.t :: [] -> `Float
          | t -> raise_unexpected_type_error_with_name "reel"
                   (Type.to_string (`Function ([`Any], `Float)))
                   (Type.to_string (`Function (t, `Float))));
      translator = (fun s -> "float(" ^ (List.hd s) ^ ")");
      import = (fun () -> ())
    };
    "entier", {
      typer = (function
            #Type.t :: [] -> `Int
          | t -> raise_unexpected_type_error_with_name "reel"
                   (Type.to_string (`Function ([`Any], `Int)))
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
    "reinitialiser_geometrie", {
      typer = (function
            [] -> `None
          | t -> raise_unexpected_type_error_with_name "reinitialiser_geometrie"
                   (Type.to_string (`Function ([], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun _ -> "turtle.home()");
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
            `String :: `String :: `Int :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "dessiner_texte"
                   (Type.to_string (`Function ([`String; `String; `Int], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun s ->
          "Thread(target=lambda: turtle.write(" ^ (List.hd s) ^ ", align='center', font=(" ^ (List.nth s 1) ^ ", " ^ (List.nth s 2) ^ ", 'normal'))).start()" ^
          "  # This action takes too long so it is executed in a thread");
      import = (fun () -> add_import "turtle" None; add_import "threading" (Some "Thread"))
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
    "effacer_ecran", {
      typer = (function
            [] -> `None
          | t -> raise_unexpected_type_error_with_name "effacer_ecran"
                   (Type.to_string (`Function ([], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun _ -> "turtle.clear()");
      import = (fun () -> add_import "turtle" None)
    };
    "delai_ecran", {
      typer = (function
            (`Int | `Float) :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "delai_ecran"
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
    "_detecter_clic", {
      typer = (function
            `Function ([`Float; `Float], `None) :: `Int :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "_detecter_clic"
                   (Type.to_string (`Function ([`Function ([`Float; `Float], `None); `Int], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun s -> "turtle.onscreenclick(" ^ (List.hd s) ^ ", " ^ (List.hd (List.tl s)) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "_detecter_touche_pressee", {
      typer = (function
            `Function ([], `None) :: (`Char | `String) :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "_detecter_touche_pressee"
                   (Type.to_string (`Function ([`Function ([], `None); `String], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (function s -> "turtle.onkeypress(" ^ (List.hd s) ^ ", " ^ (List.hd (List.tl s)) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "_detecter_touche_levee", {
      typer = (function
            `Function ([], `None) :: (`Char | `String) :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "_detecter_touche_levee"
                   (Type.to_string (`Function ([`Function ([], `None); `String], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (function s -> "turtle.onkeyrelease(" ^ (List.hd s) ^ ", " ^ (List.hd (List.tl s)) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    "_ecouter_clavier", {
      typer = (function
            [] -> `None
          | t -> raise_unexpected_type_error_with_name "_ecouter_clavier"
                   (Type.to_string (`Function ([], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun _ -> "turtle.listen()");
      import = (fun () -> add_import "turtle" None)
    };
    "_executer_apres", {
      typer = (function
            `Function ([], `None) :: `Int :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "_executer_apres"
                   (Type.to_string (`Function ([`Function ([], `None); `Int], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun s -> "turtle.ontimer(" ^ (List.hd s) ^ ", " ^ (List.hd (List.tl s)) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    (* Geometry functions *)
    "largeur_ecran", {
      typer = (function
             [] -> `Int
          | t -> raise_unexpected_type_error_with_name "largeur_ecran"
                   (Type.to_string (`Function ([], `Int)))
                   (Type.to_string (`Function (t, `Int))));
      translator = (fun _ -> "turtle.window_width()");
      import = (fun () -> add_import "turtle" None)
    };
    "hauteur_ecran", {
      typer = (function
             [] -> `Int
          | t -> raise_unexpected_type_error_with_name "hauteur_ecran"
                   (Type.to_string (`Function ([], `Int)))
                   (Type.to_string (`Function (t, `Int))));
      translator = (fun _ -> "turtle.window_height()");
      import = (fun () -> add_import "turtle" None)
    };
    "definir_taille_ecran", {
      typer = (function
            (`Int | `Float) :: (`Int | `Float) :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "definir_taille_ecran"
                   (Type.to_string (`Function ([`Int; `Int], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun s -> "turtle.setup(" ^ (List.hd s) ^ ", " ^ (List.hd (List.tl s)) ^ ")");
      import = (fun () -> add_import "turtle" None)
    };
    (* Time function *)
    "attendre", {
      typer = (function
             `Int :: [] -> `None
          | t -> raise_unexpected_type_error_with_name "hauteur_ecran"
                   (Type.to_string (`Function ([`Int], `None)))
                   (Type.to_string (`Function (t, `None))));
      translator = (fun s -> "time.sleep(" ^ (string_of_float (float_of_string (List.hd s) /. 1000.)) ^ ")");  (* The time is given in ms*)
      import = (fun () -> add_import "time" None)
    }
  ]
  in string_map_of_list assoc

let accessible_keywords: (string * Type.t) list = [
  "afficher", `Function ([`Any], `None);
  "taille", `Function ([`List `Any], `Any);
  "supprimer", `Function ([`List `Any; `Int], `Any);
  "ajouter", `Function ([`List `Any; `Any], `None);
  "min", `Function ([`Float; `Float], `Float);
  "max", `Function ([`Float; `Float], `Float);
  "abs", `Function ([`Float], `Float);
  "cos", `Function ([`Float], `Float);
  "sin", `Function ([`Float], `Float);
  "arccos", `Function ([`Float], `Float);
  "arcsin", `Function ([`Float], `Float);
  "arctan", `Function ([`Float], `Float);
  "racine", `Function ([`Float], `Float);
  "chaine", `Function ([`Any], `String);
  "entier", `Function ([`Any], `Int);
  "reel", `Function ([`Any], `Int)
]

let add_builtins map =
  StringMap.fold (function name -> function value -> StringMap.add name value) map functions
