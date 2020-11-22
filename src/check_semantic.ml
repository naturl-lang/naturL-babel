open Utils
open Errors
open Warnings
open Expressions


(* Fileprivate functions *)
open (struct
end)


let check_semantic ast =
  let add_locale_variables location variables =
    Variables.add_locale_variables location variables
  in
  (*Semantic analysis. Returns the current variables and if there is a return *)
  let rec check_semantic ~current_func variables ast =
    match ast with
    | Ast.Body children ->
      try_add_error (fun () ->
          let (variables, returns), _ = List.fold_left_map
              (function variables, returns -> fun ast ->
                  let variables, new_returns = check_semantic ~current_func variables ast
                  in (variables, returns || new_returns), ())
              (variables, false)
              children
          in variables, returns)
        ~default:(variables, false)
    | Expr (location, expr) ->
      (*Evaluate the expression's type to check for semantic error in it and update the types of variables*)
      let expr_type = try_update_err location (fun () -> type_of_expr expr variables) in
      if expr_type <> Type.None then
        add_warning ~location "La valeur de retour de cette expression n'est pas utilisée" 0;
      add_locale_variables location.line variables;
      variables, false
    | Return (location, expr) ->
      let ret_type = try_update_err location (fun () -> type_of_expr expr variables) in
      let func_name, func_location = match current_func with
        | Some func -> func
        | None -> raise_syntax_error ~location "Un retourner ne peut pas se trouver en dehors d'une fonction"
      in
      (* Get the function type to check if the expression type matches *)
      let func_type =
        match Variables.var_type_opt func_name func_location @@ !Variables.defined_variables with
        | Some (Type.Function (_, func_type)) -> func_type
        | _ -> raise_bug "18520211814"  (* A function should have been defined *)
      in if not @@ Type.equal ret_type func_type then
        raise_syntax_error ~location
          ("Le type de retour de la fonction '" ^ func_name ^
           "' est '" ^ (Type.to_string func_type)
           ^ "'. Cette expression est néanmoins de type '" ^ (Type.to_string ret_type) ^ "'");
      add_locale_variables location.line variables;
      variables, true
    | Assign (location, name, expr) ->
      (*If the variable is already decared, get its location and its type, else define it*)
      let define_location, desired_type =
        match variables |> StringMap.find_opt name with
        | Some location ->
          location, begin
            match Variables.var_type_opt name location !Variables.defined_variables with
            | Some t -> t
            | None -> Any
          end
        | None ->
          Variables.define_variable name location;
          location, Any
      in
      let expr_type = try_update_err location
          (fun () -> type_of_expr ~desired_type expr variables) in
      if desired_type = Any then
        Variables.update_type name location expr_type
      else if not @@ Type.equal expr_type desired_type then
        raise_type_error ~location (
          "Cette expression est de type '" ^ (Type.to_string expr_type) ^
          "' mais la variable '" ^ name ^ "' est de type '" ^
          (Type.to_string desired_type) ^ "'. Ces deux types sont incompatibles.");
      let variables = variables |> StringMap.add name define_location in
      add_locale_variables location.line variables;
      variables, false
    | If (location, condition, body, else_) ->
      let condition_type = try_update_err location
          (fun () -> type_of_expr ~desired_type:Bool condition variables) in
      if not @@ Type.equal condition_type Bool then
        raise_type_error ~location
          ("Cette expression est de type '" ^ (Type.to_string condition_type) ^
           "' alors qu'une condition doit être un booléen.");
      if condition = Value (Bool true) then
        add_warning ~location "La condition est toujours vraie" 0
      else if condition = Value (Bool false) then
        add_warning ~location "La condition est toujours fausse" 0;
      let merger = fun _ -> fun _ -> fun second -> Some second in
      let new_variables, returns = check_semantic ~current_func variables body in
      let variables = new_variables |> StringMap.union merger variables in
      let variables, returns = match else_ with
        | Some body ->
          let new_variables, new_returns = check_semantic ~current_func variables body in
          new_variables |> StringMap.union merger variables, returns && new_returns
        | None -> variables, false
      in
      add_locale_variables location.line variables;
      variables, returns
    | Else (location, body) ->
      let new_variables, returns = check_semantic ~current_func variables body in
      let variables = new_variables |> StringMap.union (fun _ -> fun _ -> fun s -> Some s) variables in
      add_locale_variables location.line variables;
      variables, returns
    | For (location, var_name, start, end_, body) ->
      let start_type = try_update_err location
          (fun () -> type_of_expr ~desired_type:Int start variables)
      and end_type = try_update_err location
          (fun () -> type_of_expr ~desired_type:Int end_ variables)
      in
      try_update_err location (fun () ->
          if not @@ Type.equal start_type Int then
            raise_type_error ~location
              ("La borne inférieure d'une boucle for doit être un entier, et non de type '" ^
               (Type.to_string start_type) ^ "'")
          else if not @@ Type.equal end_type Int then
            raise_type_error ~location
              ("La borne supérieure d'une boucle for doit être un entier, et non de type '" ^
               (Type.to_string start_type) ^ "'"));
      (*If the variable has already been defined, make sure it is of type integer, else define it*)
      let variables = match StringMap.find_opt var_name variables with
        | None ->
          Variables.define_variable var_name location;
          Variables.update_type var_name location Int;
          variables |> StringMap.add var_name location
        | Some var_location ->
          begin
            match Variables.var_type_opt var_name var_location !Variables.defined_variables with
            | Some t ->
              if not @@ Type.equal t Int then
                raise_type_error ~location
                  ("La variable d'une boucle for doit être un entier ; la variable '" ^
                   var_name ^ "' est de type '" ^ (Type.to_string t) ^ "'")
            | None -> raise_bug "45691454" (* Should be defined *)
          end;
          variables
      in
      (*Merge the variables of the body with the current ones*)
      let new_variables, returns = check_semantic ~current_func variables body in
      (* If the body returns and the upper bound is greater than the lower bound, then the block returns *)
      let returns = returns && match start, end_ with
          | Value (Int start_i), Value (Int end_i) -> start_i <<= end_i
          | _ -> false
      in
      let variables = new_variables |> StringMap.union (fun _ -> fun _ -> fun s -> Some s) variables in
      add_locale_variables location.line variables;
      variables, returns
    | For_each (location, name, iterable, body) ->
      let iterable_type = try_update_err location
          (fun () -> type_of_expr iterable variables) in
      (*Make sure the type is iterable and the variable type corresponds*)
      let variables =
        (match Type.get_iterable_type iterable_type with
         | Some iter_type -> (*If the type is iterable, check the variable type*)
           begin
             match StringMap.find_opt name variables with
             | None ->  (* If it hasn't been defined yet, define it with the appropriate type *)
               Variables.define_variable name location;
               Variables.update_type name location iter_type;
               variables |> StringMap.add name location
             | Some var_location -> (* If it has already been defined, check the type *)
               begin
                 match Variables.var_type_opt name var_location !Variables.defined_variables with
                 | Some t ->
                   if not @@ Type.equal t iter_type then
                     raise_type_error ~location
                       ("La variable '" ^ name ^ "' doit être de type '" ^ (Type.to_string iter_type) ^
                        "', et non '" ^ (Type.to_string t) ^ "'")
                 | None -> Variables.update_type name location iter_type
               end;
               variables
           end
         | None -> (* If the type is not iterable, raise an error *)
           raise_type_error ~location
             ("L'expression de la boucle est de type '" ^
              (Type.to_string iterable_type) ^ "'. Or, ce type n'est pas itérable")) in
      let new_variables, returns = check_semantic ~current_func variables body in
      (* If the body returns and the iterable is not empty, then the block returns *)
      let returns = returns && match iterable with
        | Value (String "") -> false
        | List (_ :: _) | Value (String _) -> true
        | _ -> false
      in
      let variables = new_variables |> StringMap.union (fun _ -> fun _ -> fun s -> Some s) variables in
      add_locale_variables location.line variables;
      variables, returns
    | While (location, condition, body) ->
      check_semantic ~current_func variables @@ If (location, condition, body, None)
    | Func_definition (location, name, args, return, body) ->
      (* Declare the arguments and *)
      let arg_types, ret_type = try_update_err location (fun () ->
          args |> List.map (function type_s, arg_name ->
              let arg_type = try_update_err location (fun () -> Type.of_string type_s) in
              Variables.define_variable arg_name location;
              Variables.update_type arg_name location arg_type;
              arg_type),
          Type.of_string return) in
      let variables = variables |> StringMap.add name location in
      let local_variables =
        args
        |> List.fold_left
          (fun map -> function _, arg -> map |> StringMap.add arg location)
          variables in
      Variables.define_variable name location;
      Variables.update_type name location @@ Function (arg_types, ret_type);
      let _, returns = check_semantic ~current_func:(Some (name, location)) local_variables body in
      if not returns && ret_type <> None then
        raise_syntax_error ~location
          "Il existe des cas pour lesquels cette fonction ne retourne pas de valeur";
      add_locale_variables location.line variables;
      variables, false
    | End -> variables, false
  in
  Variables.reset ();
  clear_errors ();
  clear_warnings ();
  let _ = check_semantic ~current_func:None (StringMap.empty) ast
  in ()
