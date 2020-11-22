(*#require "ppx_regexp";;*)
open Errors
open Context
open Expressions
open Getters

let parse_body code =
  let line_list = String.split_on_char '\n' code in
  (* Mutually recursive parsing functions. *)
  (* They need to be mutually recursive because each of them calls parse_body and parse_body calls each of them *)
  let rec parse_body ?(terminators = [""]) context =
    let rec parse_body terminators body context =
      let code = context.code and index = context.index in
      let location = get_current_line_location ~line_list context.code context.index in
      let line, index = get_line code index in
      if is_in_func_definition context then
        match%pcre line with
        | "^[ \t]*debut[ \t]*$" ->
          parse_body ["fin"; "fin fonction"] body (mark_func_defined { context with index })
        | _ -> raise_syntax_error
                 ~location
                 "Le mot-clé 'début' est attendu après la définition d'une fonction"
      else
        let node, index =
          match%pcre line with
          (*If*)
          | {|^[ \t]*si[ \t]+(?<condition>.+?)[ \t]+alors[ \t]*$|} ->
            parse_if condition { context with index } location
          (*Else if*)
          | {|^[ \t]*sinon([ \t]*|_)si[ \t]+(?<condition>.+)[ \t]+alors[ \t]*$|} ->
            parse_else_if condition { context with index } location
          (*Else*)
          | {|^[ \t]*sinon[ \t]*$|} ->
            parse_else { context with index } location
          (*For*)
          | {|^[ \t]*pour[ \t]+(?<var>[A-Za-z_]\w*)[ \t]+de[ \t]+(?<start>.+?)[ \t]+jusqu(e |_|')a[ \t](?<end_>.+?)[ \t]faire[ \t]*$|} ->
            parse_for var start end_ { context with index } location
          (*For each*)
          | {|^[ \t]*pour(_| )chaque[ \t]+(?<var>[A-Za-z_]\w*)[ \t]+dans[ \t]+(?<iter>.+?)[ \t]*faire[ \t]*$|} ->
            parse_for_each var iter { context with index } location
          (*While*)
          | "^[ \t]*tant(_| )que[ \t]+(?<condition>.+?)[ \t]+faire[ \t]*" ->
            parse_while condition { context with index } location
          (*Assignment*)
          | {|^[ \t]*(?<target>[^\s]+?)[ \t]*<-[ \t]*(?<value>.+?)[ \t]*$|} ->
            let value = expr_of_string value in
            Ast.make_assign ~target ~value ~context ~location, index
          (*Function*)
          | {|^[ \t]*fonction[ \t]*(?<name>[A-Za-z_]\w*)[ \t]*\((?<args>.*)\)[ \t]*->(?<ret_type>.+)[ \t]*$|} ->
            parse_func_definition name args ret_type { context with index } location
          (*Procedure*)
          | {|^[ \t]*procedure[ \t]*(?<name>[A-Za-z_]\w*)[ \t]*\((?<args>.*)\)[ \t]*$|} ->
            parse_proc_definition name args { context with index } location
          (*Return*)
          | "^[ \t]*retourner[ \t]*$" ->
            Ast.make_return ~expr:(Value None) ~context ~location, index
          | "^[ \t]*retourner[ \t]+(?<value>.+)[ \t]*$" ->
            let expr = expr_of_string value in
            Ast.make_return ~expr ~context ~location, index
          | "[ \t]*(?<s>.*)[ \t]*" ->
            if List.mem s terminators then
              Ast.make_end (), index
            else if s = "" then
              raise_syntax_error ~location
                ("Mot-clé '" ^ (List.hd terminators) ^ "' attendu")
            else
              let expr = try_update_err location (fun () -> expr_of_string s) in
              Ast.make_expr ~expr ~context ~location, index
          | _ -> assert false
        in match node with
        | End ->
          Ast.make_body ~children:(List.rev body), index
        | _ -> parse_body terminators (node :: body) { context with index }
    in parse_body terminators [] context

  and parse_if condition context location =
    let target = expr_of_string condition
    and body, index = parse_body ~terminators:["fin"; "fin si"]
        { context with scopes = If :: context.scopes } in
    Ast.make_if ~target ~body ~context ~location, index

  and parse_else context location =
    let body, index = parse_body ~terminators:["fin"; "fin si"] context in
    Ast.make_else ~body ~context ~location, index - 4
  (* TODO Remove this stupid 'index - 4' and replace it with a function that goes backward until it reads 'fin' *)

  and parse_else_if condition context location =
    let target = expr_of_string condition
    and body, index = parse_body ~terminators:["fin"; "fin si"]
        { context with scopes = If :: context.scopes } in
    Ast.make_else_if ~target ~body ~context ~location, index - 4
  (* TODO Same as above *)

  and parse_for var start end_ context location =
    let target = var
    and start = expr_of_string start
    and end_ = expr_of_string (end_ ^ " + 1")
    and body, index = parse_body ~terminators:["fin"; "pour"]
        { context with scopes = For :: context.scopes } in
    Ast.make_for ~target ~start ~end_ ~body ~context ~location, index

  and parse_for_each var iter context location =
    let target = var
    and iter = expr_of_string iter
    and body, index = parse_body ~terminators:["fin"; "fin pour_chaque"; "fin pour chaque"]
        { context with scopes = For :: context.scopes } in
    Ast.make_for_each ~target ~iter ~body ~context ~location, index

  and parse_while condition context location =
    let test = expr_of_string condition
    and body, index = parse_body ~terminators:["fin"; "fin tant que"; "fin tant_que"]
        { context with scopes = While :: context.scopes } in
    Ast.make_while ~test ~body ~context ~location, index

  and parse_func_definition name args_list ret_type context location =
    let split_arg arg =
      let rec split_arg = function
        | [] ->
          raise_syntax_error ~location
            "Peut-être faut_il supprimer une virgule ?"
        | name :: [] ->
          raise_syntax_error ~location
            ("Un type est attendu pour l'argument '" ^ name ^ "'")
        | type_ :: name :: [] -> type_, name
        | h :: t -> let type_, name = split_arg t in
          h ^ " " ^ type_, name
      in arg |> String.trim |> String.split_on_char ' ' |> split_arg
    in
    let args = if String.trim args_list = "" then []
      else String.split_on_char ',' args_list |> List.map split_arg
    and body, index = parse_body ~terminators:["debut"]
        { context with scopes = Func false :: context.scopes }
    in Ast.make_func_definition ~name ~args ~ret_type ~body ~context ~location, index

  and parse_proc_definition name args_list context location =
    parse_func_definition name args_list "rien" context location

  in let ast, _ = parse_body { code; index = 0; scopes = [] } in ast
