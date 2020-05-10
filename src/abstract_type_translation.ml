open Utils 
open Errors
open Structures
open Getters 
open Expressions

(*AUX*)
let get_attr_meths name context = 
    match (StringMap.find name context.vars) with 
        |`Custom (_ , attr_meths, are_set) -> attr_meths, are_set
        | _ -> failwith "Internal missuse of get_attr_meth, No class in context.vars"

let rec get_current_class_name context = 
    match context.scopes with 
        |[] -> failwith "Internal error: get_current_class_name is not used with a class context"
        |(Class_def name)::_ -> name 
        |_::r -> get_current_class_name {context with scopes = r} 
(*CORE*)
(*returns the translation of the lines that set a default value for an attribute*)
let eval_attributes context = 
    let rec manage_line length line i vars class_name result = 
        if i >= length then 
           vars, ""
        else
           let word, i = get_word line (ignore_chrs line i) in 
           if word = "<-" then 
               let expr, _ = get_word line (ignore_chrs line i) in 
               let name, _ = get_word result 5 in 
               let expr = expr_of_string expr in 
               let attr_meths, are_set = get_attr_meths class_name {context with vars = vars}  in 
               if (StringMap.find name attr_meths) = type_of_expr attr_meths expr then 
                   let expr = string_of_expr expr in
                   let result = result ^ " = " ^ expr^"\n" in
                   let are_set = StringMap.add name true are_set in
                   (StringMap.add class_name (`Custom (class_name, attr_meths, are_set)) vars, result)
               else 
                  raise_type_error "Given expression does not match the declared type" ~line: (get_line_no context.code context.index)
           else 
               let type_ = Type.of_string word in
               (*getting var name*)
               let name, i = get_word line (ignore_chrs line i) in 
               let attr_meths, are_set = get_attr_meths class_name {context with vars = vars} in 
               let attr_meths = StringMap.add name type_ attr_meths in 
               let are_set = StringMap.add name false are_set in 
               let result = "self."^name in 
               manage_line length line i (StringMap.add class_name (`Custom (class_name, attr_meths, are_set)) vars) class_name result
    in 
    let rec _main_process context result = 
        let line, i = get_line context.code context.index in 
        if line = "" || line = "methodes"  then 
           {context with index = i ; scopes = (Attributes result):: List.tl context.scopes}
        else if line = "fin"then 
           {context with scopes = (Attributes result) :: List.tl context.scopes}
        else
            let vars, l_result =  manage_line (String.length line) line 0 context.vars (get_current_class_name context) "" in 
            _main_process {context with index = i ; vars = vars} (result^l_result) 
    in
    _main_process context "" 
  
    
     

