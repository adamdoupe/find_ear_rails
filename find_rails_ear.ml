open Cfg
open Visitor
open Utils
open Set
open Cfg_refactor
open Cfg_printer.ErrorPrinter
open Parse_ruby_methods

type after_redirect_return_val = 
  | True
  | False
  | Anything
  | NoRedirect

type redirect_function = 
    {
      ret_val : after_redirect_return_val;
      name : string;
      fun_called : redirect_function option;
    }

module OrderedRedirectFunction = struct
  type t = redirect_function
  let compare rf1 rf2 = Pervasives.compare rf1.name rf2.name
end

module RedirectFunctionMap = Map.Make(OrderedRedirectFunction)

let is_rf_with_name = StrMap.mem

let is_redirect name redirects = 
  is_rf_with_name name redirects


let string_of_after_redirect value = match value with
  | True -> "Always returns true after redirect"
  | False -> "Always returns false after redirect"
  | Anything -> "Anything is possible after redirect"
  | NoRedirect -> "No redirect found"

let rec string_call_stack redirect_function = match redirect_function.fun_called with
  | None -> redirect_function.name
  | Some(other_function) -> Printf.sprintf "%s -> %s" redirect_function.name (string_call_stack other_function) 

let print_redirects_return = StrMap.iter (fun str return_function -> Printf.printf "%s %s\n\tAnd has the following call-stack %s\n" str (string_of_after_redirect return_function.ret_val) (string_call_stack return_function))

let possible_return_val_after_redirect ?(super=false) redirects cfg = 
  let rec return_val_after_redirect stmt prev = 
    let after_ear = fst(prev) in
    let prev_return = snd(prev) in
    let prev_found_redirect = snd(prev_return) in
    let next = after_ear, prev_return in
    let combine_return  r1 r2 = 
      let value_1, value_2 = fst(r1), fst(r2) in
      let name_1, name_2 = snd(r1), snd(r2) in
      let combined_value = 
	match value_1, value_2 with
	  | NoRedirect, other 
	  | other, NoRedirect -> other
	  | True, True -> True
	  | False, False -> False
	  | _, _  -> Anything
      in
      let combined_name = 
	match name_1, name_2 with
	  | Some(name), _
	  | _, Some(name)
	    -> Some(name)
	  | _, _ -> name_1
      in
      combined_value, combined_name
    in
    let combine (after1, r1) (after2,r2) = 
      let combine_after = after1 or after2 in
      combine_after, combine_return r1 r2
    in
    let rec find_in_seq lst prev = match lst with 
      | [] -> prev
      | x :: xs -> find_in_seq xs (return_val_after_redirect x prev) in
    match stmt.snode with    
      | MethodCall(_, ({mc_msg = `ID_Super})) ->
	if super then
	  true, (NoRedirect, Some("to_return"))
	else
	  next
      | MethodCall(_, ({mc_target = None; mc_msg = `ID_MethodName(name)} as mc)) -> 
	if is_redirect name redirects then 
	  true, (NoRedirect, Some(name))
	else
	  begin
	    match mc with
	      | {mc_cb = Some( CB_Block(_, block))} -> return_val_after_redirect block next
	      | _ -> next
	  end
      | Return(Some( `ID_Nil))
      | Next(Some( `ID_Nil))
      | Return(Some(`ID_False)) 
      | Next(Some(`ID_False))
      | Return(None) 
      | Next(None) ->
	if after_ear then
	  false, combine_return prev_return (False, None)
	else
	  false, prev_return
      | Return(Some( `ID_True)) 
      | Next(Some( `ID_True)) ->
	if after_ear then
	  false, combine_return prev_return (True, None)
	else
	  false, prev_return
      | Return _ 
      | Next _ ->
	if after_ear then
	  false, (Anything, prev_found_redirect)
	else
	  false, prev_return
      | Seq lst -> find_in_seq lst next
      | If(expr,if_true,if_false) -> combine (return_val_after_redirect if_true next) (return_val_after_redirect if_false next)
      | Case({case_else=Some(stmt as block_else)} as block) -> 
	let else_result = (return_val_after_redirect block_else next) in
	let when_stmts = (List.map snd block.case_whens) in
	let when_results = (List.map (fun stmt -> return_val_after_redirect stmt next) when_stmts) in
	List.fold_left combine prev (when_results @ [else_result])
      | Case({case_else=None} as block) -> 
	let when_stmts = (List.map snd block.case_whens) in
	let when_results = (List.map (fun stmt -> return_val_after_redirect stmt next) when_stmts) in
	List.fold_left combine prev when_results
      | While(expr, stmt) -> return_val_after_redirect stmt next
      | For(_, _, stmt) -> return_val_after_redirect stmt next
      (* This is for when we find a redirect_to call *)
      | Module(_, _, stmt) -> return_val_after_redirect stmt (false, prev_return)
      | Method(_, _, stmt) -> return_val_after_redirect stmt (false, prev_return)
      | Class(_, _, stmt) -> return_val_after_redirect stmt (false, prev_return)
      | ExnBlock block -> 
	(* Don't care about the ensure clause, since that never returns anything *)
	(* First, evaluate the body *)
	let ears_in_body = return_val_after_redirect block.exn_body next in
	(* else is after the body *)
	let e_else = match block.exn_else with
	  | Some(stmt) -> [return_val_after_redirect stmt ears_in_body]
	  | None -> [ears_in_body]
	in
	(* rescue statements can execute after the body. Use that as input *)
	let rescue_stmts = List.map (fun rb -> rb.rescue_body) block.exn_rescue in
	let rescue_ears = List.map (fun stmt -> return_val_after_redirect stmt ears_in_body) rescue_stmts in

	(* ensure is called after the else and after each rescue statement *)
	let e_ensure = 
	  let inputs = e_else @ rescue_ears in
	  match block.exn_ensure with
	    | Some(stmt) -> 
	      List.map (fun prev_ear -> return_val_after_redirect stmt prev_ear) inputs
	    | None -> inputs
	in

	List.fold_left combine prev e_ensure

      | Begin(stmt) -> return_val_after_redirect stmt next
      | End(stmt) -> return_val_after_redirect stmt next
      | Defined(_,stmt) -> return_val_after_redirect stmt next

      | MethodCall(_,_) 
      | Break _
      | Redo
      | Retry
      | Undef _
      | Yield _
      | Expression _
      | Assign _
      | Alias _
	-> next
  in
  let initial = (false, (NoRedirect, None)) in
  let is_ear, result = return_val_after_redirect cfg initial in
  result


class findAllPossibleReturnValuesForRedirects redirects = object(self)
  inherit default_visitor as super
    
  val mutable redirects = redirects

  method visit_stmt node = match node.snode with
    | Method(Instance_Method(`ID_MethodName("redirect_to" as name)), _, stmt) ->
      begin
	let after_redirect, redirect_function_name = possible_return_val_after_redirect ~super:true redirects stmt in
	match after_redirect with 
	  | NoRedirect -> SkipChildren
	  | _ ->
	    redirects <- StrMap.add name {name=name; ret_val=after_redirect; fun_called = None}  redirects;
	    SkipChildren
      end
    | Method(Instance_Method(`ID_MethodName(name)), _, stmt)
    | Method(Singleton_Method(_, `ID_MethodName(name)), _, stmt) -> 
      begin
	let after_redirect, redirect_function_name = possible_return_val_after_redirect redirects stmt in
	match after_redirect with 
	  | NoRedirect -> SkipChildren
	  | _ ->
	    let redirect_function_name = match redirect_function_name with
	      | Some(n) -> n
	      | None -> ""
	    in
	    let redirect_function_called = StrMap.find redirect_function_name redirects in
	    redirects <- StrMap.add name {name=name; ret_val=after_redirect; fun_called = Some(redirect_function_called)}  redirects;
	    SkipChildren
      end
    | _ ->
      super#visit_stmt node

  method redirects = redirects
end

class propogateRedirectToReturnValue ?(in_redirect_to=false) redirects = object(self)
  inherit default_visitor as super

  val redirects = redirects
  val mutable variable_values = Hashtbl.create 10

  method visit_stmt node = match node.snode with
    | Method(Instance_Method(`ID_MethodName("redirect_to")), args, stmt) -> 
      let stmt' = visit_stmt ((new propogateRedirectToReturnValue ~in_redirect_to:true  redirects) :> cfg_visitor) stmt in
      let new_node = Method(Instance_Method(`ID_MethodName("redirect_to")), args, stmt') in
      ChangeTo(update_stmt node new_node)
    | MethodCall(Some(`ID_Var(_, name)), ({mc_msg = `ID_Super})) ->
      if in_redirect_to then
	begin
	  Hashtbl.add variable_values name true; 
	  super#visit_stmt node
	end
      else
	super#visit_stmt node
    | MethodCall(Some(`ID_Var(_, name)), ({mc_msg = `ID_MethodName(method_name)})) ->  
      begin try let redirect_function = StrMap.find method_name redirects in
		let return_value = redirect_function.ret_val in
		match return_value with
		  | True -> Hashtbl.add variable_values name true; super#visit_stmt node
		  | False -> Hashtbl.add variable_values name false; super#visit_stmt node
		  | _ -> () ; super#visit_stmt node
	with Not_found -> super#visit_stmt node
      end
    (* This case is to handle the not function *)
    | If(`ID_Var(_, name), {snode=Assign(`ID_Var(_, new_name_false), `ID_False)}, {snode=Assign(`ID_Var(_, new_name_true), `ID_True)}) ->
      if new_name_false == new_name_true then
	begin try let return_value = Hashtbl.find variable_values name in
		  match return_value with
		    | true -> Hashtbl.add variable_values new_name_false false; ChangeTo(update_stmt node (Expression(`ID_Nil)))
		    | false -> Hashtbl.add variable_values new_name_false true; ChangeTo(update_stmt node (Expression(`ID_Nil)))
	  with Not_found -> super#visit_stmt node
	end 
      else
	super#visit_stmt node
    | If(`ID_Var(_, name), if_true, if_false) ->       
      begin try let return_value = Hashtbl.find variable_values name in
		match return_value with
		  | true -> ChangeTo(if_true)
		  | false -> ChangeTo(if_false)
	with Not_found -> super#visit_stmt node
      end
    | Assign(`ID_Var(_, new_name), `ID_Var(_, old_name)) ->
      begin try let old_value = Hashtbl.find variable_values old_name in
		let new_value = try Hashtbl.find variable_values new_name
		  with Not_found -> old_value
		in
		let have_same_value = old_value == new_value in
		if have_same_value then
		  begin
		    Hashtbl.add variable_values new_name old_value;
		    ChangeTo(update_stmt node (Expression(`ID_Nil)))
		  end
		else
		  begin
		    Hashtbl.remove variable_values new_name;
		    super#visit_stmt node
		  end
	with Not_found -> super#visit_stmt node
      end

    | Return(Some(`ID_Var(_, name))) ->
      begin try let return_value = Hashtbl.find variable_values name in
		match return_value with
		  | true -> ChangeTo(update_stmt node (Return(Some(`ID_True))))
		  | false -> ChangeTo(update_stmt node (Return(Some(`ID_False))))
	with Not_found -> super#visit_stmt node
      end      
    | _ -> super#visit_stmt node
end

let get_and_simplify_all_redirects cfg redirects =
  let one_redirect_pass redirects cfg =
      let new_redirects_visitor = new findAllPossibleReturnValuesForRedirects( redirects ) in
      let _ = visit_stmt (new_redirects_visitor :> cfg_visitor) cfg in
      let new_redirects = new_redirects_visitor#redirects in
      let cfg_prop_redirect = visit_stmt (new propogateRedirectToReturnValue( new_redirects ) :> cfg_visitor) cfg in
      cfg_prop_redirect, new_redirects
  in
  let prev_cfg = ref(cfg) in
  let prev_redirects = ref(redirects) in
  let next = ref(one_redirect_pass !prev_redirects !prev_cfg) in
  while (StrMap.compare (Pervasives.compare) (!prev_redirects) (snd(!next))) != 0 do
    prev_cfg := fst(!next);
    prev_redirects := snd(!next);
    next := one_redirect_pass !prev_redirects !prev_cfg
  done;
  !next
      
  
class removeSettingFunctionCalls method_name = object(self)
  inherit default_visitor as super
    
  val mutable flash_vars = Hashtbl.create 10

  method visit_stmt node = match node.snode with
    | MethodCall(Some(`ID_Var(_, name)), ({mc_msg = `ID_MethodName(method_call)})) -> 
      if (Pervasives.compare method_call method_name) == 0 then
	begin
	  Hashtbl.add flash_vars name true;
	  ChangeTo(update_stmt node (Expression(`ID_Nil)))
	end
      else
	super#visit_stmt node

    | MethodCall(_, ({mc_target = Some(`ID_Var(_, name)); mc_msg = `ID_Operator(Op_ASet)})) ->
      begin try let _ = Hashtbl.find flash_vars name in
		ChangeTo(update_stmt node (Expression(`ID_Nil)))
	with Not_found -> super#visit_stmt node
      end
    | _ -> super#visit_stmt node
      
end

module OrderedStmt = struct
  type t = stmt
  let compare stmt1 stmt2 = Pervasives.compare stmt1.sid stmt2.sid
end

module StmtMap = Map.Make(OrderedStmt)

type ear = 
  | Regular
  | Severe of StmtSet.t

let string_from_ear ear = match ear with
  | Regular -> "Regular"
  | Severe(_) -> "Severe"

let combined_ears ear1 ear2 = match ear1,ear2 with
  | Severe(stmts), Regular
  | Regular, Severe(stmts)
    -> Severe(stmts)
  | Severe(stmts1), Severe(stmts2)
    -> Severe(StmtSet.union stmts1 stmts2)
  | Regular, Regular -> Regular

type method_type = 
  | ClassMethod
  | InstanceMethod

type severe_method = 
  | Severe_Method of method_type * string

let severe_methods = 
  [Severe_Method(ClassMethod, "create"); Severe_Method(InstanceMethod, "update_attribute"); Severe_Method(InstanceMethod, "update_attributes");
   Severe_Method(InstanceMethod, "attributes="); Severe_Method(InstanceMethod, "decrement"); Severe_Method(InstanceMethod, "decrement!"); 
   Severe_Method(InstanceMethod, "delete"); Severe_Method(InstanceMethod, "destroy"); Severe_Method(InstanceMethod, "increment"); 
   Severe_Method(InstanceMethod, "increment!"); Severe_Method(InstanceMethod, "save"); Severe_Method(InstanceMethod, "save!");
   Severe_Method(InstanceMethod, "toggle"); Severe_Method(InstanceMethod, "toggle!"); Severe_Method(InstanceMethod, "touch");
   Severe_Method(InstanceMethod, "update_attributes!");
  ]

let check_method method_call severe_method = 
  let match_class_method method_call name = match method_call with
    | { mc_target = Some(`ID_Var(`Var_Constant,_)); mc_msg = `ID_MethodName(method_name)} -> Pervasives.compare method_name name == 0
    | _ -> false
  in
  let match_instance_method method_call name = match method_call with
    | { mc_target = Some(`ID_Var(`Var_Local,_)); mc_msg = `ID_MethodName(method_name)}
    | { mc_target = Some(`ID_Var(`Var_Instance,_)); mc_msg = `ID_MethodName(method_name)}
    | { mc_target = Some(`ID_Var(`Var_Global,_)); mc_msg = `ID_MethodName(method_name)}
    | { mc_target = Some(`ID_Var(`Var_Class,_)); mc_msg = `ID_MethodName(method_name)}
      -> Pervasives.compare method_name name == 0
    | _ -> false
  in
  match severe_method with
    | Severe_Method(ClassMethod, name) -> match_class_method method_call name
    | Severe_Method(InstanceMethod, name) -> match_instance_method method_call name

let is_severe_method method_call = 
  let any_matches = List.filter (fun severe_method -> check_method method_call severe_method) severe_methods in
  List.length any_matches != 0


let findEAR redirects cfg = 
  let returns_reset = ref(true) in
  let rec findEAR stmt prev = 
    let ear_map = fst(prev) in
    let prev_ear = snd(prev) in
    let combine_found_ears map1 map2 =
      let result = ref(map2) in
      let () = StmtMap.iter (fun stmt ear ->
	begin
	  let found_stmt = StmtMap.mem stmt map2 in
	  if not found_stmt then
	    result := StmtMap.add stmt ear !result
	  else
	    let map2ear = StmtMap.find stmt map2 in
	    let combined_ears = combined_ears ear map2ear in
	    result := StmtMap.add stmt combined_ears !result
	end)
	map1 
      in
      !result
    in
    let set_to_map set = 
      let result = ref(StmtMap.empty) in
      let () = StmtSet.iter (fun stmt -> result := StmtMap.add stmt Regular !result) set in
      !result
    in	
    let combine r1 r2 = 
      let combine_ear = StmtSet.union (snd r1) (snd r2) in
      let map1,map2 = (fst r1),(fst r2) in
      let found_ears = combine_found_ears map1 map2 in
      (found_ears, combine_ear)
    in

    let next = (combine_found_ears ear_map (set_to_map prev_ear)), prev_ear in
    let severe_next stmt = 
      let result = ref(fst(next)) in
      StmtSet.iter (fun ear ->
	begin
	  let prev_result = StmtMap.find ear !result in
	  let new_result = Severe(StmtSet.add stmt StmtSet.empty) in
	  let combined = combined_ears prev_result new_result  in
	  result := StmtMap.add ear combined !result
	end) prev_ear;
      !result
    in
    let rec find_in_seq lst prev = match lst with 
      | [] -> prev
      | x :: xs -> find_in_seq xs (findEAR x prev) in
    match stmt.snode with    
      | Seq lst -> find_in_seq lst next
      | If(expr,if_true,if_false) -> combine (findEAR if_true next) (findEAR if_false next)
      | Case({case_else=Some(stmt as block_else)} as block) -> 
	let else_result = (findEAR block_else next) in
	let when_stmts = (List.map snd block.case_whens) in
	let when_results = (List.map (fun stmt -> findEAR stmt next) when_stmts) in
	List.fold_left combine (StmtMap.empty, StmtSet.empty) (when_results @ [else_result])
      | Case({case_else=None} as block) -> 
	let when_stmts = (List.map snd block.case_whens) in
	let when_results = (List.map (fun stmt -> findEAR stmt next) when_stmts) in
	List.fold_left combine (StmtMap.empty, StmtSet.empty) when_results
      | While(expr, stmt) -> findEAR stmt next
      | For(_, _, stmt) -> findEAR stmt next
      (* This is for when we find a redirect_to call *)
      | MethodCall(_, ({mc_target = target; mc_msg = `ID_MethodName(name)} as mc)) -> 
	let is_redirect = is_redirect name redirects in
	let no_target = target == None in
	if is_redirect && no_target then 
	  fst(next), StmtSet.add stmt prev_ear
	else
	  if is_severe_method mc then
	    begin
	      let new_ears = severe_next stmt in
	      let new_next = new_ears, snd(next) in
	      match mc with
		| {mc_cb = Some( CB_Block(_, block))} -> findEAR block new_next
		| _ -> new_next
	    end
	  else
	  begin
	    match mc with
	      | {mc_cb = Some( CB_Block(_, block))} -> findEAR block next
	      | _ -> next
	  end
      | Module(_, _, stmt) -> findEAR stmt (fst prev, StmtSet.empty)
      | Method(_, _, stmt) -> findEAR stmt (fst prev, StmtSet.empty)
      | Class(_, _, stmt) -> findEAR stmt (fst prev, StmtSet.empty)
      | ExnBlock block -> 
	let returns_reset_find_ear returns_reset_value stmt next = 
	  begin
	    let prev_returns = !returns_reset in
	    returns_reset := returns_reset_value;
	    let result = findEAR stmt next in
	    returns_reset := prev_returns;
	    result
	  end
	in

	let eval_with_both stmt prev = 
	  let first, _ = returns_reset_find_ear true stmt prev in
	  let _, second = returns_reset_find_ear false stmt prev in
	  first, second
	in

	let eval_block eval block = 
	
	  (* First, evaluate the body *)
	  let evaled_body = eval block.exn_body prev in

	  (* else is after the body *)
	  let e_else = match block.exn_else with
	    | Some(stmt) -> [eval stmt evaled_body]
	    | None -> [evaled_body]
	  in

	  (* rescue statements are executed seperately from body. *)
	  let rescue_stmts = List.map (fun rb -> rb.rescue_body) block.exn_rescue in
	  let rescue_eval = List.map (fun stmt -> eval stmt prev) rescue_stmts in
	  
	  (* ensure is called after the else and after each rescue statement *)
	  let e_ensure = 
	    let inputs = e_else @ rescue_eval in
	    match block.exn_ensure with
	      | Some(stmt) -> 
		List.map (fun prev -> findEAR stmt prev) inputs
	      | None -> inputs
	  in
	  List.fold_left combine (StmtMap.empty, StmtSet.empty) e_ensure
	in
	let ears_found, _ = eval_block eval_with_both block in
	let _, future_ears = eval_block findEAR block in
	ears_found, future_ears

      | Begin(stmt) -> findEAR stmt next
      | End(stmt) -> findEAR stmt next
      | Defined(_,stmt) -> findEAR stmt next

      (* An expression with just nil doesn't do anything *)
      | Expression `ID_Nil -> prev

      | Return _ 
      | Next _
	-> if !returns_reset then 
	    (fst prev, StmtSet.empty) 
	  else 
	    prev

      | MethodCall(_,_) 
      | Break _
      | Redo
      | Retry
      | Undef _
      | Yield _
      | Expression _
      | Assign _
      | Alias _
	-> next
	in
  fst(findEAR cfg (StmtMap.empty, StmtSet.empty))

exception WrongEar;;

let print_ears ears redirects = StmtMap.iter (fun ear ear_type -> 
  let return_function = 
    match ear.snode with 
      | MethodCall(_, ({mc_msg = `ID_MethodName(name)})) ->
	StrMap.find name redirects
      | _ -> raise WrongEar
  in
  Printf.printf "%s EAR found in %s:%d.\n\tWith the call graph: %s\n" (string_from_ear ear_type) ear.pos.Lexing.pos_fname ear.pos.Lexing.pos_lnum (string_call_stack return_function);
  match ear_type with
    | Regular -> ();
    | Severe(severes) -> 
      let severe_stmt = StmtSet.choose severes in
      Printf.printf "\tSevere because of %s:%d.\n" severe_stmt.pos.Lexing.pos_fname severe_stmt.pos.Lexing.pos_lnum ;    
) ears

open Cfg_printer
let print_cfg =   
  CodePrinter.print_stmt stdout

let find_all_ears directory verbose = 
  let rec all_files directory = 
    let dir_files = Array.to_list( Sys.readdir directory ) in
    let with_full_path = List.map (Filename.concat directory) dir_files in    
    let files = List.filter (fun x -> not(Sys.is_directory x)) with_full_path in
    let directories = List.filter Sys.is_directory with_full_path in
    let other_files = List.flatten( List.rev_map all_files directories) in
    files @ other_files 
  in 
  let files = all_files (Filename.concat directory "/app/controllers/") in
  let app_controller_name = 
    let full_name = (Filename.concat (Filename.concat directory "/app/controllers/") "application_controller.rb") in
    let other_name = (Filename.concat (Filename.concat directory "/app/controllers/") "application.rb") in
    if Sys.file_exists full_name then
      full_name
    else if Sys.file_exists other_name then
      other_name
    else
      ""
  in
  let controllers = List.filter (fun name -> (String.compare name app_controller_name) != 0) files in
(* first, load the redirects from application_controller *)
  let application_redirects = 
    let loader = File_loader.create File_loader.EmptyCfg [] in
    let app_controller_cfg = File_loader.load_file loader app_controller_name in
    let () = compute_cfg app_controller_cfg in
    let initial_redirect_map = StrMap.add "redirect_to" {ret_val=True; name="redirect_to"; fun_called=None } (StrMap.empty) in
    let app_cfg, all_return_values = get_and_simplify_all_redirects app_controller_cfg initial_redirect_map in
    if verbose then
      print_cfg app_cfg;
    all_return_values
  in
  if verbose then
    print_redirects_return application_redirects;
  let find_ear_controller fname = 
    try 
      let loader = File_loader.create File_loader.EmptyCfg [] in 
      let cfg = File_loader.load_file loader fname in
      let () = compute_cfg cfg in
      let cfg_prop_redirect, both_return_values = get_and_simplify_all_redirects cfg application_redirects in
      if verbose then
	print_redirects_return both_return_values;
      let cfg_no_flash = visit_stmt ((new removeSettingFunctionCalls "flash")  :> cfg_visitor) cfg_prop_redirect in
      let cfg_no_session = visit_stmt ((new removeSettingFunctionCalls "session")  :> cfg_visitor) cfg_no_flash in
      if verbose then
	print_cfg cfg_no_session;
      let ears = findEAR both_return_values cfg_no_session in
      let are_ears = not (StmtMap.is_empty ears) in
      if are_ears then
	print_ears ears both_return_values
      else
	Printf.printf "No EARs found in %s.\n" (fname)
    with _ ->
      Printf.eprintf "Error while parsing %s.\n" (fname) 
  in
  let _ = List.map find_ear_controller controllers in
  ()
    

let is_rails_directory directory = 
  let controller_dir = Filename.concat directory "/app/controllers/" in
  Sys.file_exists controller_dir && Sys.is_directory controller_dir



let _ =   
  let verbose = ref (false) in
  let should_show_methods = ref (false) in
  let opts = [
    ('v', "verbose", Some(fun () -> verbose := true), None);
    ('m', "methods", Some(fun () -> should_show_methods := true), None)
    ]
  in
  let directory = ref (".") in
  Getopt.parse_cmdline opts (fun s -> directory := s);
  if !should_show_methods then
    show_methods !directory
  else
    begin
      if is_rails_directory(!directory) then
	find_all_ears !directory !verbose
      else  
	Printf.eprintf "'%s' is not a valid rails directory\n" !directory
    end

