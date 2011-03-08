open Cfg
open Visitor
open Utils
open Set
open Cfg_refactor
open Cfg_printer.ErrorPrinter

type after_redirect_return_val = 
  | True
  | False
  | Anything
  | NoRedirect

let is_redirect name redirects = 
  begin try let return_value = StrMap.find name redirects in
	    match return_value with
	      | NoRedirect -> false
	      | _ -> true
    with Not_found -> false
  end


let string_of_after_redirect value = match value with
  | True -> "Always returns true after redirect"
  | False -> "Always returns false after redirect"
  | Anything -> "Anything is possible after redirect"
  | NoRedirect -> "No redirect found"

let evaluate_exn_block eval combine empty block initial = 
  (* First, evaluate the body *)
  let ears_in_body = eval block.exn_body initial in
  (* else is after the body *)
  let e_else = match block.exn_else with
    | Some(stmt) -> [eval stmt ears_in_body]
    | None -> [ears_in_body]
  in
  (* rescue statements are executed seperately from body. *)
  let rescue_stmts = List.map (fun rb -> rb.rescue_body) block.exn_rescue in
  let rescue_eval = List.map (fun stmt -> eval stmt initial) rescue_stmts in
	  
  (* ensure is called after the else and after each rescue statement *)
  let e_ensure = 
    let inputs = e_else @ rescue_eval in
    match block.exn_ensure with
      | Some(stmt) -> 
	List.map (fun prev -> eval stmt prev) inputs
      | None -> inputs
  in
  List.fold_left combine empty e_ensure



let possible_return_val_after_redirect ?(super=false) redirects cfg = 
  let rec return_val_after_redirect stmt prev = 
    let after_ear = fst(prev) in
    let prev_return = snd(prev) in
    let next = after_ear, prev_return in
    let combine_return  r1 r2 = 
      match r1, r2 with
	| NoRedirect, other 
	| other, NoRedirect -> other
	| True, True -> True
	| False, False -> False
	| _, _  -> Anything
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
	  true, NoRedirect
	else
	  next
      | MethodCall(_, ({mc_target = None; mc_msg = `ID_MethodName(name)} as mc)) -> 
	if is_redirect name redirects then 
	  true, NoRedirect
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
	  false, combine_return prev_return False
	else
	  false, prev_return
      | Return(Some( `ID_True)) 
      | Next(Some( `ID_True)) ->
	if after_ear then
	  false, combine_return prev_return True
	else
	  false, prev_return
      | Return _ 
      | Next _ ->
	if after_ear then
	  false, Anything
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
  let initial = (false, NoRedirect) in
  let is_ear, result = return_val_after_redirect cfg initial in
  result


class findAllPossibleReturnValuesForRedirects redirects = object(self)
  inherit default_visitor as super
    
  val mutable redirect_map = redirects

  method visit_stmt node = match node.snode with
    | Method(Instance_Method(`ID_MethodName("redirect_to" as name)), _, stmt) ->
      begin
	redirect_map <- StrMap.add name (possible_return_val_after_redirect ~super:true redirect_map stmt) redirect_map;
	SkipChildren
      end
    | Method(Instance_Method(`ID_MethodName(name)), _, stmt)
    | Method(Singleton_Method(_, `ID_MethodName(name)), _, stmt) -> 
      begin
	redirect_map <- StrMap.add name (possible_return_val_after_redirect redirect_map stmt) redirect_map;
	SkipChildren
      end
    | _ ->
      super#visit_stmt node

  method redirect_map = redirect_map
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
      begin try let return_value = StrMap.find method_name redirects in
		match return_value with
		  | True -> Hashtbl.add variable_values name true; super#visit_stmt node
		  | False -> Hashtbl.add variable_values name false; super#visit_stmt node
		  | _ -> () ; super#visit_stmt node
	with Not_found -> super#visit_stmt node
      end
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
      let new_redirects = new_redirects_visitor#redirect_map in
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

let findEAR redirects cfg = 
  let returns_reset = ref(true) in
  let rec findEAR stmt prev = 
    let ear_set = fst(prev) in
    let prev_ear = snd(prev) in
    let combine r1 r2 = 
      let combine_ear = StmtSet.union (snd r1) (snd r2) in
      let combine_set = StmtSet.union (fst r1) (fst r2) in
      (combine_set, combine_ear)
    in
    
    let next = (StmtSet.union ear_set prev_ear), prev_ear in

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
	List.fold_left combine (StmtSet.empty, StmtSet.empty) (when_results @ [else_result])
      | Case({case_else=None} as block) -> 
	let when_stmts = (List.map snd block.case_whens) in
	let when_results = (List.map (fun stmt -> findEAR stmt next) when_stmts) in
	List.fold_left combine (StmtSet.empty, StmtSet.empty) when_results
      | While(expr, stmt) -> findEAR stmt next
      | For(_, _, stmt) -> findEAR stmt next
      (* This is for when we find a redirect_to call *)
      | MethodCall(_, ({mc_target = None; mc_msg = `ID_MethodName(name)} as mc)) -> 
	if is_redirect name redirects then 
	  fst(next), StmtSet.add stmt prev_ear
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
	let block_for_ears = evaluate_exn_block (returns_reset_find_ear false) combine (StmtSet.empty, StmtSet.empty) block next in
	let block_next_ears = evaluate_exn_block (returns_reset_find_ear true) combine (StmtSet.empty, StmtSet.empty) block next in
	begin
	  match block.exn_ensure with
	    | Some(_) -> (fst block_for_ears, snd block_next_ears)
	    | None -> block_next_ears
	end
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
	StmtSet.elements(fst(findEAR cfg (StmtSet.empty, StmtSet.empty)))





let print_ears = List.iter (fun ear -> Printf.printf "EAR found in %s:%d.\n" ear.pos.Lexing.pos_fname ear.pos.Lexing.pos_lnum)

let print_redirects_return = StrMap.iter (fun str return_val -> Printf.printf "%s %s\n" str (string_of_after_redirect return_val))

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
    (*let app_controller_cfg = visit_stmt (new createEnsureSemantics :> cfg_visitor) app_controller_cfg in *)
    let () = compute_cfg app_controller_cfg in
    let initial_redirect_map = StrMap.add "redirect_to" True (StrMap.empty) in
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
      (*let cfg_fixed_ensure = visit_stmt (new createEnsureSemantics :> cfg_visitor) cfg in *)
      let cfg_prop_redirect, both_return_values = get_and_simplify_all_redirects cfg application_redirects in
      if verbose then
	print_redirects_return both_return_values;
      let cfg_no_flash = visit_stmt ((new removeSettingFunctionCalls "flash")  :> cfg_visitor) cfg_prop_redirect in
      let cfg_no_session = visit_stmt ((new removeSettingFunctionCalls "session")  :> cfg_visitor) cfg_no_flash in
      if verbose then
	print_cfg cfg_no_session;
      let ears = findEAR both_return_values cfg_no_session in
      let are_ears = (List.length ears != 0) in
      if are_ears then
	print_ears ears
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
  let opts = [
    ('v', "verbose", Some(fun () -> verbose := true), None)
    ]
  in
  let directory = ref (".") in
  Getopt.parse_cmdline opts (fun s -> directory := s);
  if is_rails_directory(!directory) then
    find_all_ears !directory !verbose
  else  
    Printf.eprintf "'%s' is not a valid rails directory\n" !directory

