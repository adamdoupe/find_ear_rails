open Cfg
open Visitor
open Utils
open Set
open Cfg_refactor
open Cfg_printer.ErrorPrinter

class removeDeadCode = object(self)
  inherit default_visitor as super

  method visit_stmt node = match node.snode with
    | If(`ID_True,if_true,if_false) -> ChangeTo(if_true)

    | If(`ID_False,if_true,if_false)
    | If(`ID_Nil,if_true,if_false) -> ChangeTo(if_false)

    | _ -> super#visit_stmt node
end

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
  

let possible_return_val_after_redirect redirects cfg = 
  let rec return_val_after_redirect stmt prev = 
    let after_ear = fst(prev) in
    let prev_return = snd(prev) in
    let next = after_ear, prev_return in
    let combine (after1, r1) (after2,r2) = 
      let combine_after = after1 or after2 in
      let combine_return = 
	match r1, r2 with
	  | NoRedirect, other 
	  | other, NoRedirect -> other
	  | True, True -> True
	  | False, False -> False
	  | _, _  -> Anything
      in
      combine_after, combine_return
    in
    let rec find_in_seq lst prev = match lst with 
      | [] -> prev
      | x :: xs -> find_in_seq xs (return_val_after_redirect x prev) in
    match stmt.snode with    
      | MethodCall(_, ({mc_target = None; mc_msg = `ID_MethodName(name)} as mc)) -> 
	if is_redirect name redirects then 
	  true, Anything
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
	  false, False
	else
	  false, prev_return
      | Return(Some( `ID_True)) 
      | Next(Some( `ID_True)) ->
	if after_ear then
	  false, True
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
      | Case({case_else=Some(stmt as block_else)} as block) -> combine (find_in_seq (List.map snd block.case_whens) next) (return_val_after_redirect block_else next)
      | Case({case_else=None} as block) -> find_in_seq (List.map snd block.case_whens) next 
      | While(expr, stmt) -> return_val_after_redirect stmt next
      | For(_, _, stmt) -> return_val_after_redirect stmt next
      (* This is for when we find a redirect_to call *)
      | Module(_, _, stmt) -> return_val_after_redirect stmt (false, prev_return)
      | Method(_, _, stmt) -> return_val_after_redirect stmt (false, prev_return)
      | Class(_, _, stmt) -> return_val_after_redirect stmt (false, prev_return)
      | ExnBlock block -> 
	let e_else = match block.exn_else with
	  | Some(stmt) -> [return_val_after_redirect stmt next]
	  | None -> []
	in
	let e_ensure = match block.exn_ensure with
	  | Some(stmt) -> [return_val_after_redirect stmt next]
	  | None -> []
	in
	let rescue_stmts = List.map (fun rb -> rb.rescue_body) block.exn_rescue in
	let rescue_ears = List.map (fun stmt -> return_val_after_redirect stmt next) rescue_stmts in
	List.fold_left combine prev ([return_val_after_redirect block.exn_body next] @ rescue_ears @ e_else @ e_ensure) 
      | Begin(stmt) -> return_val_after_redirect stmt next
      | End(stmt) -> return_val_after_redirect stmt next
      | Defined(_,stmt) -> return_val_after_redirect stmt next

      (* An expression with just nil doesn't do anything *)
      | Expression `ID_Nil -> prev

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

module OrderedStmt = struct
  type t = stmt
  let compare stmt1 stmt2 = Pervasives.compare stmt1.sid stmt2.sid
  
end

module StmtMap = Map.Make(OrderedStmt) 

let rec has_a_redirect_method methods stmt = 
  let combine stmts =
    List.fold_left (or) (false) (List.map (has_a_redirect_method methods) stmts) in
  match stmt.snode with    
    | MethodCall(_, {mc_target = None; mc_msg = `ID_MethodName(name)}) -> StrSet.mem name methods
    | Seq lst -> combine lst
    | If(expr,if_true,if_false) -> (has_a_redirect_method methods if_true) or (has_a_redirect_method methods if_false)
    | Case({case_else=Some(stmt as block_else)} as block) -> (combine (List.map snd block.case_whens)) or (has_a_redirect_method methods block_else)
    | Case({case_else=None} as block) -> combine (List.map snd block.case_whens)
    | While(expr, stmt) -> has_a_redirect_method methods stmt
    | For(_, _, stmt) -> has_a_redirect_method methods stmt
    | MethodCall(_, {mc_cb = Some( CB_Block (params, block))}) -> has_a_redirect_method methods block
    | Module(_, _, stmt) -> false
    | Method(_, _, stmt) -> false
    | Class(_, _, stmt) -> false
    | ExnBlock block -> 
      let e_else = match block.exn_else with
	| Some(stmt) -> has_a_redirect_method methods stmt
	| None -> false
	in
	let e_ensure = match block.exn_ensure with
	  | Some(stmt) -> has_a_redirect_method methods stmt
	  | None -> false
	in
	let rescue_stmts = List.map (fun rb -> rb.rescue_body) block.exn_rescue in
	let rescue_has_redirect = combine rescue_stmts in
	(has_a_redirect_method methods block.exn_body) or rescue_has_redirect or e_else or e_ensure 
      | Begin(stmt)
      | End(stmt) 
      | Defined(_,stmt) 
	-> has_a_redirect_method methods stmt 
	
      | _ -> false

class findAllPossibleReturnValuesForRedirects redirects = object(self)
  inherit default_visitor as super
    
  val mutable redirect_map = redirects

  method visit_stmt node = match node.snode with
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


class findRedirectReturnMethods initial_redirect_methods = object(self)
  inherit default_visitor as super

  val mutable redirect_methods = initial_redirect_methods

  method visit_stmt node = match node.snode with
    | Method(Instance_Method(`ID_MethodName(name)), _, stmt)
    | Method(Singleton_Method(_, `ID_MethodName(name)), _, stmt) -> 
      if (has_a_redirect_method redirect_methods stmt) then
	redirect_methods <- (StrSet.add name redirect_methods)
      else
	()
      ;
      super#visit_stmt node
    | _ -> super#visit_stmt node
      
  method redirect_methods = redirect_methods

end

class propogateRedirectToReturnValue redirects = object(self)
  inherit default_visitor as super

  val redirects = redirects
  val mutable true_values = Hashtbl.create 10

  method visit_stmt node = match node.snode with
    | MethodCall(Some(`ID_Var(_, name)), ({mc_msg = `ID_MethodName(method_name)})) ->  
      begin try let return_value = StrMap.find method_name redirects in
		match return_value with
		  | True -> Hashtbl.add true_values name true; super#visit_stmt node
		  | False -> Hashtbl.add true_values name false; super#visit_stmt node
		  | _ -> () ; super#visit_stmt node
	with Not_found -> super#visit_stmt node
      end
    | If(`ID_Var(_, name), if_true, if_false) ->       
      begin try let return_value = Hashtbl.find true_values name in
		match return_value with
		  | true -> ChangeTo(if_true)
		  | false -> ChangeTo(if_false)
	with Not_found -> super#visit_stmt node
      end
    | Return(Some(`ID_Var(_, name))) ->
      begin try let return_value = Hashtbl.find true_values name in
		match return_value with
		  | true -> ChangeTo(update_stmt node (Return(Some(`ID_True))))
		  | false -> ChangeTo(update_stmt node (Return(Some(`ID_False))))
	with Not_found -> super#visit_stmt node
      end      
    | _ -> super#visit_stmt node
end

let find_all_redirect_return_value redirects cfg = 
  let visitor = new findAllPossibleReturnValuesForRedirects (redirects) in
  let _ = visit_stmt (visitor :> cfg_visitor) cfg in
  visitor#redirect_map


let get_and_simplify_all_redirects cfg redirects =
  let one_redirect_pass redirects cfg =
      let cfg_prop_redirect = visit_stmt (new propogateRedirectToReturnValue( redirects ) :> cfg_visitor) cfg in
      let new_redirects_visitor = new findAllPossibleReturnValuesForRedirects( redirects ) in
      let _ = visit_stmt (new_redirects_visitor :> cfg_visitor) cfg_prop_redirect in
      cfg_prop_redirect, new_redirects_visitor#redirect_map
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
      
  

let find_all_redirects ?(initial_redirects=(StrSet.add "redirect_to" (StrSet.empty))) cfg = 
  let one_redirect_pass redirects = 
    let visitor = new findRedirectReturnMethods (redirects) in
    let _ = visit_stmt (visitor :> cfg_visitor) cfg in
    visitor#redirect_methods
  in
  let previous = ref( initial_redirects ) in
  let next = ref (one_redirect_pass !previous) in
  while (StrSet.compare !previous !next) != 0 do
    previous := !next;
    next := one_redirect_pass !previous
  done;
  !next


class removeFlashCalls = object(self)
  inherit default_visitor as super

  val mutable flash_vars = Hashtbl.create 10

  method visit_stmt node = match node.snode with
    | MethodCall(Some(`ID_Var(_, name)), ({mc_msg = `ID_MethodName("flash")})) -> Hashtbl.add flash_vars name true;
      ChangeTo(update_stmt node (Expression(`ID_Nil)))
    | MethodCall(_, ({mc_target = Some(`ID_Var(_, name)); mc_msg = `ID_Operator(Op_ASet)})) ->
      begin try let _ = Hashtbl.find flash_vars name in
		ChangeTo(update_stmt node (Expression(`ID_Nil)))
	with Not_found -> super#visit_stmt node
      end
    | _ -> super#visit_stmt node
      
end

class removeSessionCalls = object(self)
  inherit default_visitor as super

  val mutable session_vars = Hashtbl.create 10

  method visit_stmt node = match node.snode with
    | MethodCall(Some(`ID_Var(_, name)), ({mc_msg = `ID_MethodName("session")})) -> Hashtbl.add session_vars name true;
      ChangeTo(update_stmt node (Expression(`ID_Nil)))
    | MethodCall(_, ({mc_target = Some(`ID_Var(_, name)); mc_msg = `ID_Operator(Op_ASet)})) ->
      begin try let _ = Hashtbl.find session_vars name in
		ChangeTo(update_stmt node (Expression(`ID_Nil)))
	with Not_found -> super#visit_stmt node
      end
    | _ -> super#visit_stmt node
end

let findEAR redirects cfg = 
  let rec findEAR stmt prev = 
    let ear_set = fst(prev) in
    let prev_ear = snd(prev) in
    let add_ear set prev = match prev with
      | None -> set
      | Some(ear) -> StmtSet.add ear set in
    let combine r1 r2 = 
      let combine_ear = match ((snd r1), (snd r2)) with
	| Some(ear), _ 
	| _, Some(ear)  
	  -> Some(ear)
	| None, None -> None in
      let combine_set = StmtSet.union (fst r1) (fst r2) in
      (combine_set, combine_ear)
    in
    
    let next = (add_ear ear_set prev_ear), prev_ear in

    let rec find_in_seq lst prev = match lst with 
      | [] -> prev
      | x :: xs -> find_in_seq xs (findEAR x prev) in
    match stmt.snode with    
      | Seq lst -> find_in_seq lst next
      | If(expr,if_true,if_false) -> combine (findEAR if_true next) (findEAR if_false next)
      | Case({case_else=Some(stmt as block_else)} as block) -> combine (find_in_seq (List.map snd block.case_whens) next) (findEAR block_else next)
      | Case({case_else=None} as block) -> find_in_seq (List.map snd block.case_whens) next 
      | While(expr, stmt) -> findEAR stmt next
      | For(_, _, stmt) -> findEAR stmt next
	(* This is for when we find a redirect_to call *)
      | MethodCall(_, ({mc_target = None; mc_msg = `ID_MethodName(name)} as mc)) -> 
	if is_redirect name redirects then 
	  fst(prev), Some(stmt)
	else
	  begin
	    match mc with
	      | {mc_cb = Some( CB_Block(_, block))} -> findEAR block next
	      | _ -> next
	  end
      | Module(_, _, stmt) -> findEAR stmt (fst prev, None)
      | Method(_, _, stmt) -> findEAR stmt (fst prev, None)
      | Class(_, _, stmt) -> findEAR stmt (fst prev, None)
      | ExnBlock block -> 
	let e_else = match block.exn_else with
	  | Some(stmt) -> [findEAR stmt next]
	  | None -> []
	in
	let e_ensure = match block.exn_ensure with
	  | Some(stmt) -> [findEAR stmt next]
	  | None -> []
	in
	let rescue_stmts = List.map (fun rb -> rb.rescue_body) block.exn_rescue in
	let rescue_ears = List.map (fun stmt -> findEAR stmt next) rescue_stmts in
	List.fold_left combine (StmtSet.empty, None) ([findEAR block.exn_body next] @ rescue_ears @ e_else @ e_ensure) 
      | Begin(stmt) -> findEAR stmt next
      | End(stmt) -> findEAR stmt next
      | Defined(_,stmt) -> findEAR stmt next

	(* An expression with just nil doesn't do anything *)
      | Expression `ID_Nil -> prev

	(* A return or next doesn't count, and resets the after_redirect flag *)
      | Return _ 
      | Next _
	-> (fst prev, None)

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
  StmtSet.elements(fst(findEAR cfg (StmtSet.empty, None)))





let print_ears = List.iter (fun ear -> Printf.printf "EAR found in %s:%d.\n" ear.pos.Lexing.pos_fname ear.pos.Lexing.pos_lnum)

let print_redirects_return = StrMap.iter (fun str return_val -> Printf.printf "%s %s\n" str (string_of_after_redirect return_val))

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
    let initial_redirect_map = StrMap.add "redirect_to" True (StrMap.empty) in
    let _, all_return_values = get_and_simplify_all_redirects app_controller_cfg initial_redirect_map in
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
      let cfg_no_flash = visit_stmt (new removeFlashCalls :> cfg_visitor) cfg_prop_redirect in
      let cfg_no_session = visit_stmt (new removeSessionCalls :> cfg_visitor) cfg_no_flash in
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

