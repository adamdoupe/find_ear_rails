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


class propogateRedirectToReturnValue = object(self)
  inherit default_visitor as super

  val mutable true_values = Hashtbl.create 10

  method visit_stmt node = match node.snode with
    | MethodCall(Some(`ID_Var(_, name)), ({mc_msg = `ID_MethodName("redirect_to")} as mc)) ->  Hashtbl.add true_values name true;
      ChangeTo(update_stmt node (MethodCall(None, mc)))
    | If(`ID_Var(_, name), if_true, if_false) ->       
      begin try let _ = Hashtbl.find true_values name in
	    ChangeTo(if_true)
	with Not_found -> super#visit_stmt node
      end

    | _ -> super#visit_stmt node
end

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


module OrderedStmt = struct
  type t = stmt
  let compare stmt1 stmt2 = Pervasives.compare stmt1.sid stmt2.sid
  
end

module StmtMap = Map.Make(OrderedStmt) 

let findEAR cfg = 
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
      | MethodCall(_, {mc_msg = `ID_MethodName("redirect_to")}) -> fst(next), Some(stmt)
      | MethodCall(_, {mc_cb = Some( CB_Block (params, block))}) -> findEAR block next
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

let find_an_ear fname = 
  let loader = File_loader.create File_loader.EmptyCfg [] in
  let s = File_loader.load_file loader fname in
  let () = compute_cfg s in
  (* let () = compute_cfg_locals s in *)
  let s' = visit_stmt (new propogateRedirectToReturnValue :> cfg_visitor) s in
  let s'' = visit_stmt (new removeFlashCalls :> cfg_visitor) s' in
  let s''' = visit_stmt (new removeSessionCalls :> cfg_visitor) s'' in
  let ears = findEAR s''' in
  let are_ears = (List.length ears != 0) in
  if are_ears then
    print_ears ears
  else
    Printf.printf "No EARs found in %s.\n" (fname)

let find_all_ears directory = 
  let rec all_files directory = 
    let dir_files = Array.to_list( Sys.readdir directory ) in
    let with_full_path = List.map (Filename.concat directory) dir_files in    
    let files = List.filter (fun x -> not(Sys.is_directory x)) with_full_path in
    let directories = List.filter Sys.is_directory with_full_path in
    let other_files = List.flatten( List.rev_map all_files directories) in
    files @ other_files 
  in 
  let files = all_files (Filename.concat directory "/app/controllers/") in
  let _ = List.map find_an_ear files in
  ()
    

let is_rails_directory directory = 
  let controller_dir = Filename.concat directory "/app/controllers/" in
  Sys.file_exists controller_dir && Sys.is_directory controller_dir

let _ =   
  let directory = Sys.argv.(1) in
  if is_rails_directory(directory) then
    find_all_ears directory
  else  
    Printf.eprintf "'%s' is not a valid rails directory" directory

