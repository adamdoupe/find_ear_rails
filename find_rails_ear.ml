open Cfg
open Visitor
open Utils
open Set
open Cfg_refactor
open Cfg_printer.ErrorPrinter

let method_formal_name = function
  | `Formal_meth_id var
  | `Formal_amp var
  | `Formal_star var
  | `Formal_default(var, _) -> var

module ExecutionAfterRedirect = struct
  type t = bool
      
  let empty = false
    
  let eq t1 t2 = t1 == t2

  let to_string = string_of_bool

  let transfer hasEAR stmt = match stmt.snode with
    | MethodCall(_, _) 
      -> let () = Printf.printf "Got here" in
	 true
    | _ -> let () = Printf.printf "%s\n" (string_of_cfg stmt) in
	   hasEAR

  let join = 
    List.exists ((=) true) 

end

module EARFlow = Dataflow.Forwards(ExecutionAfterRedirect)

module NilAnalysis = struct
  type t = fact StrMap.t
  and fact = MaybeNil | NonNil

  let empty = StrMap.empty
  let eq t1 t2 = StrMap.compare Pervasives.compare t1 t2 = 0

  let fact_to_s = function 
    | MaybeNil -> "MaybeNil"
    | NonNil -> "NonNil"
  let to_string t = strmap_to_string fact_to_s t
    
  let meet_fact t1 t2 = match t1,t2 with
    | MaybeNil, _
    | _, MaybeNil -> MaybeNil
    | NonNil, NonNil -> NonNil

  let update s fact map = StrMap.add s fact map

  let meet_fact s v map = 
    let fact = 
      try meet_fact (StrMap.find s map) v
      with Not_found -> v
    in StrMap.add s fact map

  let join lst = 
    let () = Printf.printf "In join\n" in
    List.fold_left (fun acc map -> StrMap.fold meet_fact map acc)
      StrMap.empty lst


  let rec update_lhs fact map lhs = match lhs with
    | `ID_Var(`Var_Local, var) -> update var fact map
    | #identifier -> map
    | `Tuple lst -> List.fold_left (update_lhs MaybeNil) map lst
    | `Star (#lhs as l) -> update_lhs NonNil map l

  let transfer map stmt = 
    let () = Printf.printf "In transfer\n" in
    match stmt.snode with
    | Assign(lhs, #literal) -> update_lhs NonNil map lhs
    | Assign(lhs, `ID_Var(`Var_Local, rvar)) -> update_lhs (StrMap.find rvar map) map lhs
    | MethodCall(lhs_o, {mc_target = Some(`ID_Var(`Var_Local,targ))}) ->
      let map = match lhs_o with
	| None -> map
	| Some lhs -> update_lhs MaybeNil map lhs
      in update targ NonNil map

    | Class(Some lhs, _, _) | Module(Some lhs,_,_)
    | MethodCall(Some lhs,_) | Yield(Some lhs,_)
    | Assign(lhs, _) -> update_lhs MaybeNil map lhs

    | _ -> map

  let init_formals args fact = 
    List . fold_left
      (fun acc param -> update (method_formal_name param) fact acc) empty args

end

module NilDataFlow = Dataflow.Forwards(NilAnalysis)


let transform targ node = 
  freparse ~env:node.lexical_locals "unless %a.nil? then %a end"
    format_expr targ format_stmt node


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

class safeNil inf = object(self)
  inherit default_visitor as super
  val facts = inf

  method visit_stmt node = match node.snode with
    | Method(mname, args, body) ->
      let in' , out' = NilDataFlow.fixpoint body in
      let me = {<facts = in'>} in
      let body' = visit_stmt (me:>cfg_visitor) body in
      ChangeTo(update_stmt node (Method(mname, args, body')))
	
    | MethodCall(_, {mc_target=Some (`ID_Var(`Var_Local, var) as targ)}) ->
      begin try let map = Hashtbl.find facts node in
		begin try match StrMap.find var map with
		  | NilAnalysis.MaybeNil -> ChangeTo(transform targ node)
		  | NilAnalysis.NonNil -> SkipChildren
		  with Not_found -> ChangeTo(transform targ node)
		end
	with Not_found -> assert false
      end

    | MethodCall(_, {mc_target=Some(#expr as targ)}) ->
      ChangeTo(transform targ node)
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

let testing_if_collapsing fname =
  let loader = File_loader.create File_loader.EmptyCfg [] in
  let s = File_loader.load_file loader fname in
  let () = compute_cfg s in
  let s' = visit_stmt (new propogateRedirectToReturnValue :> cfg_visitor) s in
  print_stmt stdout s'

let find_an_ear fname = 
  let loader = File_loader.create File_loader.EmptyCfg [] in
  let s = File_loader.load_file loader fname in
  let () = compute_cfg s in
  (* let () = compute_cfg_locals s in *)
  let s' = visit_stmt (new propogateRedirectToReturnValue :> cfg_visitor) s in
  let ears = findEAR s' in
  let are_ears = (List.length ears != 0) in
  if are_ears then
    print_ears ears
  else
    Printf.printf "No EARs found in %s.\n" (fname)

    

let nonnil fname =
  let loader = File_loader.create File_loader.EmptyCfg [] in
  let s = File_loader.load_file loader fname in
  let () = compute_cfg s in
  let () = compute_cfg_locals s in
  let ifacts, _ = NilDataFlow.fixpoint s in
  let s' = visit_stmt (new safeNil ifacts :> cfg_visitor) s in
  print_stmt stdout s'
  
  
  
let main fname =
  let loader = File_loader.create File_loader.EmptyCfg [] in
  let s = File_loader.load_file loader fname in
  let () = compute_cfg s in
  let () = compute_cfg_locals s in
  let ifacts, outfacts = EARFlow.fixpoint s in
  let print_fact stmt hasEAR = 
    if hasEAR then
      Printf.printf "T:%s" (string_of_cfg stmt)
    else
      Printf.printf "F:%s" (string_of_cfg stmt)
  in
  let print_facts tbl = Hashtbl.iter print_fact tbl in

  let () = print_facts ifacts in
  ()
  
  



let _ = 
  let fname = Sys.argv.(1) in
  (*testing_if_collapsing fname;  *)
  find_an_ear fname; 
  (*main fname; *)
  (*nonnil "ear_test.rb"; *)



(*
class foo_visitor = 
object(self)
  inherit default_visitor
  method visit_stmt s = match s.snode with
    | MethodCall(lhs_o, mc) ->
      begin match mc.mc_msg with
	| `ID_MethodName("redirect_to") ->
	  let new_mc = {mc with mc_msg = `ID_MethodName("bar")} in
	  let new_snode = MethodCall(lhs_o, new_mc) in
	  let new_stmt = mkstmt new_snode s.pos in
	  ChangeTo new_stmt
	| _ -> DoChildren
      end
    | _ -> DoChildren
end

let _ =
  let fname = Sys.argv.(1) in
  let ast = Parse_helper.parse_file fname in
  let cfg = Cfg_refactor.refactor_ast ast in
  let visitor = new foo_visitor in
  let new_cfg = visit_stmt visitor cfg in
      
  Cfg_printer.CodePrinter.print_stmt stdout new_cfg
*)
