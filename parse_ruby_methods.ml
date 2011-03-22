open Cfg
open Visitor
open Utils
open Set

type ruby_class = string

type ruby_access_control = 
  | Public
  | Protected
  | Private

let string_from_ruby_ac ac = match ac with
  | Public -> "public"
  | Protected -> "protected"
  | Private -> "private"

let ruby_ac_from_string str = match str with
  | "public" -> Public
  | "protected" -> Protected
  | "private" -> Private
  | _ -> raise (Invalid_argument "str")

type ruby_method_name = string

type ruby_method = 
    {
      ruby_class : ruby_class;
      access: ruby_access_control;
      name: ruby_method_name;
    }

let full_method_name r1 = Printf.sprintf "%s:%s" r1.ruby_class r1.name

module RubyMethod = struct
  type t = ruby_method
  let compare x y = Pervasives.compare (full_method_name x) (full_method_name y)
end

module RubyMethodSet = Set.Make(RubyMethod)

class find_all_methods = object(self)
  inherit default_visitor as super

  val mutable methods = RubyMethodSet.empty
  val mutable current_protection = Public
  val mutable current_class = "Object"

  method visit_stmt node = match node.snode with

    | Class(_, MetaClass(`ID_Var(_, name)), _) 
    | Class(_, NominalClass(`ID_Var(_, name), _), _) ->
      let prev_class = current_class in 
      let prev_protection = current_protection in
      begin
	current_class <- name;
	current_protection <- Public;
	ChangeDoChildrenPost(node, fun n -> 
	    begin 
	      current_class <- prev_class;
	      current_protection <- prev_protection;
	      n
	    end)
      end

    | Method(Instance_Method(`ID_MethodName(name)), _, _)
    | Method(Singleton_Method(_, `ID_MethodName(name)), _, _) -> 
      begin
	methods <- RubyMethodSet.add { ruby_class = current_class; access = current_protection; name = name } methods;
	SkipChildren
      end


    (* a method call with no arguments changes the current_protection *)
    | MethodCall(_, {mc_msg = `ID_MethodName(name); mc_args = []}) -> 
      begin
	try current_protection <- ruby_ac_from_string name; 	  
	    SkipChildren
	with _ -> SkipChildren
      end

    (* a method call with arguments (either strings or symbols) makes those methods have that access status *)
    | MethodCall(_, {mc_msg = `ID_MethodName(name); mc_args = args}) -> 
      begin
	match name with
	  | "public"
	  | "protected"
	  | "private"
	    -> 
	    let rec strings_from_args args = match args with
	      | `Lit_String(str) :: xs
	      | `Lit_Atom(str) :: xs -> [str] @ strings_from_args xs
	      | x :: xs -> strings_from_args args
	      | _ -> []
	    in
	    let methods_to_change = strings_from_args args in
	    let new_protection = ruby_ac_from_string name in
	    let update_method_new_protection method_name = 
	      let old_method = {ruby_class = current_class; access = current_protection; name = method_name } in
	      let methods_removed = RubyMethodSet.remove old_method methods in
	      let new_method = {ruby_class = current_class; access = new_protection; name = method_name } in
	      methods <- RubyMethodSet.add new_method methods_removed
	    in
	    List.iter update_method_new_protection methods_to_change

	  | _ -> ()
      end;
      SkipChildren

    | _ ->
      super#visit_stmt node

  method methods = methods

end

let show_methods file = 
  let loader = File_loader.create File_loader.EmptyCfg [] in
  let cfg = File_loader.load_file loader file in
  let () = compute_cfg cfg in
  let methods_visitor = new find_all_methods in
  let _ = visit_stmt (methods_visitor :> cfg_visitor) cfg in
  RubyMethodSet.iter (fun meth -> Printf.printf "%s::%s : %s\n" meth.ruby_class meth.name (string_from_ruby_ac meth.access)) methods_visitor#methods
