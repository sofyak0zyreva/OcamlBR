(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree

module R : sig
  type 'a t

  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
    val fold_right : 'a list -> init:'b t -> f:('a -> 'b -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  ('a, 'b, 'c) Base.Map.t
      -> init:'d t
      -> f:('a -> 'b -> 'd -> 'd t)
      -> 'd t
  end

  val fresh : int t
  val run : 'a t -> ('a, error) Result.t
end = struct
  type 'a t =
    int -> int * ('a, error) Result.t (* a composition of result and state monad *)

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f state ->
    let last, res = m state in
    match res with
    | Result.Error x ->
      last, Error x (* if the first computation (m) fails, propagate the error *)
    | Result.Ok a ->
      f a last (* if it succeeds, pass the result (a) to the next computation (f) *)
  ;;

  (* wraps a value x into the monad without modifying the state. It returns the current state (last) and a successful result (Ok x) *)
  let return x last = last, Base.Result.return x

  (* creates a failed monadic computation, propagating the error (e) while leaving the state unchanged *)
  let fail e st = st, Base.Result.fail e
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Result.Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
  ;;

  (* syntatic sugar *)
  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  (* defines a monadic version of List.fold_left, can handle computations that may fail *)
  (* e.g. for solving multiple type constraints during inference *)
  module RList = struct
    let fold_left xs ~init ~f =
      Base.List.fold_left xs ~init ~f:(fun acc x ->
        let open Syntax in
        let* acc = acc in
        f acc x)
    ;;

    let fold_right xs ~init ~f =
      let open Syntax in
      Base.List.fold_right xs ~init ~f:(fun x acc ->
        let* acc = acc in
        f x acc)
    ;;
  end

  module RMap = struct
    let fold_left map ~init ~f =
      Base.Map.fold map ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end

  (* generates new state for representing a new fresh variable *)
  let fresh last = last + 1, Result.Ok last

  (* runs from initial state 0 and extracts the result from the monadic computation *)
  let run monad = snd (monad 0)
end

module Type = struct
  (* checks if a type variable v occurs anywhere within a given type; primarily used during type unification *)
  let rec occurs_in v = function
    | TVar b -> b = v
    | TPrim _ -> false
    | TArrow (l, r) -> occurs_in v l || occurs_in v r
    | TTuple (fst, snd, rest) ->
      occurs_in v fst || occurs_in v snd || List.exists (occurs_in v) rest
    | TList t -> occurs_in v t
    | TOption t -> occurs_in v t
    | TRecord _ -> false
  ;;

  (* computes the set of all type variables in a given type; primarily used to generalize types during type inference *)
  let type_vars =
    let rec helper acc = function
      | TVar b -> VarSet.add b acc
      | TPrim _ -> acc
      | TArrow (l, r) -> helper (helper acc l) r
      | TList t -> helper acc t
      | TTuple (fst, snd, rest) -> List.fold_left helper acc (fst :: snd :: rest)
      | TOption t -> helper acc t
      | TRecord _ -> acc
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val apply : t -> ty -> ty
  val singleton : type_var -> ty -> t R.t
  val unify : ty -> ty -> t R.t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
  val remove : t -> type_var -> t
  (* val pp_subst : Format.formatter -> t -> unit *)
end = struct
  open R
  open R.Syntax
  open Base

  type t = (type_var, ty, Int.comparator_witness) Map.t

  (* let pp_subst ppf sub =
     Base.Map.iteri sub ~f:(fun ~key ~data ->
     Stdlib.Format.fprintf ppf "[%d = %a] " key pp_ty data)
     ;; *)

  let empty = Map.empty (module Int)

  (* creates a substitution for variable [v] with type [ty] if no occurence is found *)
  let mapping v ty = if Type.occurs_in v ty then fail `Occurs_check else return (v, ty)

  let singleton k v =
    let* k, v = mapping k v in
    return (Base.Map.singleton (module Base.Int) k v)
  ;;

  let find subst v = Map.find subst v
  let remove subst v = Map.remove subst v

  (* applies a substitution to a type *)
  let apply subst =
    let rec helper = function
      | TVar v as ty ->
        (match find subst v with
         | Some ty' -> ty'
         | None -> ty)
      | TArrow (l, r) -> TArrow (helper l, helper r)
      | TTuple (f, s, rest) -> TTuple (helper f, helper s, List.map ~f:helper rest)
      | TList t -> TList (helper t)
      | TPrim _ as ty -> ty
      | TOption t -> TOption (helper t)
      | TRecord _ as ty -> ty
    in
    helper
  ;;

  (* attempts to unify two types [ty1] and [ty2], returning a substitution *)
  let rec unify ty1 ty2 =
    match ty1, ty2 with
    | TPrim l, TPrim r when String.equal l r -> return empty
    | TVar v1, TVar v2 when v1 = v2 -> return empty
    | TVar v, ty | ty, TVar v -> singleton v ty
    | TArrow (l1, r1), TArrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | TTuple (f1, s1, rest1), TTuple (f2, s2, rest2) ->
      let* rest_unified =
        match List.map2 rest1 rest2 ~f:(fun t1 t2 -> unify t1 t2) with
        | Unequal_lengths ->
          fail (`Unification_failed (TTuple (f1, s1, rest1), TTuple (f2, s2, rest2)))
        | Ok res -> return res
      in
      (* Format.printf "f1 %a \n" pp_ty f1;
         Format.printf "f2 %a \n" pp_ty f2; *)
      let* fst_unified = unify f1 f2 in
      (* Format.printf "(apply fst_unified s1) %a \n" pp_ty (apply fst_unified s1);
         Format.printf "(apply fst_unified s2) %a \n" pp_ty (apply fst_unified s2); *)
      let* snd_unified = unify (apply fst_unified s1) (apply fst_unified s2) in
      List.fold_left rest_unified ~init:(compose fst_unified snd_unified) ~f:(fun acc s ->
        let* s = s in
        let* acc = acc in
        compose acc s)
    | TList t1, TList t2 -> unify t1 t2
    | TOption t1, TOption t2 -> unify t1 t2
    | TRecord n1, TRecord n2 when String.equal n1 n2 -> return empty
    | _, _ -> fail (`Unification_failed (ty1, ty2))

  (* extends a substitution with a new mapping for variable [v] *)
  and extend v ty subst =
    match Map.find subst v with
    | None ->
      let ty = apply subst ty in
      let* new_subs = singleton v ty in
      let upd ~key ~data acc =
        let* acc = acc in
        let ty = apply new_subs data in
        return (Map.update acc key ~f:(function _ -> ty))
      in
      Map.fold subst ~init:(return new_subs) ~f:upd
    | Some existing_ty ->
      let* new_subs = unify ty existing_ty in
      compose subst new_subs

  and compose s1 s2 = RMap.fold_left s2 ~init:(return s1) ~f:extend

  let compose_all subs_list = RList.fold_left subs_list ~init:(return empty) ~f:compose
end

module VarSet = struct
  include VarSet

  let fold_left_m f acc set =
    fold
      (fun x acc ->
        let open R.Syntax in
        let* acc = acc in
        f acc x)
      acc
      set
  ;;
end

module Scheme = struct
  (* let occurs_in v = function
     | S (xs, t) -> (not (VarSet.mem v xs)) && Type.occurs_in v t
     ;; *)

  let free_vars = function
    | S (bs, t) -> VarSet.diff (Type.type_vars t) bs
  ;;

  let apply sub (S (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    S (names, Subst.apply s2 ty)
  ;;

  (* let pp = pp_scheme *)
end

module KeyComparator = struct
  module T = struct
    type t = string * type_var

    let compare (_, int1) (_, int2) = Int.compare int1 int2

    let sexp_of_t (s, i) =
      Base.Sexp.List [ Base.Sexp.Atom s; Base.Sexp.Atom (Int.to_string i) ]
    ;;
  end

  include T
  include Base.Comparator.Make (T)
end

module RecordEnv = struct
  open R
  open R.Syntax

  type t_ordered =
    (string * type_var, (string * ty) list, KeyComparator.comparator_witness) Base.Map.t

  let empty_ordered = Base.Map.empty (module KeyComparator)

  type t_unordered =
    (string, (string * ty) list, Base.String.comparator_witness) Base.Map.t

  type t = t_unordered * t_ordered

  let empty_unordered = Base.Map.empty (module Base.String)
  let empty : t = empty_unordered, empty_ordered

  let env_unordered = function
    | env, _ -> env
  ;;

  let env_ordered = function
    | _, env -> env
  ;;

  let find_record env name = Base.Map.find (env_unordered env) name
  let mem_record env name = Base.Map.mem (env_unordered env) name

  (* let pp_record_env (env : t * t_ordered) : unit =
     let env = env_ordered env in
     let pp_field (field_name, ty) = Format.printf "  %s : %a;" field_name pp_ty ty in
     let pp_record ((record_name, ty_var), fields) =
     Format.printf "Record: %s %d." record_name ty_var;
     Base.List.iter fields ~f:(fun field -> pp_field field);
     Format.printf "\n"
     in
     Base.Map.iteri env ~f:(fun ~key:record_name ~data:fields ->
     pp_record (record_name, fields))
     ;; *)

  (* ------------------- validation of new types : details below ------------------------- *)
  (* aux function for failing cases with duplicate labels: e.g. type t = {t: bool; t: int} *)
  module StringSet = Stdlib.Set.Make (String)

  let validate_unique_labels fields =
    let rec aux seen = function
      | [] -> return ()
      | (label, _) :: rest ->
        if StringSet.mem label seen
        then fail (`Duplicate_field_labels label)
        else aux (StringSet.add label seen) rest
    in
    aux StringSet.empty fields
  ;;

  (* check that field types are defined *)
  let rec validate_field_type env ty =
    match ty with
    | TPrim _ -> return ()
    | TVar _ -> return ()
    | TArrow (t1, t2) ->
      let* _ = validate_field_type env t1 in
      validate_field_type env t2
    | TTuple (t1, t2, ts) ->
      let* _ = validate_field_type env t1 in
      let* _ = validate_field_type env t2 in
      RList.fold_left ts ~init:(return ()) ~f:(fun _ t -> validate_field_type env t)
    | TList t | TOption t -> validate_field_type env t
    | TRecord name ->
      if mem_record env name then return () else fail (`Undefined_type name)
  ;;

  let validate_record_fields env fields =
    RList.fold_left fields ~init:(return ()) ~f:(fun _ (_label, ty) ->
      validate_field_type env ty)
  ;;

  (* check that there is no multiple definition of the type name (sig is allowed to be the same) *)
  let validate_type_name env name =
    if mem_record env name then fail (`Multiple_definition_of_type name) else return ()
  ;;

  (*---------------------------------------------------------------------------------*)

  let add_record (env : t) name fields : t R.t =
    let* _ = validate_type_name env name in
    let* _ = validate_unique_labels fields in
    let* _ = validate_record_fields env fields in
    let env, env_ordered =
      match env with
      | env, env_ordered -> env, env_ordered
    in
    let* ty_var = fresh in
    return
      ( Base.Map.set env ~key:name ~data:fields
      , Base.Map.set env_ordered ~key:(name, ty_var) ~data:fields )
  ;;

  let all_records_ordered env = Base.Map.to_alist (env_ordered env)

  (* retrieve a field's type from a record by name and label *)
  let infer_field_with_name env record_name field_name =
    match find_record env record_name with
    | None -> fail (`Undefined_type record_name)
    | Some fields ->
      (match Base.List.Assoc.find ~equal:String.equal fields field_name with
       | Some field -> return (Subst.empty, field)
       | None -> fail (`Missing_label (record_name, field_name)))
  ;;

  let infer_field_name env s t field_name =
    let records = all_records_ordered env in
    let f1 acc ((name, _), fields) =
      List.find_map
        (fun (lbl, ty) -> if String.equal lbl field_name then Some (ty, name) else acc)
        fields
    in
    match List.fold_left f1 None records with
    | Some (ty, name) ->
      let* s2 = Subst.unify t (Subst.apply s (TRecord name)) in
      let* s_final = Subst.compose s s2 in
      return (s_final, ty)
    | None -> fail (`Missing_label ("record", field_name))
  ;;

  let infer_field_access env s t field_name =
    match t with
    | TRecord name -> infer_field_with_name env name field_name
    | TVar _ -> infer_field_name env s t field_name
    | _ -> fail (`Unification_failed (t, TRecord "record type"))
  ;;

  let compare lst1 lst2 =
    if List.length lst1 = List.length lst2
       && List.for_all2 (fun (lbl1, _) (lbl2, _, _) -> lbl1 = lbl2) lst1 lst2
    then
      List.fold_left2
        (fun acc (_, ty1) (_, sub, ty2) ->
          let* _ = acc in
          let* _ = Subst.unify ty2 (Subst.apply sub ty1) in
          acc)
        (return true)
        lst1
        lst2
    else return false
  ;;

  (* matching fields' signatures; if more than one type with such sig is found, the latter is given *)
  let infer_record env inferred_record_fields =
    (* Format.printf " meeeow  \n"; *)
    let records = all_records_ordered env in
    let f1 acc ((name, _), fields) =
      (* Format.printf " meeeow %s \n" name; *)
      let* satisfied = compare fields inferred_record_fields in
      if satisfied then return (Some name) else acc
    in
    let* name = List.fold_left f1 (return None) records in
    match name with
    | Some name -> return (Subst.empty, TRecord name)
    | None -> fail (`Undefined_type "no such type")
  ;;

  (* if some type is expected, try matching it against one of the existing *)
  let infer_record_with_name env record_name inferred_record_fields =
    match find_record env record_name with
    | None -> fail (`Undefined_type record_name)
    | Some fields ->
      let* satisfied = compare fields inferred_record_fields in
      if satisfied
      then return (TRecord record_name)
      else fail (`Undefined_type "no such type")
  ;;
end

module TypeEnv = struct
  open Base

  type t = (string, scheme, String.comparator_witness) Map.t

  let empty = Map.empty (module String)

  let free_vars env =
    Map.fold env ~init:VarSet.empty ~f:(fun ~key:_ ~data:s acc ->
      VarSet.union acc (Scheme.free_vars s))
  ;;

  let apply s env = Map.map env ~f:(Scheme.apply s)
  let extend key s env = Map.update env key ~f:(fun _ -> s)

  let extend_many list env =
    List.fold list ~init:env ~f:(fun env (key, v) -> extend key v env)
  ;;

  let find env key = Map.find env key

  let merge_envs subst acc_env env_pat =
    let acc_env = apply subst acc_env in
    let env_pat = apply subst env_pat in
    Map.fold env_pat ~init:acc_env ~f:(fun ~key ~data acc_env -> extend key data acc_env)
  ;;

  let remove = Map.remove

  let find_type_exn env key =
    match Map.find_exn env key with
    | S (_, typ) -> typ
  ;;

  let pp ppf env =
    Stdlib.Format.fprintf ppf "{| ";
    Map.iteri env ~f:(fun ~key:name ~data:scheme ->
      Stdlib.Format.fprintf ppf "%s -> %a; " name pp_scheme scheme);
    Stdlib.Format.fprintf ppf "|}"
  ;;
end

module Infer = struct
  open R
  open R.Syntax

  let unify = Subst.unify
  let fresh_var = fresh >>| fun n -> TVar n

  let instantiate : scheme -> ty R.t =
    fun (S (bs, t)) ->
    VarSet.fold_left_m
      (fun typ name ->
        let* f1 = fresh_var in
        let* s = Subst.singleton name f1 in
        return (Subst.apply s typ))
      bs
      (return t)
  ;;

  let generalize (env : TypeEnv.t) (ty : ty) : scheme =
    let free = VarSet.diff (Type.type_vars ty) (TypeEnv.free_vars env) in
    S (free, ty)
  ;;

  let lookup_env e xs =
    match TypeEnv.find xs e with
    | None -> fail (`Undefined_variable e)
    | Some scheme ->
      let* ans = instantiate scheme in
      return (Subst.empty, ans)
  ;;

  let fst_arrow = function
    | TArrow (arg, _) -> arg
    | ty -> failwith (Format.asprintf "Expected function type, got: %a" pp_ty ty)
  ;;

  let snd_arrow = function
    | TArrow (_, ret) -> ret
    | ty -> failwith (Format.asprintf "Expected function type, got: %a" pp_ty ty)
  ;;

  let string_of_id (Ast.Id name) = name

  let infer_const = function
    | Ast.Int _ -> tprim_int
    | Ast.Bool _ -> tprim_bool
    | Ast.String _ -> tprim_string
    | Ast.Unit -> tprim_unit
  ;;

  let rec infer_pattern env = function
    | Ast.PVar id ->
      let var_name = string_of_id id in
      let* fresh = fresh_var in
      let extended_env = TypeEnv.extend var_name (S (VarSet.empty, fresh)) env in
      return (Subst.empty, fresh, extended_env)
    | Ast.PAny ->
      let* fresh = fresh_var in
      return (Subst.empty, fresh, env)
    | Ast.PConst c ->
      let fresh = infer_const c in
      return (Subst.empty, fresh, env)
    | Ast.PTuple (p1, p2, pl) ->
      let* sub1, typ1, env1 = infer_pattern env p1 in
      let* sub2, typ2, env2 = infer_pattern (TypeEnv.apply sub1 env1) p2 in
      let f1 (pat : Ast.pattern) (sub_prev, l, env) =
        let* sub_cur, arg, env = infer_pattern env pat in
        let* sub = Subst.compose sub_prev sub_cur in
        return (sub, arg :: l, env)
      in
      let* sub, arg, env = RList.fold_right pl ~init:(return (sub2, [], env2)) ~f:f1 in
      return (sub, TTuple (typ1, typ2, arg), env)
    | Ast.PList pats ->
      let* fresh_el_type = fresh_var in
      let f1 (sub_acc, env_acc) pat =
        let* sub_cur, el_type, env_cur = infer_pattern env_acc pat in
        let* unified_sub = Subst.compose sub_acc sub_cur in
        let* final_sub = Subst.unify (Subst.apply sub_cur fresh_el_type) el_type in
        let combined_sub = Subst.compose unified_sub final_sub in
        let* combined_sub = combined_sub in
        return (combined_sub, TypeEnv.apply final_sub env_cur)
      in
      let* final_sub, final_env =
        RList.fold_left pats ~init:(return (Subst.empty, env)) ~f:f1
      in
      return (final_sub, TList (Subst.apply final_sub fresh_el_type), final_env)
    | Ast.PCons (p1, p2) ->
      let* sub1, typ1, env1 = infer_pattern env p1 in
      let* _, typ2, env2 = infer_pattern (TypeEnv.apply sub1 env1) p2 in
      let* subst = Subst.unify typ2 (TList typ1) in
      let env = TypeEnv.apply subst env2 in
      return (subst, Subst.apply subst typ2, env)
    | Ast.POption None ->
      let* fresh = fresh_var in
      return (Subst.empty, TOption fresh, env)
    | Ast.POption (Some p) ->
      let* sub, typ, env = infer_pattern env p in
      return (sub, TOption typ, env)
  ;;

  let infer_ty_pattern env : Ast.ty_pattern -> (Subst.t * ty * TypeEnv.t) t = function
    | pat, Some typ ->
      let* s, t, env = infer_pattern env pat in
      let typ = Subst.apply s typ in
      let* subst = unify typ t in
      return (subst, Subst.apply subst typ, TypeEnv.apply subst env)
    | pat, None -> infer_pattern env pat
  ;;

  let validate_let_rec_lhs ty_pat =
    match ty_pat with
    | Ast.PVar _, _ -> return ty_pat
    | _ -> fail (`Ill_left_hand_side ": only variables are allowed")
  ;;

  let validate_let_rec_rhs expr =
    match expr with
    | Ast.Efun _ -> return expr
    | _ -> fail (`Ill_right_hand_side "of let rec")
  ;;

  let rec infer env record_env (expr : Ast.expr) : (Subst.t * ty) R.t =
    match expr with
    | Evar (Id x) -> lookup_env x env
    | Econst (Int _) -> return (Subst.empty, tprim_int)
    | Econst (Bool _) -> return (Subst.empty, tprim_bool)
    | Econst (String _) -> return (Subst.empty, tprim_string)
    | Econst Unit -> return (Subst.empty, tprim_unit)
    | Ebin_op (op, e1, e2) ->
      let* s1, t1 = infer env record_env e1 in
      let* s2, t2 = infer (TypeEnv.apply s1 env) record_env e2 in
      let* e1t, e2t, et =
        match op with
        | Mult | Div | Add | Sub -> return (tprim_int, tprim_int, tprim_int)
        | Eq | Neq | Lt | Lte | Gt | Gte ->
          let* fresh = fresh_var in
          return (fresh, fresh, tprim_bool)
        | And | Or -> return (tprim_bool, tprim_bool, tprim_bool)
        | Cons ->
          let* fresh = fresh_var in
          return (fresh, TList fresh, TList fresh)
      in
      let* sub3 = Subst.unify (Subst.apply s2 t1) e1t in
      let* sub4 = Subst.unify (Subst.apply sub3 t2) e2t in
      let* sub = Subst.compose_all [ s1; s2; sub3; sub4 ] in
      return (sub, Subst.apply sub et)
    | Eun_op (op, e) ->
      let* s, t = infer env record_env e in
      let* op_type =
        match op with
        | Negative | Positive -> return (tprim_int @-> tprim_int)
        | Not -> return (tprim_bool @-> tprim_bool)
      in
      let* s2 = unify t (fst_arrow op_type) in
      let* s_final = Subst.compose_all [ s2; s ] in
      return (s_final, Subst.apply s_final (snd_arrow op_type))
    | Eif_then_else (c, th, Some el) ->
      let* s1, t1 = infer env record_env c in
      let* s2, t2 = infer (TypeEnv.apply s1 env) record_env th in
      let* s3, t3 = infer (TypeEnv.apply s2 env) record_env el in
      let* s4 = unify t1 tprim_bool in
      let* s5 = unify t2 t3 in
      let* final_subst = Subst.compose_all [ s5; s4; s3; s2; s1 ] in
      return (final_subst, Subst.apply final_subst t2)
    | Eif_then_else (c, th, None) ->
      let* s1, t1 = infer env record_env c in
      let* s2, t2 = infer (TypeEnv.apply s1 env) record_env th in
      let t3 = tprim_unit in
      let* s4 = unify t1 tprim_bool in
      let* s5 = Subst.unify t2 t3 in
      let* final_subst = Subst.compose_all [ s5; s4; s2; s1 ] in
      return (final_subst, Subst.apply final_subst t2)
    | Elet (Non_recursive, Evalue_binding ((PVar (Id x), t_opt), e1), _, e2) ->
      let* s1, t1 = infer env record_env e1 in
      let* env2 =
        match t_opt with
        | Some expected_type ->
          let expected_type = Subst.apply s1 expected_type in
          let* sub1 = Subst.unify t1 expected_type in
          return (TypeEnv.apply sub1 env)
        | None -> return (TypeEnv.apply s1 env)
      in
      let t_gen = generalize env2 t1 in
      let env3 = TypeEnv.extend x t_gen env in
      let* s2, t2 = infer (TypeEnv.apply s1 env3) record_env e2 in
      let* final_subst = Subst.compose s1 s2 in
      return (final_subst, t2)
    | Elet (Non_recursive, Evalue_binding ((pattern, t_opt), e1), bindings, e2) ->
      let* s1, t1 = infer env record_env e1 in
      let* s2, t_pat, env1 = infer_ty_pattern env (pattern, t_opt) in
      let* subst1 = Subst.compose s1 s2 in
      let* unified_subst = unify (Subst.apply subst1 t_pat) t1 in
      let initial_env = TypeEnv.apply unified_subst env1 in
      let* extended_env =
        List.fold_left
          (fun acc_env vb ->
            let* acc_env = acc_env in
            match vb with
            | Ast.Evalue_binding ((p, opt_ty), expr) ->
              let* s_bind, t_bind = infer acc_env record_env expr in
              let* s_pat, t_pat, env_pat = infer_ty_pattern acc_env (p, opt_ty) in
              let* combined_subst = Subst.compose s_bind s_pat in
              let* final_subst = unify (Subst.apply combined_subst t_pat) t_bind in
              let updated_env = TypeEnv.merge_envs final_subst acc_env env_pat in
              return updated_env)
          (return initial_env)
          bindings
      in
      let* s3, t2 = infer extended_env record_env e2 in
      let* full_subst = Subst.compose_all [ s3; unified_subst; subst1 ] in
      return (full_subst, t2)
    | Elet (Recursive, Evalue_binding ((PVar (Id x), t_opt), e1), [], e2) ->
      let* e1 = validate_let_rec_rhs e1 in
      let* tv = fresh_var in
      let env2 = TypeEnv.extend x (S (VarSet.empty, tv)) env in
      let* s1, t1 = infer env2 record_env e1 in
      let* s2 = unify (Subst.apply s1 tv) t1 in
      let* s_final = Subst.compose s1 s2 in
      let env3 = TypeEnv.apply s_final env in
      let* env4 =
        match t_opt with
        | Some expected_type ->
          let expected_type = Subst.apply s1 expected_type in
          let* sub1 = Subst.unify t1 expected_type in
          return (TypeEnv.apply sub1 env3)
        | None -> return (TypeEnv.apply s1 env3)
      in
      let t_gen = generalize env4 (Subst.apply s_final tv) in
      let* s3, t2 = infer (TypeEnv.extend x t_gen env4) record_env e2 in
      let* s_final = Subst.compose s_final s3 in
      return (s_final, t2)
    | Elet (Recursive, value_binding, value_bindings, e2) ->
      let* env_ext, s_acc =
        List.fold_left
          (fun acc_env (Ast.Evalue_binding (ty_pattern, expr)) ->
            let* expr = validate_let_rec_rhs expr in
            let* ty_pattern = validate_let_rec_lhs ty_pattern in
            let* env_acc, _ = acc_env in
            let* s_expr, t_expr = infer env_acc record_env expr in
            let* s_pat, t_pat, env_pat = infer_ty_pattern env_acc ty_pattern in
            let* subst = Subst.compose s_expr s_pat in
            let* unified_subst = unify t_expr t_pat in
            let* combined_subst = Subst.compose subst unified_subst in
            let extended_env = TypeEnv.apply combined_subst env_pat in
            return (extended_env, combined_subst))
          (return (env, Subst.empty))
          (value_binding :: value_bindings)
      in
      let* s2, t2 = infer env_ext record_env e2 in
      let* final_subst = Subst.compose s_acc s2 in
      return (final_subst, t2)
    | Efun (ty_pattern, ty_pattern_list, body) ->
      let* env, pat_types =
        RList.fold_left
          (ty_pattern :: ty_pattern_list)
          ~init:(return (env, []))
          ~f:(fun (env, pat_types) pat ->
            let* _, typ, new_env = infer_ty_pattern env pat in
            return (new_env, typ :: pat_types))
      in
      let* s_body, t_body = infer env record_env body in
      let arrow_type =
        List.fold_right
          (fun pat_type acc -> TArrow (Subst.apply s_body pat_type, acc))
          (List.rev pat_types)
          t_body
      in
      return (s_body, arrow_type)
    | Efun_application (e1, e2) ->
      let* s1, t1 = infer env record_env e1 in
      let* s2, t2 = infer (TypeEnv.apply s1 env) record_env e2 in
      let* tv = fresh_var in
      let* s3 = unify (Subst.apply s2 t1) (TArrow (t2, tv)) in
      let* s_final = Subst.compose_all [ s3; s2; s1 ] in
      return (s_final, Subst.apply s_final tv)
    | Eoption (Some e) ->
      let* s, t = infer env record_env e in
      return (s, TOption t)
    | Eoption None ->
      let* tv = fresh_var in
      return (Subst.empty, TOption tv)
    | Ematch (e, c, cl) ->
      let* sub1, t1 = infer env record_env e in
      let env = TypeEnv.apply sub1 env in
      let* tv = fresh_var in
      infer_match env record_env (c :: cl) sub1 t1 tv ~with_expr:true
    | Efunction (c, cl) ->
      let* t1 = fresh_var in
      let* tv = fresh_var in
      infer_match env record_env (c :: cl) Subst.empty t1 tv ~with_expr:false
    | Etuple (e1, e2, es) ->
      let* s1, t1 = infer env record_env e1 in
      let* s2, t2 = infer (TypeEnv.apply s1 env) record_env e2 in
      let infer_tuple_elements env es =
        let rec aux env = function
          | [] -> return ([], [])
          | e :: es' ->
            let* s, t = infer env record_env e in
            let* s', ts = aux (TypeEnv.apply s env) es' in
            return (s' @ [ s ], t :: ts)
        in
        aux env es
      in
      let* s3, ts = infer_tuple_elements (TypeEnv.apply s2 env) es in
      let* s_final = Subst.compose_all (s3 @ [ s2; s1 ]) in
      return (s_final, TTuple (t1, t2, ts))
    | Elist es ->
      (match es with
       | [] ->
         let* fresh = fresh_var in
         return (Subst.empty, tlist fresh)
       | _ :: _ ->
         let infer_list_elements env es =
           let rec aux env = function
             | [] -> return ([], [])
             | e :: es' ->
               let* s, t = infer env record_env e in
               let* s', ts = aux (TypeEnv.apply s env) es' in
               return (s' @ [ s ], t :: ts)
           in
           aux env es
         in
         let* s, ts = infer_list_elements env es in
         let* s_final = Subst.compose_all s in
         return (s_final, TList (List.hd ts)))
    | Econstraint (e, t) ->
      let* s1, t1 = infer env record_env e in
      let* s2 = unify t1 (Subst.apply s1 t) in
      let* s_final = Subst.compose s1 s2 in
      return (s_final, Subst.apply s2 t1)
    | Erecord (record_field, record_fields) ->
      let* inferred_record_fields =
        RList.fold_right
          (record_field :: record_fields)
          ~init:(return [])
          ~f:(fun (Ast.Erecord_field (Label name, expr)) acc ->
            let* s, t = infer env record_env expr in
            return ((name, s, t) :: acc))
      in
      let* s, t = RecordEnv.infer_record record_env inferred_record_fields in
      return (s, t)
    | Efield_access (e, Label name) ->
      let* s1, t1 = infer env record_env e in
      let* s, t = RecordEnv.infer_field_access record_env s1 t1 name in
      return (s, t)

  and infer_match env record_env cases inferred_sub inferred_t ty_var ~with_expr =
    let* s, final_t =
      Base.List.fold
        cases
        ~init:(return (inferred_sub, ty_var))
        ~f:(fun acc (Ecase (pat, expr)) ->
          let* s1, t = acc in
          let* env, s2 =
            if with_expr
            then
              let* _, pat_t, env = infer_pattern env pat in
              let* subst = unify pat_t inferred_t in
              let env = TypeEnv.apply subst env in
              let name =
                match pat with
                | PVar (Id name) | POption (Some (PVar (Id name))) -> Some name
                | _ -> None
              in
              let env =
                match name with
                | Some name ->
                  let found_t = TypeEnv.find_type_exn env name in
                  let env = TypeEnv.remove env name in
                  let t_gen = generalize env found_t in
                  TypeEnv.extend name t_gen env
                | None -> env
              in
              return (env, subst)
            else
              let* _, pat, env = infer_pattern env pat in
              let* s2 = unify inferred_t pat in
              return (env, s2)
          in
          let* s3 = Subst.compose s1 s2 in
          let* s4, t4 = infer (TypeEnv.apply s3 env) record_env expr in
          let* s5 = unify t t4 in
          let* subst = Subst.compose_all [ s3; s4; s5 ] in
          return (subst, Subst.apply subst t))
    in
    let final_t = if with_expr then final_t else Subst.apply s inferred_t @-> final_t in
    return (s, final_t)

  and infer_typed_record env record_env expected_type = function
    | Ast.Erecord (record_field, record_fields) ->
      let* inferred_record_fields =
        RList.fold_right
          (record_field :: record_fields)
          ~init:(return [])
          ~f:(fun (Ast.Erecord_field (Label name, expr)) acc ->
            let* s, t = infer env record_env expr in
            return ((name, s, t) :: acc))
      in
      let* t =
        RecordEnv.infer_record_with_name record_env expected_type inferred_record_fields
      in
      return (Subst.empty, t)
    | _ -> return (Subst.empty, TRecord expected_type)
  ;;

  let infer_ty_opt env record_env t_opt expr =
    match t_opt with
    | Some expected_type ->
      (match expected_type with
       | TRecord t -> infer_typed_record env record_env t expr
       | _ -> infer env record_env expr)
    | None -> infer env record_env expr
  ;;

  let w expr = Result.map snd (run (infer TypeEnv.empty RecordEnv.empty expr))

  let infer_structure_item env record_env = function
    | Ast.SEval expr ->
      let* subst, _ = infer env record_env expr in
      let updated_env = TypeEnv.apply subst env in
      return (subst, updated_env, record_env)
    | Ast.SValue (Recursive, Evalue_binding ((PVar (Id x), t_opt), expr), []) ->
      let* expr = validate_let_rec_rhs expr in
      let* tv = fresh_var in
      let env = TypeEnv.extend x (S (VarSet.empty, tv)) env in
      let* subst, inferred_ty = infer env record_env expr in
      let* subst2 = unify (Subst.apply subst tv) inferred_ty in
      let* composed_subst = Subst.compose subst subst2 in
      let* env2 =
        match t_opt with
        | Some expected_type ->
          let expected_type = Subst.apply composed_subst expected_type in
          let* sub1 = Subst.unify inferred_ty expected_type in
          return (TypeEnv.apply sub1 env)
        | None -> return (TypeEnv.apply composed_subst env)
      in
      let generalized_ty = generalize env2 (Subst.apply composed_subst inferred_ty) in
      let env = TypeEnv.extend x generalized_ty env2 in
      return (composed_subst, env, record_env)
    | Ast.SValue (Recursive, value_binding, value_bindings) ->
      let all_bindings = value_binding :: value_bindings in
      let* env_with_placeholders =
        List.fold_left
          (fun acc_env (Ast.Evalue_binding (ty_pattern, _)) ->
            let* ty_pattern = validate_let_rec_lhs ty_pattern in
            let* env_acc = acc_env in
            let* s_pat, _, env_pat = infer_ty_pattern env_acc ty_pattern in
            let extended_env = TypeEnv.apply s_pat env_pat in
            return extended_env)
          (return env)
          all_bindings
      in
      let* env_ext, s_acc =
        List.fold_left
          (fun acc_env (Ast.Evalue_binding (ty_pattern, expr)) ->
            let* expr = validate_let_rec_rhs expr in
            let* env_acc, _ = acc_env in
            let* s_expr, t_expr = infer env_acc record_env expr in
            let* s_pat, t_pat, env_pat = infer_ty_pattern env_acc ty_pattern in
            let* subst = Subst.compose s_expr s_pat in
            let* unified_subst = unify t_expr t_pat in
            let* combined_subst = Subst.compose subst unified_subst in
            let extended_env = TypeEnv.apply combined_subst env_pat in
            return (extended_env, combined_subst))
          (return (env_with_placeholders, Subst.empty))
          all_bindings
      in
      return (s_acc, env_ext, record_env)
    | Ast.SValue (Non_recursive, Evalue_binding ((PVar (Id x), t_opt), expr), _) ->
      let* subst, inferred_ty = infer_ty_opt env record_env t_opt expr in
      let* env2 =
        match t_opt with
        | Some expected_type ->
          let expected_type = Subst.apply subst expected_type in
          let* sub1 = Subst.unify inferred_ty expected_type in
          return (TypeEnv.apply sub1 env)
        | None -> return (TypeEnv.apply subst env)
      in
      let generalized_ty = generalize env2 inferred_ty in
      let env = TypeEnv.extend x generalized_ty (TypeEnv.apply subst env) in
      return (subst, env, record_env)
    | Ast.SValue (Non_recursive, Evalue_binding ((pattern, t_opt), expr), _) ->
      let* subst_expr, inferred_ty = infer_ty_opt env record_env t_opt expr in
      let* subst_pat, t_pat, env_pat = infer_ty_pattern env (pattern, t_opt) in
      let* combined_subst =
        let* composed = Subst.compose subst_expr subst_pat in
        return composed
      in
      let* unified_subst = unify (Subst.apply combined_subst t_pat) inferred_ty in
      let updated_env = TypeEnv.apply unified_subst env_pat in
      let* final_subst = Subst.compose unified_subst combined_subst in
      return (final_subst, updated_env, record_env)
    | Ast.SType (record, field_decl, field_decls) ->
      let fields =
        List.map
          (fun (Ast.Sfield_decl (Label name, t)) -> name, t)
          (field_decl :: field_decls)
      in
      let* record_env = RecordEnv.add_record record_env record fields in
      return (Subst.empty, env, record_env)
  ;;

  let infer_structure env record_env structure =
    let rec process_structure env record_env subst = function
      | [] -> return (subst, env)
      | item :: rest ->
        let* subst', env', record_env' = infer_structure_item env record_env item in
        let* composed_subst = Subst.compose subst subst' in
        process_structure env' record_env' composed_subst rest
    in
    process_structure env record_env Subst.empty structure
  ;;

  let env =
    TypeEnv.extend_many
      [ "print_int", S (VarSet.empty, TArrow (tprim_int, tprim_unit))
      ; "print_endline", S (VarSet.empty, TArrow (tprim_string, tprim_unit))
      ]
      TypeEnv.empty
  ;;

  let record_env = RecordEnv.empty
  let infer_program str = Result.map snd (run (infer_structure env record_env str))
end
