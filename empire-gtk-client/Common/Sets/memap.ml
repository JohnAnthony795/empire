module MS = Meset

module type M =
sig
  type key
  type 'a map
  module Keyset: Meset.S with type elt = key
                                
  val empty: tos:('a -> string) -> mergev:('a -> 'a -> 'a) -> 'a map
  val size: 'a map -> int
  val keys: 'a map -> Keyset.set
  val update: ?exist:bool -> ?force:bool -> ?default:(key -> 'a) -> ?f:(key -> 'a -> 'a) -> 'a map -> key -> 'a map
  val update_k: ?exist:bool -> ?force:bool -> ?default:(key -> 'a) -> ?f:(key -> 'a -> 'a) -> 'a map -> key -> 'a map * key
  val add: ?exist:bool -> ?force:bool -> 'a map -> key -> 'a -> 'a map
  val add_k: ?exist:bool -> ?force:bool -> 'a map -> key -> 'a -> 'a map * key                                                   
  val find_result2s: 'a map -> (key * 'a) MS.mem_result -> string
  val find: no:bool -> 'a map -> key -> (key * 'a) MS.mem_result
  val remove: 'a map -> key -> 'a map
  val fold: ?comparekey:(key -> 'b) -> ?compare:(key -> key -> int) -> 'a map -> 'c -> (key -> 'a -> 'c -> 'c) -> 'c
  val map2s: ?comparekey:(key -> 'b) -> ?compare:(key -> key -> int) -> ?title:string -> 'a map -> string
  val mmap: 'a map -> ('b -> 'b -> 'b) -> ('a -> 'b) -> ('b -> string) -> 'b map
end

module Make = functor (Elt: MS.ELEMENT) ->
struct
  type key = Elt.elt

  module S = MS.Make(Elt)
  module Keyset = S

  module M = Map.Make (struct type t = Elt.id  let compare = Pervasives.compare end)

  (* The map uses the first id of the key. 
   * Invariant: { the first id of keys } == { keys of idmap }  *)
  type 'a map = {
    keys: S.set ;    
    v2s: 'a -> string ;
    mergev: 'a -> 'a -> 'a ;
    idmap: 'a M.t }

  let keys mp = mp.keys
  
  let raiserror e msg = raise (MS.Error (e, msg))
  
  let empty ~tos ~mergev =
    { keys = S.empty ;
      v2s = tos ;
      mergev = (fun a b -> try mergev a b with e -> raiserror (MS.User e) ("mergev on " ^ tos a ^ " and " ^ tos b));
      idmap = M.empty }

  let get_firstid key =
    match Elt.get_ids key with
    | [] -> raiserror MS.No_identifier (Elt.elt2s key)
    | id :: _ -> id
    | exception e -> raiserror (MS.User e) ("get_ids on " ^ Elt.elt2s key)

  let size mp =
    let res = S.size mp.keys in
    assert (res = M.cardinal mp.idmap) ;
    res

  let update_k ?exist ?force ?default ?(f=fun _ v -> v) mp key =

    let (keys, newkey, previous_value, idmap) =
      match (S.mem ~no:false mp.keys key, exist) with
      | MS.No, Some true -> raiserror MS.Elt_not_found (Elt.elt2s key)
      | MS.Undetermined, Some true -> raiserror MS.Partial (Elt.elt2s key)
      | MS.Yes _, Some false -> raiserror MS.Overwrite (Elt.elt2s key)
                                  
      | (MS.No | MS.Undetermined), _ ->
        begin match default with
          | None -> raiserror MS.Elt_not_found (Elt.elt2s key)
          | Some defv -> (S.merge_in mp.keys key, key, defv key, mp.idmap)
        end
      
      | MS.Yes all, _ ->
        let (keys, newkey) = S.merge_in_e ~exist:true ?force mp.keys key in

        if keys == mp.keys then (keys, newkey, M.find (get_firstid newkey) mp.idmap, mp.idmap)
        else
          
          (* Find keys that have been removed. *)
          match S.mem ~no:false mp.keys newkey with
          | MS.No | MS.Undetermined -> assert false
          | MS.Yes [] -> assert false
          | MS.Yes (ckey1 :: changed_keys) ->

            let oldid1 = get_firstid ckey1 in
            assert (M.mem oldid1 mp.idmap) ;
            let oldv1 = M.find oldid1 mp.idmap
            and idm1  = M.remove oldid1 mp.idmap in
            
            (* Remove these ids. *)
            let (idmap, newv) = List.fold_left
                begin fun (idm, vv) ckey ->
                  let oldid = get_firstid ckey in
                  assert (M.mem oldid idm) ;
                  let vv' = mp.mergev (M.find oldid idm) vv in
                  (M.remove oldid idm, vv')
                end
                (idm1, oldv1) changed_keys
            in
            (keys, newkey, newv, idmap)
    in
    let idmap = M.add (get_firstid newkey) (f newkey previous_value) idmap in

    { keys ;
      v2s    = mp.v2s ;
      mergev = mp.mergev ;
      idmap }, newkey

  let update ?exist ?force ?default ?f mp key = fst (update_k ?exist ?force ?default ?f mp key)

  let add_k ?exist ?force mp key v = update_k ?exist ?force ~default:(fun _ -> v) ~f:(fun _ _ -> v) mp key
                                                   
  let add ?exist ?force mp key v = fst (add_k ?exist ?force mp key v)

  
  let find_result2s mp res = match res with
    | MS.No -> "No"
    | MS.Undetermined -> "Undetermined"
    | MS.Yes l -> Printf.sprintf "Yes [ %s ]"
                    (List.fold_left (fun acu (key,v) -> (if acu = "" then "" else acu ^ " ; ") ^ Elt.elt2s key ^ " => " ^ mp.v2s v) "" l)

  let find ~no mp key =
    match S.mem ~no mp.keys key with
    | MS.No -> MS.No
    | MS.Undetermined -> MS.Undetermined
    | MS.Yes keys ->
      MS.Yes (List.map (fun k -> (k, M.find (get_firstid k) mp.idmap)) keys)

  let remove mp key =
    let keys = S.remove mp.keys key
    and idmap =
      let fid = get_firstid key in
      assert (M.mem fid mp.idmap) ;
      M.remove fid mp.idmap
    in

    { keys ;
      v2s = mp.v2s ;
      mergev = mp.mergev ;
      idmap }        

  let mmap mp mergev f v2s =
    { keys = mp.keys ;
      v2s ;
      mergev ;
      idmap = M.map f mp.idmap }

  let fold ?comparekey ?compare mp acu f =
    S.fold ?comparekey ?compare mp.keys acu (fun key acu -> f key (M.find (get_firstid key) mp.idmap) acu)

  let map2s ?comparekey ?compare ?(title="") mp =
    let init = Printf.sprintf "===================  MAP%s  =================" (if title="" then "" else ": " ^ title) in
    let all =
      fold ?comparekey ?compare mp (init ^ "\n\n")
        begin fun key v acu ->
          acu ^ Printf.sprintf " - %s => %s\n" (Elt.elt2s key) (mp.v2s v)
        end
    in
    all ^ String.make (String.length init) '='

end
