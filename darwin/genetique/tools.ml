

let marshal_read_cand filename =
  let ic = open_in_bin filename in
  let return = (Marshal.from_channel ic : t_candidat) in
  close_in ic;
  return

let marshal_read_popu filename =
  let ic = open_in_bin filename in
  let return = (Marshal.from_channel ic : t_stockage) in
  close_in ic;
  return

let marshal_write filename element =
  let oc = open_out_bin filename in
  Marshal.to_channel oc element [Marshal.Closures; Marshal.Compat_32];
  close_out oc;
  ()
