module RangeTools

open FStar.List.Supplement
open FStar.Tactics
module L = FStar.List.Tot

let rng_view_lt (r0 r1: rng_view) =
  let (l0,c0) = r0.start_pos in
  let (l1,c1) = r1.start_pos in
  l0 < l1 || (l0 = l1 && c0 < c1)

let range_lt (r0 r1: range) =
  rng_view_lt (inspect_range r0) (inspect_range r1)

let fusion_rng_view r0 r1 =
  { file_name = r0.file_name; start_pos = r0.start_pos; end_pos = r1.end_pos }

let rng_view_to_string {file_name; start_pos; end_pos}
  = let h (x,y) = string_of_int x ^ "," ^ string_of_int y in
    file_name ^ "@" ^ h start_pos ^ ":" ^ h end_pos
let range_to_string r = rng_view_to_string (inspect_range r)

let contents_of_file (path: string)
  : Tac string
  = launch_process "cat" [path] ""

let find_range_in_file (r: rng_view)
  : Tac string
  = let (start_line, start_col) = r.start_pos in
    let (end_line, end_col) = r.end_pos in
    if end_line < start_line || start_line < 1
    then fail ("find_range_in_file: malformed range, end line is before start line (" ^ rng_view_to_string r ^ ")")
    else ();
    let path = r.file_name in
    let lines = String.split ['\n'] (contents_of_file path) in
    let sel = take (end_line - start_line + 1) (drop (start_line - 1) lines) in
    if (end_line = start_line && end_col < start_col) || start_col < 0 || end_col < 0
    then fail ("find_range_in_file: malformed range, end column is before start column (" ^ rng_view_to_string r ^ ")")
    else ();
    if Nil? sel then fail "error, emtpy range" else ();
    let sel = (drop_str (L.hd sel) start_col)::L.tl sel in
    let n:nat = if end_line = start_line then end_col - start_col else end_col in
    let sel = L.init sel@[take_str (L.last sel) n] in
    String.concat "\n" sel
