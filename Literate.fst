module Literate

open FStar.Tactics
open FStar.Tactics.Builtins
module L = FStar.List.Tot
open FStar.List.Supplement
open RangeTools

type comment_kind
  = | StarComment: level: nat -> comment_kind
    | SlashComment: level: nat -> comment_kind
    | AnnotComment
    | CommandComment

type module_fragment =
  | Comment: k:comment_kind -> v:string -> module_fragment
  | Declaration: n:name -> annots:list string -> module_fragment

noeq type modul =
  { fragments: list (rng_view*module_fragment)
  ; module_name: name
  }

let debug_module_fragment
  = function | Comment _ v -> "(* " ^ String.concat "\\n" (String.split ['\n'] v) ^ " *)"
             | Declaration n annots -> "## " ^ String.concat "." n ^ " @"^String.concat ";" annots

let debug_modul m =
  "\n\n# " ^ String.concat "." m.module_name ^ "\n" ^
  String.concat "\n" (map (fun (r,f) -> debug_module_fragment f ^ "   " ^ rng_view_to_string r) m.fragments)
      
val zip: list 'a -> list 'b -> (list ('a & 'b))
let rec zip l1 l2 = match l1,l2 with
    | hd1::tl1, hd2::tl2 -> (hd1,hd2)::(zip tl1 tl2)
    | _ -> []

let rng_view_eq (x y: rng_view): bool
  =  x.file_name = y.file_name
  && x.start_pos = y.start_pos
  && x.end_pos   = y.end_pos

let modul_eq (x y: modul): bool
  = x.module_name = y.module_name
  && L.length x.fragments = L.length y.fragments
  && (L.for_all (fun ((r0,f0), (r1,f1)) -> f0 = f1 && rng_view_eq r0 r1) (zip x.fragments y.fragments))

let trimLeft' (s: list String.char): list String.char = dsnd (takeWhile (fun x -> L.mem x [' ';'\n']) s)
let trimLeft (s: string): string = String.string_of_list (trimLeft' (String.list_of_string s))
let trimRight (s: string): string = String.string_of_list (L.rev (trimLeft' (L.rev (String.list_of_string s))))
let trim s: string = trimRight (trimLeft s)

let parse_comment (s0: string)
  : Tac (comment_kind * string)
  = let s = String.list_of_string s0 in
    let h c = let (|left, right|) = takeWhile ((=) c) s in
              max 0 (L.length left - 2), String.string_of_list (trimLeft' right)
    in
    match s with
    | '/'::'/'::'@'::tl -> AnnotComment,   String.string_of_list (trimLeft' tl)
    | '/'::'/'::'!'::tl -> CommandComment, String.string_of_list (trimLeft' tl)
    | '/'::tl -> let level, str = h '/' in
               SlashComment level, str
    | '('::'*'::tl -> let level, str = h '*' in
               StarComment level, str
    | _ -> fail ("Could not parse comment '"^s0^"'")

let _ = lookup_typ

let ranges_in_module env (m: name): Tac (list (rng_view*module_fragment)) = 
  let fvs = defs_in_module env m in
  let get_range (v: fv): Tac range = 
    match lookup_sigelt_range env (inspect_fv v) with
    | Some r -> r
    | None -> fail ("ranges_in_module: could not get the range for sigelt `"^String.concat "." m^"`") in
  let defs: list (name * range) = map (fun v -> (inspect_fv v, get_range v)) fvs in
  let comments: list (string * range) = comments_of_module m in
  let ranges = map (fun (n,r) -> (inspect_range r,Declaration n [])) defs
             @ map (fun (c,r) -> (inspect_range r,(let k,s = parse_comment c in Comment k s)))     comments
  in
  List.Tot.sortWith (fun (r0,_) (r1,_) -> if rng_view_lt r0 r1 then 1 else -1) ranges

let lookup_modul (m: name): Tac modul
  = { fragments = ranges_in_module (top_env ()) m
    ; module_name = m
    }

let right_after (r0 r1:rng_view): bool
  = fst r0.end_pos + 1 = fst r1.start_pos || fst r0.end_pos = fst r1.start_pos
let are_ranges_bundle r0 r1 = right_after r0 r1 
                         || fst r0.end_pos + 2 = fst r1.start_pos 

let fusion_as_bundle (rx,x) (ry,y) =
   let rxy = fusion_rng_view rx ry in
   match x, y with
   | Comment k x, Comment _ y -> 
        let sep = "\n" in
        rxy, Comment k (x ^ sep ^ y)
   | Declaration n0 a0, Declaration n1 _ ->
        rxy, Declaration (n0 @ ("+"::n1)) a0
   | _ -> fail "fusion_as_bundle: got heterogenous blocks"

let is_bundle a b 
        = match a, b with
        | (r0,Comment k0 _), (r1,Comment k1 _) -> k0 <> CommandComment && k0 = k1 && are_ranges_bundle r0 r1
        | (r0, Declaration _ a0), (r1, Declaration _ a1) -> a0=a1                && are_ranges_bundle r0 r1
        | _ -> false
let is_annot_group a b 
    = match a, b with
      | (r0,Comment AnnotComment _), (r1,Declaration _ _) -> true
      | _ -> false

let strPrefix (pre s: string): bool
  = let pl = String.length pre in
    if pl > String.length s
    then false
    else pre = String.sub s 0 pl

let isUppercase =
  function |'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'
           |'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z' -> true 
           | _ -> false

let is_actual_sigelt (n: name): bool = 
  match n with
  | [] -> false
  | _  -> let last = L.last n in
         not ( strPrefix "uu__" last
             || strPrefix "__" last
             || (match String.list_of_string last with
               | [] -> false | hd::_ -> isUppercase hd
               )
             )

let parse_option (str: string): (string * string)
  = match String.split ['='] str with
  | key::tl -> let value = String.concat "=" tl in
             (key,value)
  | _ -> (str,"")

let parse_options (ss: list string): list (string * string)
  = L.map parse_option ss

let lookup_opt (ss: list (string * string)) (key: string): option string
  = match L.find (fun (k,v) -> k = key) ss with
  | Some (_,v) -> Some v
  | _ -> None

let fusion_annot: (rng_view*module_fragment) -> _ -> Tac _ = fun (rx,x) (ry,y) ->
      guard (Comment? x && Declaration? y);
      let Comment k annot = x in
      guard (AnnotComment? k);
      let Declaration c annots = y in
      ry, Declaration c (String.split ['\n'] annot@annots)

let rec modul_concat_comments' (m: modul)
  : Tac modul
  = let fragments = m.fragments in
    // remove dummy rangers (aka generated definition)
    let fragments = L.filter #(_*module_fragment)
      (fun (r,_) -> r.file_name <> "<dummy>"
      ) fragments
    in
    // remove irrelevant comments (aka non triple slash ones)
    let fragments = L.filter #(_*module_fragment)
      (fun (_,x) -> match x with
               | Comment (SlashComment 0) _ 
               | Comment (StarComment _) _ -> false
               | _ -> true
      ) fragments
    in 
    // remove projectors or stuff like that (i.e. uu__.*)
    let fragments = L.filter #(_*module_fragment)
      (fun (_,x) -> match x with
               | Declaration n _ -> is_actual_sigelt n
               | _ -> true
      ) fragments
    in
    let h f g fragments = map (fold_left'_tac f) (groupBy' g fragments) in
    let fragments = h fusion_annot   is_annot_group   fragments in
    let fragments = h fusion_as_bundle is_bundle fragments in
    let fragments = map #(_*module_fragment)
      (fun (r,x) -> match x with
               | Declaration n annots -> 
                 let f: _ -> _ -> Tac _ = fun (annots',r) annot ->
                   let k, v = parse_option annot in
                   match k with
                   | "offset-start-line" -> annots', (match v with
                                         | "-1" -> {r with start_pos = (fst r.start_pos - 1, snd r.start_pos)}
                                         | "-2" -> {r with start_pos = (fst r.start_pos - 2, snd r.start_pos)}
                                         | "-3" -> {r with start_pos = (fst r.start_pos - 3, snd r.start_pos)}
                                         | _ -> r)
                   | "signature-only" -> annots', {r with end_pos = (fst r.start_pos, 1000)}
                   | _ -> annot::annots', r
                 in
                 let annots, r = fold_left f ([],r) annots in
                 r, Declaration n annots
               | _ -> (r,x)
      ) fragments
    in 
    let fragments = L.filter #(_*module_fragment)
      (fun (_,x) -> not (Declaration? x && L.mem "hide" (Declaration?.annots x))
      ) fragments
    in 
    // let fragments = h fusion_as_bundle is_bundle fragments in
    let fragments = L.flatten (map (
      function | r,Comment CommandComment body ->
                           let args = String.split ['.';' '] body in
                           ( match args with
                           | "show"::n -> (
                                      match lookup_sigelt_range (top_env ()) n with
                                    | Some r -> [inspect_range r, Declaration n []]
                                    | None -> fail ("Cannot 'show' definition "^String.concat "." n)
                           )
                           | "raw"::file -> [r,Comment (SlashComment 0) (contents_of_file (String.concat "." file))]
                           | "open"::file -> (modul_concat_comments' (lookup_modul file)).fragments
                           | _ -> fail ("Unknown command: " ^ String.concat "." args)
                           )
               | r,x -> [r,x]
    ) fragments) in
    { m with fragments = fragments }

let rec modul_concat_comments (m: modul): Tac modul
  = //print ("[modul_concat_comments]\n" ^ debug_modul m);
    let m' = modul_concat_comments' m in
    if modul_eq m' m then m' else modul_concat_comments m'
  
type renderer = rng_view -> module_fragment -> (unit -> Tac string) -> Tac string

let render_modul_as (render: renderer) m
  : Tac string
  = let m = modul_concat_comments (lookup_modul m) in
    // take care of inserting two endlines only when necessary
    let r = fold_left (fun (wasCom,acc) (isCom,s) -> 
                      isCom
                   , (if String.length acc = 0 then s else (if isCom = wasCom then acc ^ "\n" ^ s else acc ^ "\n\n" ^ s))
                   )
                   (true, "")
                   (map (fun (r,f) -> (Comment? f, render r f (fun _ -> find_range_in_file r))) m.fragments) in
    snd r

let qualifier_to_string =
  function   | Assumption -> Some "assmume"
             | New        -> Some "new"
             | Private    -> Some "private"
             | Unfold_for_unification_and_vcgen -> Some "unfold"
             | Irreducible -> Some "irreducible"
             | Inline_for_extraction -> Some "inline_for_extraction"
             | NoExtract   -> Some "noextract"
             | Noeq        -> Some "noeq"
             | Reifiable   -> Some "reifiable"
             | _           -> None

let flatten_options (l: list (option 'a)): list 'a = L.flatten (L.map (function | Some x -> [x] | _ -> []) l)

let attrs_str_of_name n = 
  let attrs = match lookup_typ (top_env ()) n with
            | Some t -> flatten_options (L.map qualifier_to_string (sigelt_quals t))
            | None -> [] in
  String.concat " " attrs

let generic_renderer
  (comment: string -> renderer)
  (preambule_decl postamble_decl: string)
  : renderer =
  fun r f original_text ->
      match f with
    | Comment _ c -> comment c r f original_text
    | Declaration n annots -> 
      let opt = lookup_opt (parse_options annots) in
      let attrs_str = attrs_str_of_name n in
      // let original_text (): Tac _ = trim (original_text ()) in
      let original_text: _ -> Tac _ = 
        if L.mem "hide-quals" annots
        then original_text
        else (fun _ -> (if attrs_str = "" then "" else attrs_str ^ " ") ^ original_text ()) in
      let body = match opt "printer" with
        | Some printer_instruction -> 
               let printer_instruction = trim printer_instruction in
               let chunks = String.split ['|'] printer_instruction in
               guard (Cons? chunks);
               let printer_name::chunks = chunks in
               let fv = explode_qn printer_name in
               ( match lookup_typ (top_env ()) fv with
               | Some t -> 
                 let fT: term = Tv_FVar (pack_fv fv) in
                 let fF: renderer =
                   ( match chunks with
                   | [] -> unquote fT
                   | [a0] -> (unquote fT <: string -> renderer) a0
                   | [a0;a1] -> (unquote fT <: string -> string -> renderer) a0 a1
                   | [a0;a1;a2] -> (unquote fT <: string -> string -> string -> renderer) a0 a1 a2
                   | [a0;a1;a2;a3] -> (unquote fT <: string -> string -> string -> string -> renderer) a0 a1 a2 a3
                   | _ -> fail "The attribute 'printer' support up to 4 arguments only for now."
                   )
                 in
                 fF r f original_text
               | _ -> "[ERROR:printer not found "^printer_name^"]"
               )
        | None -> // String.concat "." n ^ " >> " ^
                 original_text ()
      in
      preambule_decl ^ body ^ postamble_decl

let latex_renderer: renderer = generic_renderer (fun s _ _ _ -> s) "\\begin{fstarcode}\n" "\n\\end{fstarcode}"

let modul_to_latex = render_modul_as latex_renderer

let contents_to_file (path content: string)
  : Tac unit
  = let _ = launch_process "bash" ["-c";"cat - > \"$0\""; path] content in ()
  
let modul_to_latexFile_tac path m = 
  contents_to_file path (modul_to_latex m)

let modul_to_latexFile path m
  : Pure unit
         (requires (set_range_of (with_tactic (fun () -> modul_to_latexFile_tac path m; trivial ()) (squash True)) (range_of path)))
         (ensures (fun _ -> True))
  = ()


