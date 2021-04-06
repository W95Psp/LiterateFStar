module FStar.List.Supplement

open FStar.List.Tot

let max x y = if x > y then x else y
let rec drop (n: nat) (l: list 'a): (r: list 'a {length r = max 0 (length l - n)})
  = match n, l with
  | 0,_ | _,[]  -> l
  | _,hd::tl -> drop (n-1) tl
let rec take (n: nat) (l: list 'a): (r: list 'a {length r = min n (length l)})
  = match n, l with
  | 0,_ | _,[]  -> []
  | _,hd::tl -> hd::take (n-1) tl

let rec take_eq_lemma (l: list 'a)
  : Lemma (take (length l) l == l)
          [SMTPat (take (length l) l)]
  = match l with
  | [] -> ()
  | _::tl -> take_eq_lemma tl

let drop_str (s: string) (n: nat): string = 
  if n > String.length s then ""
  else String.sub s n (String.length s - n)

let take_str (s: string) (n: nat): string = 
  if n > String.length s then s
  else String.sub s 0 n

let fold_left' (f: 'a -> 'a -> 'a) (l: list 'a {Cons? l}) = 
  fold_left f (hd l) (tl l)

let rec span (p: 'a -> bool) (l: list 'a)
  : r: ( (r:list 'a{for_all p r})
       * (r:list 'a{Cons? r ==> ~(p (hd r))}))
    {fst r @ snd r == l}
  = match l with
  | [] -> ([], [])
  | hd::tl -> if p hd then let lR,lL = span p tl in
                         hd::lR,lL
                    else [],l

let rec groupBy (f: 'a -> 'a -> bool) (l: list 'a)
  : Tot (r: list (sub: list 'a {match sub with | [] -> false | hd::tl -> for_all (f hd) tl})
        { flatten (map id r) == l}) (decreases (length l))
  = match l with
  | [] -> []
  | hd::tl -> let hd_class, rest = span (f hd) tl in
            append_length hd_class rest;
            (hd::hd_class)::groupBy f rest

let rec zip
    (l0: list 'a) (l1: list 'b)
  : r: list ('a * 'b) { length r == min (length l0) (length l1) }
  = match l0, l1 with
  | (h0::t0), (h1::t1) -> (h0,h1)::zip t0 t1
  | _ -> []

let rec fst_zip_lemma (l0: list 'a) (l1: list 'b): Lemma (map fst (zip l0 l1) == take (min (length l0) (length l1)) l0)
  = match l0,l1 with
  | _::a, _::b -> fst_zip_lemma a b
  | _ -> ()

let rec snd_zip_lemma (l0: list 'a) (l1: list 'b): Lemma (map snd (zip l0 l1) == take (min (length l0) (length l1)) l1)
  = match l0,l1 with
  | _::a, _::b -> snd_zip_lemma a b
  | _ -> ()

let rec takeWhile (p:'a -> bool) (l: list 'a)
  : yes:list 'a & no:list 'a {for_all p yes /\ yes@no == l}
  = match l with
  | [] -> (| [], [] |)
  | hd::tl -> if p hd then let (| r, rest |) = takeWhile p tl in (| hd::r, rest |)
                    else (| [], l |)

let curry2 (f: 'a -> 'b -> 'c): ('a*'b -> 'c) = fun (x,y) -> f x y

let takeWhileBin (f: 'a -> 'a -> bool) (l: list 'a)
  : yes:list 'a & no:list 'a {yes @ no == l}
  = match l with
  | [] -> (|[], []|)
  | hd::tl -> let zipped = zip l tl in
            let (|r, rest|) = takeWhile (curry2 f) zipped in
            snd_zip_lemma l tl;
            map_append snd r rest;
            (| (hd::map snd r), map snd rest |)

let rec groupBy' (f: 'a -> 'a -> bool) (l: list 'a)
  : Tot (list (sub: list 'a {Cons? sub})) (decreases length l)
  = match l with
  | [] -> []
  | hd::tl -> let (|hd_class, rest|) = takeWhileBin f l in
            hd_class::groupBy' f (append_length hd_class rest; rest)

open FStar.Tactics
let fold_left'_tac (f: 'a -> 'a -> Tac 'a) (l: list 'a {Cons? l}) = 
  fold_left f (hd l) (tl l)

