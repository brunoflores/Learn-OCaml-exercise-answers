let chars = Str.regexp "[a-zA-Z]"

let anagrams (s1 : string) (s2 : string) : bool =
  let module StringMap = Map.Make (Char) in
  let count m c =
    if Str.string_match chars (String.make 1 c) 0 then
      StringMap.update (Char.lowercase_ascii c)
        (function Some v -> Some (v + 1) | None -> Some 1)
        m
    else m
  in
  let s1_hist = String.fold_left count StringMap.empty s1 in
  let s2_hist = String.fold_left count StringMap.empty s2 in
  let s1_bindings = StringMap.bindings s1_hist in
  let s2_bindings = StringMap.bindings s2_hist in
  List.for_all2
    (fun (c1, v1) (c2, v2) -> c1 = c2 && v1 = v2)
    s1_bindings s2_bindings

let () =
  let tests =
    [
      ("Listen", "Silent", true);
      ("New York Times", "monkeys write", true);
      ("evil", "vile", true);
      ("not", "an anagram", false);
    ]
  in
  let test acc (s1, s2, expected) =
    let got = anagrams s1 s2 in
    let res = got = expected in
    let _ =
      if res = false then
        Format.printf "%s and %s, got %b, expected %b\n" s1 s2 got expected
    in
    acc && res
  in
  let all_pass = List.fold_left test true tests in
  assert all_pass
