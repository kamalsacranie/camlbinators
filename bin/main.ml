open Camlbinators.StringParser

let _ =
  let id a = a in
  let arr = id $$> symbol '[' <$$ id <$ id <$$ id in
  let res = arr.parse { input = "[ 100, 200, 300, ]"; pos = 0 } in
  show_parser_result
    (fun f b -> pp_with_md (pp_with_md (pp_with_md pp_atom)) f b)
    res
  |> print_endline
