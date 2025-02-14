module StringParserInner (M : sig
  type state [@@deriving show, eq]
end) =
struct
  type metadata' = { s : int; l : int } [@@deriving show, eq, yojson]
  type metadata = metadata' option [@@deriving show, eq, yojson]
  type error = string [@@deriving show]
  type 'a with_md = 'a * metadata [@@deriving show, eq, yojson]
  type input = { input : string; pos : int } [@@deriving show]
  type atom = char [@@deriving show]
  type state = M.state [@@deriving show, eq]

  let md (_, md) = md
  let nomd (a, _) = a
  let _bind_md md a = (md, a)
  let default_md = None

  let merge_mds mds =
    List.fold_left
      (fun acc md ->
        match (acc, md) with
        | None, None -> None
        | acc, None -> acc
        | None, md -> md
        | Some a, Some b -> (
            let mds = [ a; b ] in
            mds |> List.sort (fun a b -> a.s - b.s) |> function
            | [ a; b ] ->
                Some { s = a.s; l = max (b.s + b.l) (a.s + a.l) - a.s }
            | _ -> failwith "You suck at coding"))
      None mds

  let bind_md md a = (a, md)
  let valid_string input = String.length input > 0

  let head input =
    if valid_string input.input then Some input.input.[0] else None

  let rest { input; pos } =
    if valid_string input then
      { input = String.sub input 1 (String.length input - 1); pos = pos + 1 }
      |> Option.some
    else None

  let separate input =
    let open Option in
    bind (head input) (fun head ->
        bind (rest input) (fun rest -> some (head, rest)))

  let show_atom atom = Printf.sprintf "%c" atom
  let create_error error _ = error
  let take_input_md { pos; _ } = Some { s = pos; l = 1 }
end

module Utils = struct
  let string_of_char_list s = List.to_seq s |> String.of_seq
  let is_digit a = '0' <= a && '9' >= a
  let is_lowercase a = 'a' <= a && 'z' >= a
  let is_uppercase a = 'A' <= a && 'Z' >= a
  let is_whitespace a = a == ' ' || a == '\n' || a == '\t' || a == '\r'
end

module Parser (M : sig
  type state [@@deriving show, eq]
end) =
struct
  module ParserInner = StringParserInner (M)
  include Parser.Parser (ParserInner)
  open Utils

  let many_char_as_string p = string_of_char_list $> many_merge p
  let some_char_as_string p = string_of_char_list $> some_merge p
  let digit = satisfies is_digit <!> "failed to match digit character"

  let lowercase =
    satisfies is_lowercase <!> "failed to match lowercase character"

  let uppercase =
    satisfies is_uppercase <!> "failed to match uppercase character"

  let letter = lowercase <|> uppercase <!> "failed to match letter character"
  let alphanum = digit <|> letter <!> "failed to match alphanum character"

  let non_whitespace =
    satisfies (fun a -> is_whitespace a |> not)
    <!> "failed to match non-whitespace character"

  let white_space_char =
    satisfies is_whitespace <!> "failed to match whitespace character"

  let white_space_string =
    some_char_as_string white_space_char <!> "failed to match whitespace string"

  let strip p =
    ?%white_space_string *> p <* ?%white_space_string
    <!> "when stripping whitespace"

  let take_string = many_char_as_string non_whitespace

  let string' s p =
    let char_list = List.init (String.length s) (String.get s) |> List.rev in
    let s_match =
      List.fold_left (fun acc c -> List.cons $> p c <*> acc) (pure []) char_list
    in
    string_of_char_list $> s_match
    <!> Printf.sprintf "failed to match string \"%s\"" s

  let string s = string' s atom
  let token s = strip (string s)
  let symbol c = strip (atom c)

  let string_case_insensitive s =
    string' s (fun c ->
        atom (Char.lowercase_ascii c)
        <|> atom (Char.uppercase_ascii c)
        <$ Char.lowercase_ascii)
    <!> "failed to match case insensitive string:"
end
