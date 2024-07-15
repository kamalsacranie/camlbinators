(** The module which describes the metadata and its handling for the parser.
    These metadata actions are used by the parser to handle metadata with
    whatever is inside the parser *)
module type ParserInner = sig
  type metadata [@@deriving show, eq, yojson]
  type error = string [@@deriving show]
  type 'a with_md [@@deriving show, eq]
  type atom [@@deriving show]
  type input [@@deriving show]
  type state [@@deriving show]

  val md : 'a with_md -> metadata
  val nomd : 'a with_md -> 'a
  val bind_md : metadata -> 'a -> 'a with_md
  val default_md : metadata
  val take_input_md : input -> metadata
  val merge_mds : metadata list -> metadata
  val head : input -> atom option
  val rest : input -> input option
  val separate : input -> (atom * input) option
  val show_atom : atom -> string
  val create_error : error -> input * state -> error
end

module type Parser = sig
  type atom [@@deriving show]
  type input [@@deriving show]
  type metadata [@@deriving show, eq, yojson]
  type 'a with_md [@@deriving show]
  type state [@@deriving show]
  type error [@@deriving show]
  (* { err : string; pos : int; state : state } *)

  type 'a parser_result = ('a with_md * input * state, error list) result
  [@@deriving show]

  type 'a t = { parse : input * state -> 'a parser_result }

  val md : 'a with_md -> metadata
  val nomd : 'a with_md -> 'a
  val bind_md : metadata -> 'a -> 'a with_md
  val ( <^> ) : 'a -> metadata -> 'a with_md
  val split_md : 'a with_md -> 'a * metadata
  val default_md : metadata
  val take_input_md : input -> metadata
  val merge_mds : metadata list -> metadata
  val md_fmap : ('a -> 'b) -> 'a with_md -> 'b with_md
  val md_apply : ('a -> 'b) with_md -> 'a with_md -> 'b with_md
  val wrap : 'a t -> 'a with_md t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val ( $> ) : ('a -> 'b) -> 'a t -> 'b t
  val ( <$ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( $$> ) : ('a with_md -> 'b) -> 'a t -> 'b t
  val ( <$$ ) : 'a t -> ('a with_md -> 'b) -> 'b t
  val pure : 'a -> 'a t
  val pure_whole : 'a with_md -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( <**> ) : ('a with_md -> 'b) t -> 'a t -> 'b t
  val longbind : 'a t -> ('a with_md -> 'b t) -> 'b t
  val ( >>== ) : 'a t -> ('a with_md -> 'b t) -> 'b t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( <* ) : 'a t -> 'b t -> 'a t
  val ( *> ) : 'a t -> 'b t -> 'b t
  val ( <** ) : 'a t -> 'b t -> 'a t
  val ( **> ) : 'a t -> 'b t -> 'b t
  val replace : 'a t -> 'b -> 'b t
  val ( << ) : 'a t -> 'b -> 'b t
  val pass : unit t
  val fail : string -> 'a t
  val alternate : 'a t -> 'a t -> 'a t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val choice : 'a t list -> 'a t
  val is' : 'a t -> unit t
  val not' : unit t -> unit t
  val is_not : 'a t -> unit t
  val take : atom t
  val satisfies : (atom -> bool) -> atom t
  val ( <!!> ) : 'a t -> (input * state -> string) -> 'a t
  val ( <!> ) : 'a t -> string -> 'a t
  val atom : atom -> atom t
  val many : 'a t -> 'a with_md list t
  val merge_list_md : 'a with_md list -> 'a list
  val many_merge : 'a t -> 'a list t
  val some' : 'a t -> 'a list t -> 'a list t
  val some : 'a t -> 'a with_md list t
  val some_merge : 'a t -> 'a list t
  val optional : 'a t -> 'a option t
  val ( ?% ) : 'a t -> 'a option t
  val many_sep : 'a t -> 'b t -> 'a with_md list t
  val many_sep_merge : 'a t -> 'b t -> 'a list t
  val some_sep : 'a t -> 'b t -> 'a with_md list t
  val some_sep_merge : 'a t -> 'b t -> 'a list t
  val get : state t
  val set : state -> unit t
  val modify : (state -> state) -> unit t
  val p_lazy : 'a t lazy_t -> 'a t
  val mapping : ('a t * 'b) list -> 'b t
  val rept : int -> 'a t -> 'a list t
end

module Parser (M : ParserInner) :
  Parser
    with type atom = M.atom
     and type input = M.input
     and type 'a with_md = 'a M.with_md
     and type metadata = M.metadata
     and type error = M.error
     and type state = M.state = struct
  type atom = M.atom [@@deriving show]
  type input = M.input [@@deriving show]
  type metadata = M.metadata [@@deriving show, eq, yojson]
  type 'a with_md = 'a M.with_md [@@deriving show, eq]
  type state = M.state [@@deriving show]
  type error = M.error [@@deriving show]

  type 'a parser_result = ('a with_md * input * state, error list) result
  [@@deriving show]

  type 'a t = { parse : input * state -> 'a parser_result }

  (** Given a thing & its associated metdata, just returns its metadata *)
  let md = M.md

  (** Given a thing & its associated metadata, just returns the thing *)
  let nomd = M.nomd

  (** Binds some metadata to some thing *)
  let bind_md = M.bind_md

  (** Binds some metadata to some thing *)
  let ( <^> ) a b = M.bind_md b a

  (** Splits some thing & its metadata into a tuple of the thing and its
      metadata *)
  let split_md x = (x |> nomd, x |> md)

  (** Creates the default metadata to assign to a thing *)
  let default_md = M.default_md

  (** A function which receives the position when taking from the feed and creates some
      metadata. We really need to find a way to make it so that we pass the
      atom as well but right now the types don't really allow it *)
  let take_input_md = M.take_input_md

  (** A function to merge an abitrary number of metadatas *)
  let merge_mds = M.merge_mds

  (** Applies a function to the thing but not its metadata *)
  let md_fmap f x = x |> M.nomd |> f <^> (x |> M.md)

  (** Applicative behaviour for metadata things *)
  let md_apply (f : ('a -> 'b) with_md) (x : 'a with_md) : 'b with_md =
    let f, mdf = split_md f in
    let x, mdx = split_md x in
    f x <^> M.merge_mds [ mdf; mdx ]

  (** [wrap p] take the parser [p] and runs the parser on the input. It then
    wraps the parsed token, which consists of the actual token and its
    associated metadata, in its own metadata. This effectively nests the
    metadata and crystalises it for that token. For example, if my metadata
    is source mappings, and we parsed that characted ['x'] from the input
    "x", our parser's inner result would be [val x : char with_md]. Wraping
    this will yield [val x: char with_md with_md]. This means the parser
    will continue modifying the outermd in the next parse functions. *)
  let wrap (p : 'a t) : 'a with_md t =
    let parse stateful_input =
      Result.bind (p.parse stateful_input) (fun (a, input, state) ->
          Result.ok (a <^> M.md a, input, state))
    in
    { parse }

  (** Standard fmap for a functor.
    @param f is a function from 'a to 'b
    @param p is a parser of type 'a
    @return 'b t, the parser after having the function applied to the inner value *)
  let map (f : 'a -> 'b) (p : 'a t) =
    let parse stateful_input =
      Result.(
        bind (p.parse stateful_input) (fun (x, input, state) ->
            ok (md_fmap f x, input, state)))
    in
    { parse }

  let ( $> ) = map
  let ( <$ ) p f = map f p
  let ( $$> ) f p = f $> (p |> wrap)
  let ( <$$ ) p f = f $$> p

  (** Canonical pure function. Takes [x] and wraps it with the default provided
      metadata value *)
  let pure x =
    {
      parse =
        (fun (input, state) -> Result.ok (x <^> M.default_md, input, state));
    }

  (** Used for lifting a value with its metadata alread bound, into a parser
      monad. I.e., [x] is an instance of ['a M.with_md] *)
  let pure_whole x =
    { parse = (fun (input, state) -> Result.ok (x, input, state)) }

  let apply (mf : ('a -> 'b) t) (p : 'a t) =
    let open Result in
    let parse stateful_input =
      bind (mf.parse stateful_input) (fun (f, input, state) ->
          bind
            (p.parse (input, state))
            (fun (a, input', state') -> ok (md_apply f a, input', state')))
    in
    { parse }

  let ( <*> ) = apply
  let ( <**> ) mf m = mf <*> (m |> wrap)

  (** Used for when you want to have access to the inner value AND its metadata
      in a bind operation. For example, if I parse some brackets and I want
      them to be part of the parsing range, I might use this to get access to
      the opening anclosing bracket metadatas and merge them with the between
      bracket item metadata *)
  let longbind (p : 'a t) (mf : 'a with_md -> 'b t) : 'b t =
    let parse stateful_input =
      Result.bind (p.parse stateful_input) (fun (a, input, state) ->
          (mf a).parse (input, state))
    in
    { parse }

  let ( >>== ) = longbind

  let bind p mf =
    let open Result in
    let parse stateful_input =
      bind (p.parse stateful_input) (fun (a, input, state) ->
          let aa, mda = split_md a in
          bind
            ((mf aa).parse (input, state))
            (fun (b, input', state') ->
              let bb, mdb = split_md b in
              ok (bb <^> M.merge_mds [ mda; mdb ], input', state')))
    in
    { parse }

  let ( >>= ) = bind

  (** [<*] Allow the parsing and then disposing of the result and its metadata
      but keeping the previous parser's result & metadata *)
  let ( <* ) pa pb =
    pa >>== fun a ->
    pb >>== fun _ -> pure_whole a

  (** [*>] Allow the parsing and then disposing of the result and its metadata
      but keeping the following parser's result & metadata*)
  let ( *> ) pa pb =
    pa >>== fun _ ->
    pb >>== fun b -> pure_whole b

  (** Like <* but merges the metadata of the inner item that is ignored *)
  let ( <** ) pa pb =
    pa >>== fun a ->
    pb >>== fun b ->
    let merged_mds = M.merge_mds [ a |> M.md; b |> M.md ] in
    pure_whole (a |> M.nomd <^> merged_mds)

  (** Like *> but merges the metadata of the inner item that is ignored *)
  let ( **> ) pa pb =
    (* let pa = symbol '[' in *)
    (* let pb = (some_sep (some digit) (symbol ',')) in *)
    pa >>== fun a ->
    pb >>== fun b ->
    pure_whole (b |> M.nomd <^> M.merge_mds [ a |> M.md; b |> M.md ])

  let replace pa b = pa >>= fun _ -> pure b
  let ( << ) = replace

  let pass =
    let parser (input, state) = Result.ok (() <^> M.default_md, input, state) in
    { parse = parser }

  let fail s =
    {
      parse =
        (fun stateful_input -> Result.error [ M.create_error s stateful_input ]);
    }

  (** This is the canonical alternate which tries the first parser and then if
      that doesnt work, tries the second parser *)
  let alternate pa pb =
    let open Result in
    let parse stateful_input =
      match pa.parse stateful_input with
      | Ok (a, input, state) -> ok (a, input, state)
      | Error _ -> pb.parse stateful_input
    in
    { parse }

  let ( <|> ) = alternate

  let choice parser_list =
    match parser_list with
    | [] -> fail "choice parser had no choices to choose from"
    | p_acc :: ps -> List.fold_left (fun acc p -> acc <|> p) p_acc ps

  (* Not and is are kind of cruches and they're not really best practice. Take
     for example the string "this_i". Lets say a grammar that has Identifier of
     alphanumerics and subscripted array of alphanums_alphanum. If we parse
     [identifier <|> sarray] on this input, we will always match identifier.
     You could fix this with is' not' but checking the next char is not _, or
     you could be more idiomatic and do [f $> alphanum_string <*> ((some $>
     subscripts) <|> (pure None))]*)
  let is' p =
    let open Result in
    let parser ((input, state) as stateful_input) =
      bind (p.parse stateful_input) (fun _ ->
          ok (() <^> M.default_md, input, state))
    in
    { parse = parser }

  let not' (p : unit t) =
    let parser ((input, state) as stateful_input) =
      match p.parse stateful_input with
      | Ok _ -> (fail "didn't satisfy not").parse stateful_input
      | Error _ -> Result.ok (() <^> M.default_md, input, state)
    in
    { parse = parser }

  let is_not p = p |> is' |> not'

  let take : atom t =
    let open Result in
    let parse ((input, state) as stateful_input) =
      M.separate input |> function
      | None -> (fail "end of input").parse stateful_input
      | Some (head, rest) -> ok (head <^> M.take_input_md input, rest, state)
    in
    { parse }

  let satisfies predicate : atom t =
    take >>= fun a ->
    if predicate a then pure a
    else fail (Printf.sprintf "Predicate not satisfied for %s" (M.show_atom a))

  let ( <!!> ) (l : 'a t) f =
    let parse stateful_input =
      l.parse stateful_input
      |> Result.map_error (fun es ->
             M.create_error (f stateful_input) stateful_input :: es)
    in
    { parse }

  let ( <!> ) (l : 'a t) e = l <!!> fun _ -> e

  let atom (c : atom) : atom t =
    satisfies (fun a -> a == c)
    <!> Printf.sprintf "Failed to match atom %s" (M.show_atom c)

  (* TODO: figure out the correct reversing for this and all its dependencies
     *)
  let many p =
    let rec many' acc ((input, state) as stateful_input) =
      match p.parse stateful_input with
      | Ok (a, input, state) -> many' (a :: acc) (input, state)
      | Error _ -> (acc, input, state)
    in
    let parse stateful_input =
      let result_as, input, state = many' [] stateful_input in
      let span = List.map (fun r -> r |> M.md) result_as |> M.merge_mds in
      Result.ok (List.rev result_as <^> span, input, state)
    in
    { parse }

  let merge_list_md a = List.map M.nomd a
  let many_merge p = merge_list_md $> many p
  let some' p many = List.cons $> p <*> many <!> "failed to match one or more"
  let some p = some' (p |> wrap) (many p)
  let some_merge p = some' p (many_merge p)

  let rec rept n p =
    match n - 1 with
    | 0 -> (fun x -> x :: []) $> p
    | _ when n > 0 -> (fun p ps -> p :: ps) $> p <*> rept (n - 1) p
    | _ -> fail "can't repeat a parser negative times"

  let optional (p : 'a t) =
    {
      parse =
        (fun ((input, state) as stateful_input) ->
          match p.parse stateful_input with
          | Ok (a, input, state) ->
              let a, md = split_md a in
              Ok (Some a <^> md, input, state)
          | Error _ -> Ok (None <^> M.default_md, input, state));
    }

  let ( ?% ) = optional

  let many_sep p sep =
    choice
      [
        (fun x xs -> x :: xs) $$> p <*> many (sep *> p) <* ?%sep;
        (function Some x -> [ x ] | None -> []) $> ?%(p |> wrap <* ?%sep);
      ]

  let many_sep_merge p sep = merge_list_md $> many_sep p sep

  let some_sep p sep =
    many_sep p sep >>= fun els ->
    if List.length els < 1 then
      fail "couldn't match one or more delimited items"
    else pure els

  let some_sep_merge p sep = merge_list_md $> some_sep p sep

  let get =
    let parse (input, state) = Ok (state <^> M.default_md, input, state) in
    { parse }

  let set new_state =
    let parse (input, _) = Ok (() <^> M.default_md, input, new_state) in
    { parse }

  let modify f = f $> get >>= set
  let p_lazy p = { parse = (fun input -> (Lazy.force p).parse input) }
  let mapping ts = ts |> List.map (fun (sign, ty) -> sign << ty) |> choice
end
