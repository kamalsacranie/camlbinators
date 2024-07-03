module type ParserInner = sig
  type metadata [@@deriving show]
  type error = string [@@deriving show]

  (* Figure out a nicer way to handle errors*)
  type 'a with_md [@@deriving show]

  val md : 'a with_md -> metadata
  val nomd : 'a with_md -> 'a
  val bind_md : metadata -> 'a -> 'a with_md
  val split_md : 'a with_md -> 'a * metadata
  val default_md : metadata
  val take_input_md : int -> metadata
  val merge_mds : metadata list -> metadata
end

module type ParserInput = sig
  type atom [@@deriving show]
  type feed [@@deriving show]
  type state [@@deriving show]

  val head : feed -> atom option
  val rest : feed -> feed option
  val separate : feed -> (atom * feed) option
  val show_atom : atom -> string
end

module type Parser = sig
  type atom [@@deriving show]
  type feed [@@deriving show]
  type metadata [@@deriving show]
  type 'a with_md [@@deriving show]
  type state [@@deriving show]
  type input = { input : feed; pos : int; state : state } [@@deriving show]
  type error = { err : string; pos : int; state : state } [@@deriving show]

  type 'a parser_result = ('a with_md * input, error list) result
  [@@deriving show]

  type 'a t = { parse : input -> 'a parser_result }

  val md : 'a with_md -> metadata
  val nomd : 'a with_md -> 'a
  val bind_md : metadata -> 'a -> 'a with_md
  val split_md : 'a with_md -> 'a * metadata
  val default_md : metadata
  val take_input_md : int -> metadata
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
  val fail : (feed -> string) -> 'a t
  val alternate : 'a t -> 'a t -> 'a t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val choice : 'a t list -> 'a t
  val is' : 'a t -> unit t
  val not' : unit t -> unit t
  val is_not : 'a t -> unit t
  val take : atom t
  val satisfies : (atom -> bool) -> atom t
  val ( <!!> ) : 'a t -> (feed -> string) -> 'a t
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
  val get : state t
  val set : state -> unit t
  val modify : (state -> state) -> state t
end

module Parser (M : ParserInner) (I : ParserInput) :
  Parser
    with type atom = I.atom
     and type feed = I.feed
     and type 'a with_md = 'a M.with_md
     and type metadata = M.metadata
     and type state = I.state = struct
  type atom = I.atom [@@deriving show]
  type feed = I.feed [@@deriving show]
  type metadata = M.metadata [@@deriving show]
  type 'a with_md = 'a M.with_md [@@deriving show]
  type state = I.state [@@deriving show]
  type input = { input : feed; pos : int; state : state } [@@deriving show]
  type error = { err : string; pos : int; state : state } [@@deriving show]

  type 'a parser_result = ('a with_md * input, error list) result
  [@@deriving show]

  type 'a t = { parse : input -> 'a parser_result }

  let md = M.md
  let nomd = M.nomd
  let bind_md = M.bind_md
  let split_md = M.split_md
  let default_md = M.default_md
  let take_input_md = M.take_input_md
  let merge_mds = M.merge_mds
  let md_fmap f x = x |> M.nomd |> f |> M.bind_md (x |> M.md)

  let md_apply (f : ('a -> 'b) with_md) (x : 'a with_md) : 'b with_md =
    let f, mdf = M.split_md f in
    let x, mdx = M.split_md x in
    f x |> M.bind_md (M.merge_mds [ mdf; mdx ])

  (** [wrap p] take the parser [p] and runs the parser on the input. It then
    wraps the parsed token, which consists of the actual token and its
    associated metadata, in its own metadata. This effectively nests the
    metadata and crystalises it for that token. For example, if my metadata
    is source mappings, and we parsed that characted ['x'] from the input
    "x", our parser's inner result would be [val x : char with_md]. Wraping
    this will yield [val x: char with_md with_md]. This means the parser
    will continue modifying the outermd in the next parse functions. *)
  let wrap (p : 'a t) : 'a with_md t =
    let parse input =
      Result.bind (p.parse input) (fun (a, input') ->
          Result.ok (a |> M.bind_md (M.md a), input'))
    in
    { parse }

  (** Standard fmap for a functor.
    @param f is a function from 'a to 'b
    @param p is a parser of type 'a
    @return 'b t, the parser after having the function applied to the inner value *)
  let map (f : 'a -> 'b) (p : 'a t) =
    let parse input =
      Result.(
        bind (p.parse input) (fun (x, input') -> ok (md_fmap f x, input')))
    in
    { parse }

  let ( $> ) = map
  let ( <$ ) p f = map f p
  let ( $$> ) f p = f $> (p |> wrap)
  let ( <$$ ) p f = f $$> p

  (** Canonical pure function. Takes [x] and wraps it with the default provided metadata value *)
  let pure x =
    { parse = (fun input -> Result.ok (x |> M.bind_md M.default_md, input)) }

  (** Used for lifting a value with its metadata alread bound, into a parser
      monad. I.e., [x] is an instance of ['a M.with_md] *)
  let pure_whole x = { parse = (fun input -> Result.ok (x, input)) }

  let apply (mf : ('a -> 'b) t) (p : 'a t) =
    let open Result in
    let parse input =
      bind (mf.parse input) (fun (f, input') ->
          bind (p.parse input') (fun (a, input'') -> ok (md_apply f a, input'')))
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
    let parse input =
      Result.bind (p.parse input) (fun (a, input') -> (mf a).parse input')
    in
    { parse }

  let ( >>== ) = longbind

  let bind p mf =
    let open Result in
    let parse input =
      bind (p.parse input) (fun (a, input') ->
          let aa, mda = M.split_md a in
          bind ((mf aa).parse input') (fun (b, input'') ->
              let bb, mdb = M.split_md b in
              ok (bb |> M.bind_md (M.merge_mds [ mda; mdb ]), input'')))
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
    pure_whole (a |> M.nomd |> M.bind_md merged_mds)

  (** Like *> but merges the metadata of the inner item that is ignored *)
  let ( **> ) pa pb =
    (* let pa = symbol '[' in *)
    (* let pb = (some_sep (some digit) (symbol ',')) in *)
    pa >>== fun a ->
    pb >>== fun b ->
    pure_whole (b |> M.nomd |> M.bind_md (M.merge_mds [ a |> M.md; b |> M.md ]))

  let replace pa b = pa >>= fun _ -> pure b
  let ( << ) = replace

  let pass =
    let parser input = Result.ok (() |> M.bind_md M.default_md, input) in
    { parse = parser }

  let fail e =
    {
      parse =
        (fun { pos; input; state } ->
          Result.error [ { err = e input; pos; state } ]);
    }

  (** This is the canonical alternate which tries the first parser and then if
      that doesnt work, tries the second parser *)
  let alternate pa pb =
    let open Result in
    let parse input =
      match pa.parse input with
      | Ok (a, input') -> ok (a, input')
      | Error _ -> pb.parse input
    in
    { parse }

  let ( <|> ) = alternate

  let choice parser_list =
    match parser_list with
    | [] -> fail (fun _ -> "choice parser had no choices to choose from")
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
    let parser input =
      bind (p.parse input) (fun _ -> ok (() |> M.bind_md M.default_md, input))
    in
    { parse = parser }

  let not' (p : unit t) =
    let parser input =
      match p.parse input with
      | Ok _ -> (fail (fun _ -> "didn't satisfy not")).parse input
      | Error _ -> Result.ok (() |> M.bind_md M.default_md, input)
    in
    { parse = parser }

  let is_not p = p |> is' |> not'

  let take : atom t =
    let open Result in
    let parse ({ input; pos; state } as i) =
      I.separate input |> function
      | None -> (fail (fun _ -> "end of input")).parse i
      | Some (head, rest) ->
          ok
            ( head |> M.bind_md (M.take_input_md pos),
              { input = rest; pos = pos + 1; state } )
    in
    { parse }

  let satisfies predicate : atom t =
    take >>= fun a ->
    if predicate a then pure a
    else
      fail (fun _ ->
          Printf.sprintf "Predicate not satisfied for %s" (I.show_atom a))

  let ( <!!> ) (l : 'a t) f =
    let parse ({ input; pos; state } as i : input) =
      l.parse i
      |> Result.map_error (fun e -> [ { err = f input; pos; state } ] @ e)
    in
    { parse }

  let ( <!> ) (l : 'a t) f = l <!!> fun _ -> f

  let atom (c : atom) : atom t =
    satisfies (fun a -> a == c)
    <!> Printf.sprintf "Failed to match atom %s" (I.show_atom c)

  let many p =
    let rec many' acc input =
      match p.parse input with
      | Ok (a, input') -> many' (a :: acc) input'
      | Error _ -> (acc, input)
    in
    let parse input =
      let result_as, input' = many' [] input in
      let span = List.map (fun r -> r |> M.md) result_as |> M.merge_mds in
      Result.ok (List.rev result_as |> M.bind_md span, input')
    in
    { parse }

  let merge_list_md a = List.map M.nomd a
  let many_merge p = merge_list_md $> many p
  let some' p many = List.cons $> p <*> many <!> "failed to match one or more"
  let some p = some' (p |> wrap) (many p)
  let some_merge p = some' p (many_merge p)

  let optional (p : 'a t) =
    {
      parse =
        (fun input ->
          match p.parse input with
          | Ok (a, input') ->
              let a, md = M.split_md a in
              Ok (Some a |> M.bind_md md, input')
          | Error _ -> Ok (None |> M.bind_md M.default_md, input));
    }

  let ( ?% ) = optional

  let many_sep p sep =
    choice
      [
        (fun x xs -> x :: xs) $> (p |> wrap) <*> many (sep *> p);
        (function Some x -> [ x ] | None -> []) $> ?%(p |> wrap);
      ]

  let many_sep_merge p sep = merge_list_md $> many_sep p sep

  let some_sep p sep =
    (fun x xs -> x :: xs)
    $> (p |> wrap)
    <*> many (sep *> p)
    <* ?%sep <!> "couldn't match one or more delimited items"

  let get =
    let parse (input : input) =
      Ok (input.state |> M.bind_md M.default_md, input)
    in
    { parse }

  let set new_state =
    let parse { input; pos; _ } =
      Ok (() |> M.bind_md M.default_md, { input; pos; state = new_state })
    in
    { parse }

  let modify f = (f $> get >>= set) *> get
end
