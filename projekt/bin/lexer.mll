{
  open Lexing
  open Lexing_types
  open Lexing_utils

  let handleError lexbuf =
      let pos = (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf) in
      let token = Lexing.lexeme lexbuf in
      let exc = InvalidToken (mkLocation pos, token) in
      raise exc

  let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

    let operator_table =
        create_hashtable 23 [
            (";" , Operator(Semi));
            ("=" , Operator(Assign));
            ("->", Operator(Binding));
            ("$" , Operator(Apply));
            ("," , Operator(Comma));
            ("==", Operator(Relation(Equals)));
            ("<>", Operator(Relation(NotEquals)));
            ("<" , Operator(Relation(Less)));
            (">" , Operator(Relation(Greater)));
            ("<=", Operator(Relation(LessEqual)));
            (">=", Operator(Relation(GreaterEqual)));
            ("+" , Operator(Arthmetic(Add)));
            ("-" , Operator(Arthmetic(Sub)));
            ("*" , Operator(Arthmetic(Mult)));
            ("/" , Operator(Arthmetic(Div)));
            ("%" , Operator(Arthmetic(Mod)));
            ("||", Operator(Boolean(Or)));
            ("&&", Operator(Boolean(And)));
            ("!" , Operator(Boolean(Not)));
            ("(" , Operator(OpenBracket));
            (")" , Operator(CloseBracket));
            ("{" , Operator(OpenScope));
            ("}" , Operator(CloseScope));
        ]

    let builtin_table : (string, token) Hashtbl.t =
        create_hashtable 11 [
            ("if"       , If);
            ("fst"      , Builtin(Fst));
            ("snd"      , Builtin(Snd));
            ("readc"    , Builtin(Readc));
            ("writec"   , Builtin(Writec));
            ("at"       , Builtin(At));
            ("true"     , Bool(true));
            ("false"    , Bool(false));
            ("is_number", Builtin(NumberPred));
            ("is_bool"  , Builtin(BoolPred));
            ("is_unit"  , Builtin(UnitPred));
            ("is_pair"  , Builtin(PairPred));
        ]
}

    let identifier = ['a'-'z' 'A'-'Z' '_']+
    let number     = ['1'-'9']['0'-'9']* | '0'
    let operator   = [';' '=' ',' '+' '-' '*' '(' ')' '/' '%' '{' '}' '!' '<' '>' '$'] 
                    | "==" | "<>" | "<=" | ">=" | "->" | "&&" | "||"
    let whitespace = ['\t' ' ']
    let char_literal   = [' '-'[' ']'-'~']
    let escape_char    = '\\' ['0' 't' 'n' 'r' '"' ''' '\\'] | '\\' ['0'-'7'] ['0'-'7'] ['0'-'7']

    rule token = parse
      | ['\n']
      { new_line lexbuf; token lexbuf }

      | "//"
      { line_comment lexbuf }

      | eof
      { EOF }

      | "()"
      { Unit }

      | ''' ( (char_literal) as c ) '''
      {
         Number(int_of_char c)
      }

      | identifier as id
      {
        try
            Hashtbl.find builtin_table id
        with Not_found ->
            Id(id)
      }

      | (number as num)
      {
        match (int_of_string_opt num) with
        | Some(num) -> Number(num)
        | None -> handleError lexbuf
      }

      | whitespace+
      { token lexbuf }

      | operator as op
      {
        try
            Hashtbl.find operator_table op
        with Not_found ->
            handleError lexbuf
      }

      | _
      { handleError lexbuf }


  and line_comment = parse
      | '\n'
      { new_line lexbuf; token lexbuf }

      | eof
      { EOF }

      | _
      { line_comment lexbuf }



