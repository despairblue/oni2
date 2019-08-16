open Core;
open Oni_Extensions;
open Oni_Extensions.JSONLexer;
open Lexing;

let print_position = (outx, lexbuf) => {
  let pos = lexbuf.lex_curr_p;
  fprintf(
    outx,
    "%s:%d:%d",
    pos.pos_fname,
    pos.pos_lnum,
    pos.pos_cnum - pos.pos_bol + 1,
  );
};

let parse_with_error = lexbuf =>
  try (JSONParser.prog(JSONLexer.read, lexbuf)) {
  | SyntaxError(msg) =>
    fprintf(stderr, "%a: %s\n", print_position, lexbuf, msg);
    None;
  | JSONParser.Error =>
    fprintf(stderr, "%a: syntax error\n", print_position, lexbuf);
    exit(-1);
  };

/* part 1 */
let rec parse_and_print = lexbuf =>
  switch (parse_with_error(lexbuf)) {
  | Some(value) =>
    printf("%a\n", Json.output_value, value);
    parse_and_print(lexbuf);
  | None => ()
  };

let loop = (filename, ()) => {
  let inx = In_channel.create(filename);
  let lexbuf = Lexing.from_channel(inx);
  lexbuf.lex_curr_p = {...lexbuf.lex_curr_p, pos_fname: filename};
  parse_and_print(lexbuf);
  In_channel.close(inx);
};

/* part 2 */
let () =
  Command.basic_spec(
    ~summary="Parse and display JSON",
    Command.Spec.(empty +> anon("filename" %: file)),
    loop,
  )
  |> Command.run;
