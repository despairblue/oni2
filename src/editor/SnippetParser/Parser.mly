%token DOLLAR
%token <int> INT
%token COLON
%token PIPE
%token COMMA
%token LEFT_BRACE
%token RIGHT_BRACE
%token <string> STRING
%token EOF

(* part "1" *)
%start <Snippet.svalue option> prog
%%

(* part "2" *)
prog:
  | EOF       { None }
  | v = value { Some v }
  ;

(* part "3" *)
value:
  | DOLLAR; i = INT                    { `TabStop i }
  | DOLLAR; vl = placeholder_fields    { `Placeholder vl }
  | DOLLAR; vl = choices               { `Choice vl }
  | DOLLAR; vl = variable_with_default { `Variable vl }
  | DOLLAR; vl = variable { `Variable vl }
  ;

placeholder_fields:
    LEFT_BRACE 
    i = INT 
    COLON 
    s = STRING 
    RIGHT_BRACE { (i, s, []) }
    
choices:
    LEFT_BRACE 
    i = INT
    PIPE 
    vl = separated_list(COMMA, STRING) 
    PIPE    
    RIGHT_BRACE { (i, vl) }
    
variable_with_default:
    LEFT_BRACE
    name = STRING 
    COLON 
    default = STRING 
    RIGHT_BRACE { (name, Some(default)) }
  
variable:
    name = STRING { (name, None) }
  
