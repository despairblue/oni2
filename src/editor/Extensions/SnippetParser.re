// type marker =
// | TextmateSnippet.t
// | Text.t;

// type baseMarker = {
//   value: string,
//   children: list(marker),
// };

type placeholder = {index: int};
type textmateSnippet = {placeholder: (placeholder, list(placeholder))};
type text = {value: string};
type choice = {options: list(text)};
type transform = {regexp: Str.regexp};
type formatString = {
  index: int,
  shorthandName: option(string),
  ifValue: option(string),
  elseValue: option(string),
};
type variable = {name: string};

type marker =
  | Placeholder(placeholder)
  | TextmateSnippet(textmateSnippet)
  | Text(text)
  | Choice(choice)
  | Transform(transform)
  | FormatString(formatString)
  | Variable(variable);

module Marker = {
  // type kind = Choice | Placeholder
  type t = {value: string};

  // TODO: implement
  let escape = (value: string): string => value;

  let make = (value: string): t => {value: value};
  // TODO: implement
  // toString
  // toTextmateString
  // len
  // clone
};

module Text = {
  type t = {children: list(t)};
};

module Choice = {
  type t = {options: list(Text.t)};

  let make = (): t => {options: []};
};

module Placeholder = {
  type t = {
    index: int,
    children: list(t),
  };

  // STATIC
  let compareByIndex = (a, b) => 0;

  // CONSTRUCTOR
  let create = (index: int): t => {index, children: []};

  let isFinalTabstop = (placeholder: t): bool => placeholder.index == 0;

  let choice = (placeholder: t): option(Choice.t) => None;

  let rec toTextmateString = (placeholder: t): string =>
    if (List.length(placeholder.children) === 0) {
      Printf.sprintf(
        "\\$%i",
        placeholder.index,
        // else if()
      );
    } else {
      let placeholderStrings =
        placeholder.children
        |> ListLabels.map(~f=toTextmateString)
        |> StringLabels.concat(~sep="");

      Printf.sprintf("\\$%i:%s", placeholder.index, placeholderStrings);
    };
};

module TextmateSnippet = {
  type t = {
    children: list(t),
    placeholder: (Placeholder.t, list(Placeholder.t)),
  };
};

module JSONParser = {
  type token =
    | NULL
    | TRUE
    | FALSE
    | STRING(string)
    | INT(int)
    | FLOAT(float)
    | ID(string)
    | LEFT_BRACK
    | RIGHT_BRACK
    | LEFT_BRACE
    | RIGHT_BRACE
    | COMMA
    | COLON
    | EOF;
};
