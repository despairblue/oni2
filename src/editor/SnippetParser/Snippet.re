type value = [
  | `Assoc(list((string, value)))
  | `Bool(bool)
  | `Float(float)
  | `Int(int)
  | `List(list(value))
  | `Null
  | `String(string)
];

type svalue = [
  | `TabStop(int)
  | `Placeholder(int, string, list(svalue))
  | `Choice(int, list(string))
  | `Variable(string, option(string))
];

/* part 1 */
open Core;
let rec output_value = outc =>
  fun
  | `TabStop(i) => printf("$%d", i)
  | `Placeholder(index, placeholder, _list) =>
    printf("${%d:%s}", index, placeholder)
  | `Choice(index, choices) => print_list(outc, index, choices)
  | `Variable(name, default) => {
      switch (default) {
      | Some(default) => printf("${%s:%s}", name, default)
      | None => printf("$%s", name)
      };
    }

and print_list = (outc, index, arr) => {
  printf("${%d|", index);
  List.iteri(
    ~f=
      (i, v) => {
        if (i > 0) {
          printf(",");
        };
        printf("%s", v);
      },
    arr,
  );
  printf("|}");
};
