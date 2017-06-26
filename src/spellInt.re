module IntMap =
  Map.Make {
    type t = int;
    let compare = compare;
  };

let spellingsByNumber =
  IntMap.(
    empty |> add 0 "zero" |> add 1 "one" |> add 2 "two" |> add 3 "three" |> add 4 "four" |>
    add 5 "five" |>
    add 6 "six" |>
    add 7 "seven" |>
    add 8 "eight" |>
    add 9 "nine" |>
    add 10 "ten" |>
    add 11 "eleven" |>
    add 12 "twelve" |>
    add 13 "thirteen" |>
    add 14 "fourteen" |>
    add 15 "fifteen" |>
    add 16 "sixteen" |>
    add 17 "seventeen" |>
    add 18 "eighteen" |>
    add 19 "nineteen" |>
    add 20 "twenty" |>
    add 30 "thiry" |>
    add 40 "forty" |>
    add 50 "fifty" |>
    add 60 "sixty" |>
    add 70 "seventy" |>
    add 80 "eighty" |>
    add 90 "ninety"
  );

let spellInt (number: int) => {
  let signSpelling = number < 0 ? "negative " : "";
  let rec buildSpelling (number: int) =>
    try (IntMap.find number spellingsByNumber) {
    | Not_found =>
      switch number {
      | number when number < 100 =>
        let rest = number mod 10;
        buildSpelling (number - rest) ^ " " ^ (rest === 0 ? "" : buildSpelling rest)
      | number when number < 1000 =>
        let rest = number mod 100;
        buildSpelling ((number - rest) / 100) ^
        " hundered" ^ (rest === 0 ? "" : " and " ^ buildSpelling rest)
      | number when number < 1000000 =>
        let rest = number mod 1000;
        buildSpelling ((number - rest) / 1000) ^
        " thousand" ^ (rest === 0 ? "" : ", " ^ buildSpelling rest)
      | number when number >= 1000000 =>
        let rest = number mod 1000000;
        buildSpelling ((number - rest) / 1000000) ^
        " million" ^ (rest === 0 ? "" : ", " ^ buildSpelling rest)
      | _ => "NIL"
      }
    };
  switch (buildSpelling (abs number)) {
  | "NIL" => None
  | numberSpelling => Some (signSpelling ^ numberSpelling)
  }
};