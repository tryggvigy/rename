module IntMap =
  Map.Make {
    type t = int;
    let compare = compare;
  };
