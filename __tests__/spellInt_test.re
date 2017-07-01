/* Reason */
open Jest;

open SpellInt;

let _ =
  describe
    "SpellInt"
    (
      fun () => {
        open Expect;
        test "spells 0" (fun () => expect (spellInt 0) |> toBe "zero");
        test "spells 00" (fun () => expect (spellInt 00) |> toBe "zero");
        test "spells 001" (fun () => expect (spellInt 001) |> toBe "one");
        test "spells 1" (fun () => expect (spellInt 1) |> toBe "one");
        test
          "spells -555"
          (fun () => expect (spellInt (-555)) |> toBe "negative five hundered and fifty five");
        test "spells 99" (fun () => expect (spellInt 99) |> toBe "ninety nine");
        test "spells 100" (fun () => expect (spellInt 100) |> toBe "one hundered");
        test
          "spells 999" (fun () => expect (spellInt 999) |> toBe "nine hundered and ninety nine");
        test
          "spells 9999"
          (
            fun () => expect (spellInt 9999) |> toBe "nine thousand, nine hundered and ninety nine"
          );
        test
          "spells 99999"
          (
            fun () =>
              expect (spellInt 99999) |> toBe "ninety nine thousand, nine hundered and ninety nine"
          );
        test
          "spells 999999"
          (
            fun () =>
              expect (spellInt 999999) |>
              toBe "nine hundered and ninety nine thousand, nine hundered and ninety nine"
          );
        test "spells 900000" (fun () => expect (spellInt 900000) |> toBe "nine hundered thousand");
        test "spells 1100" (fun () => expect (spellInt 1100) |> toBe "one thousand, one hundered");
        test
          "spells 1111111"
          (
            fun () =>
              expect (spellInt 1111111) |>
              toBe "one million, one hundered and eleven thousand, one hundered and eleven"
          );
        test
          "spells 12345678"
          (
            fun () =>
              expect (spellInt 12345678) |>
              toBe "twelve million, three hundered and forty five thousand, six hundered and seventy eight"
          );
        test
          "spells 123456789"
          (
            fun () =>
              expect (spellInt 123456789) |>
              toBe "one hundered and twenty three million, four hundered and fifty six thousand, seven hundered and eighty nine"
          );
        /* numbers <= billion not supported. */
        test
          "spells 1000000000"
          (fun () => expect (spellInt 1000000000) |> toBe "one thousand million")
      }
    );
