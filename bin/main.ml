module Term = struct
  type t = Var of string | Lambda of string * t | Application of t * t

  let lambda bind body = Lambda (bind, body)
  let app f x = Application (f, x)
end

module Bruijn = struct
  type t = Var of int | Lambda of t | Application of t * t

  let ( let* ) = Result.bind

  let rec of_term ctx = function
    | Term.Var name -> (
        match List.find_index (fun e -> e == name) ctx with
        | None -> Error "burrice"
        | Some idx -> Ok (Var idx))
    | Term.Lambda (bind, body) ->
        let ctx = bind :: ctx in
        let* body = of_term ctx body in
        Ok (Lambda body)
    | Term.Application (f, x) ->
        let* f = of_term ctx f in
        let* x = of_term ctx x in
        Ok (Application (f, x))

  let rec pprint = function
    | Var idx -> Printf.sprintf "%d" idx
    | Lambda body -> Printf.sprintf "\\. %s" (pprint body)
    | Application (f, x) -> Printf.sprintf "(%s %s)" (pprint f) (pprint x)
end

module Value = struct
  type t = Closure of (t -> t) | Neutral of neutral
  and neutral = Var of int | App of neutral * t

  open Bruijn

  let rec eval env = function
    | Var idx -> List.nth env idx
    | Lambda body ->
        Closure
          (fun x ->
            let env = x :: env in
            eval env body)
    | Application (f, x) -> (
        match (eval env f, eval env x) with
        | Closure f, x -> f x
        | Neutral n, x -> Neutral (App (n, x)))

  let rec quote level = function
    | Closure f ->
        let x = f (Neutral (Var level)) in
        let x = quote (level + 1) x in
        Bruijn.Lambda x
    | Neutral n -> quote_neutral level n

  and quote_neutral level = function
    | Var idx -> Bruijn.Var (level - idx - 1)
    | App (f, x) -> Bruijn.Application (quote_neutral level f, quote level x)
end

let run () =
  let open Term in
  let ( let* ) = Result.bind in

  let zero = lambda "s" (lambda "z" (Var "z")) in
  let succ =
    lambda "p"
      (lambda "s"
         (lambda "z" (app (Var "s") (app (app (Var "p") (Var "s")) (Var "z")))))
  in
  let three = app succ (app succ (app succ zero)) in
  let app = app three three in

  let* app = app |> Bruijn.of_term [] in
  let result = Value.eval [] app in
  Ok result

let () =
  match run () with
  | Ok res ->
      let res = Value.quote 0 res in
      print_endline (Bruijn.pprint res)
  | Error e -> print_endline e
