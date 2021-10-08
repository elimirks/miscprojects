type command = PointerLeft
             | PointerRight
             | Increment
             | Decrement
             | OutputChar
             | InputChar
             | Bracket of command list

type env = {
    pointer: int ref;
    memory: int array;
  }

let parse program =
  let stack = ref [[]] in
  let pushCommand c = (c :: (List.hd !stack)) :: List.tl !stack in
  let parseChar character =
    stack := match character with
    | '<' -> pushCommand PointerLeft
    | '>' -> pushCommand PointerRight
    | '+' -> pushCommand Increment
    | '-' -> pushCommand Decrement
    | '.' -> pushCommand OutputChar
    | ',' -> pushCommand InputChar
    | '[' -> [] :: !stack
    | ']' -> (
      match !stack with
      | x::y::xs -> (Bracket (List.rev x) :: y) :: xs
      | _        -> [] (* wat?! Horrible error handling :] *)
    )
    | _   -> !stack
  in
  for i = 0 to String.length program - 1 do
    parseChar program.[i]
  done;
  List.hd !stack |> List.rev

let readChar () =
  try Scanf.scanf "%c" Char.code with
    End_of_file -> 0

let rec step env command =
  let ptr = env.pointer
  and mem = env.memory
  in
  match command with
  | PointerLeft  ->
     if !ptr == 0 then
       ()
     else
       ptr := !ptr - 1
  | PointerRight -> ptr := !ptr + 1
  | Increment    -> Array.set mem !ptr (Array.get mem !ptr + 1)
  | Decrement    -> Array.set mem !ptr (Array.get mem !ptr - 1)
  | OutputChar   -> Array.get env.memory !ptr |> Char.chr |> Printf.printf "%c"
  | InputChar    -> readChar () |> Array.set mem !ptr
  | Bracket body ->
     while Array.get env.memory !ptr != 0 do
       List.iter (step env) body
     done

let runProgram path =
  let program = Core.In_channel.read_all path |> parse in
  let env = {
      pointer = ref 0;
      (* As per the original BF spec, the max size was 30000 *)
      memory = Array.make 30000 0
    } in
  List.iter (step env) program

let () =
  match Sys.argv with
  | [| _; path |] -> runProgram path
  | _ -> Printf.printf "Usage: %s [PATH].bf" Sys.argv.(0)
