type command = PointerLeft
             | PointerRight
             | Increment
             | Decrement
             | OutputChar
             | InputChar

type env = {
    pointer: int;
    memory: int array;
  }

let env = {
    pointer = 0;
    memory = Array.make 30000 0
  }

let input = Core.In_channel.read_all "./hello.bf"

let rec parse str index =
  if index >= String.length str then
    []
  else
    match String.get str index with
    | '>' -> PointerLeft  :: parse str (index + 1)
    | '<' -> PointerRight :: parse str (index + 1)
    | '+' -> Increment    :: parse str (index + 1)
    | '-' -> Decrement    :: parse str (index + 1)
    | '.' -> OutputChar   :: parse str (index + 1)
    | ',' -> InputChar    :: parse str (index + 1)
    | _   -> []

let program = parse input 0

let run c env =
  match c with
  | PointerLeft ->
     env.pointer = env.pointer - 1
  | PointerRight ->
     env.pointer = env.pointer + 1
  | Increment ->
     Array.set env.memory env.pointer
       (Array.get env.memory env.pointer + 1)
  | Decrement ->
     Array.set env.memory env.pointer
       (Array.get env.memory env.pointer - 1)
  | _   -> ()

let () = print_endline input
