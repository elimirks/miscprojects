type command = PointerLeft
             | PointerRight
             | Increment
             | Decrement
             | OutputChar
             | Bracket of command list

type env = {
    pointer: int ref;
    memory: int array;
  }

let env = {
    pointer = ref 0;
    (* As per the original BF spec, the max size was 30000 *)
    memory = Array.make 30000 0
  }

let input = Core.In_channel.read_all "./hello.bf"

let stackParse str =
  let stack = ref [[]] in
  let f character =
    stack := match character with
    | '<' -> (PointerLeft  :: (List.hd !stack)) :: List.tl !stack
    | '>' -> (PointerRight :: (List.hd !stack)) :: List.tl !stack
    | '+' -> (Increment    :: (List.hd !stack)) :: List.tl !stack
    | '-' -> (Decrement    :: (List.hd !stack)) :: List.tl !stack
    | '.' -> (OutputChar   :: (List.hd !stack)) :: List.tl !stack
    | '[' -> [] :: !stack
    | ']' -> (
      match !stack with
      | x::y::xs -> (Bracket (List.rev x) :: y) :: xs
      | _        -> [] (* wat?! Horrible error handling :] *)
    )
    | _   -> !stack
  in
  for i = 0 to String.length str - 1 do
    f str.[i]
  done;
  List.hd !stack |> List.rev

let program = stackParse input

let rec step env c =
  let ptr = env.pointer
  and mem = env.memory
  in
  match c with
  | PointerLeft  ->
     if !ptr == 0 then
       ()
     else
       ptr := !ptr - 1
  | PointerRight -> ptr := !ptr + 1
  | Increment    -> Array.set mem !ptr (Array.get mem !ptr + 1)
  | Decrement    -> Array.set mem !ptr (Array.get mem !ptr - 1)
  | OutputChar   -> Printf.printf "%c" (Char.chr (Array.get env.memory !ptr))
  | Bracket body ->
     while (Array.get env.memory !ptr) != 0 do
       let _ = List.map (step env) body in
       ()
     done

let () =
  let _ = List.map (step env) program in
  ()
