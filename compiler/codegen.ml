open Compiler
open Instructions

let incrementInstructionCount (count:int) (instr:instruction) : int = 
    match instr with
    | Pushglobal _   -> count+2
    | Push _         -> count+2
    | Pushint _      -> count+2
    | Mkap           -> count+1
    | Unwind         -> count+1
    | Slide _        -> count+2
    | Jump _         -> count+2
    | Update _       -> count+2
    | Pop _          -> count+2
    | Alloc _        -> count+2
    | Eval           -> count+1
    | Add            -> count+1
    | Sub            -> count+1
    | Mul            -> count+1
    | Div            -> count+1
    | Neg            -> count+1
    | Eq             -> count+1
    | Ne             -> count+1
    | Le             -> count+1
    | Lt             -> count+1
    | Ge             -> count+1
    | Gt             -> count+1
    | Jfalse _       -> count+2
    | Label _        -> count+1
    | Pack _         -> count+3
    | Split _        -> count+2
    | Casejump ls    -> count+1+(List.length ls)
;;

let rec generateScEnv (compiledScs:compiledSc list) (instructionCount:int) : env =
  match compiledScs with
  | [] -> []
  | (name, args, body)::rest -> 
      (name, instructionCount) :: 
      (generateScEnv rest ((List.fold_left incrementInstructionCount instructionCount body)+1))
;;

let rec addLabelToEnv (count:int) (instruction:instruction) (labelEnv:env) =
  match instruction with
  | Label l -> (l, count) :: labelEnv
  | _ -> labelEnv
;;

let addLabelAndIncrement (countAndLabelEnv:int * env) (instruction:instruction) : int * env =
  let (count, labelEnv) = countAndLabelEnv in
  let labelEnv2 = addLabelToEnv count instruction labelEnv in
  (incrementInstructionCount count instruction, labelEnv2)
;;

let generateLocalLabelEnv (instructions:instruction list) (scIndex:int) : env =
  List.map 
    (fun (n, i) -> (n, scIndex+i+1))
    (snd (List.fold_left addLabelAndIncrement (0, []) instructions)) 
;;

let instructionToCode (instruction:instruction) (labelEnv:(string * int) list) : int list = 
  match instruction with
  | Pushglobal f -> [0; (lookup labelEnv f)]
  | Push n -> [1; n]
  | Pushint i -> [2; i]
  | Mkap -> [3]
  | Unwind -> [4]
  | Slide i -> [5; i]
  | Jump f -> [6; (lookup labelEnv f)]
  | Update n -> [7; n]
  | Pop i -> [8; i]
  | Alloc n -> [9; n]
  | Eval -> [10]
  | Add -> [11]
  | Sub -> [12]
  | Mul -> [13]
  | Div -> [14]
  | Neg -> [15]
  | Eq -> [16]
  | Ne -> [17]
  | Le -> [18]
  | Lt -> [19]
  | Ge -> [20]
  | Gt -> [21]
  | Jfalse l -> [22; (lookup labelEnv l)]
  | Label _ -> [23]
  | Pack (i, j) -> [24; i; j]
  | Split i -> [25; i]
  | Casejump ls -> [25] (*@ (enfunktion ls)*) 
;;

let rec codeGenerationHelper (compiledScs : compiledSc list) (scEnv : (string * int) list) =
  match compiledScs with
  | [] -> []
  | (name, args, body)::rest ->
      let localEnv = generateLocalLabelEnv body (lookup scEnv name) in
      Printf.fprintf stdout "SC name: %s\n" name;
      printEnv localEnv;
      args :: List.flatten (List.map (fun inst -> instructionToCode inst (localEnv @ scEnv)) body) @
      codeGenerationHelper rest scEnv
;;

let codeGeneration (compiledScs : compiledSc list) : int list =
  let scEnv = generateScEnv compiledScs
                 (incrementInstructionCount (incrementInstructionCount (incrementInstructionCount 0 Unwind) (Pushglobal "main")) Eval) in
  printEnv scEnv;
  (instructionToCode Unwind scEnv)
  @ (instructionToCode (Pushglobal "main") scEnv)
  @ (instructionToCode Eval scEnv)
  @ codeGenerationHelper compiledScs scEnv
;;
