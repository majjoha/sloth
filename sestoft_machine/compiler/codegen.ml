open Compiler
open Instructions
open Environment

let ext = ".sls"
;;

let incrementInstructionCount (count:int) (instr:instruction) : int =
  match instr with
  | Take          -> count+1
  | Enter _       -> count+2
  | Push _        -> count+2
  | Sepcase       -> count+1
  | Let _         -> count+2
  | Enterglobal _ -> count+2
  | Pushglobal _  -> count+2
  | Freevars s    -> count+(List.length s)+1
  | Case _        -> count+2
  | Pack _        -> count+3
  | CstI _        -> count+2
  | Add           -> count+1
  | Sub           -> count+1
  | Mul           -> count+1
  | Div           -> count+1
  | Lt            -> count+1
  | Gt            -> count+1
  | Le            -> count+1
  | Ge            -> count+1
  | Eq            -> count+1
  | Neg           -> count+1
  | Seplet       -> count+1
;;

let instructionToCode (instruction:instruction) (labelEnv:env) : int list =
  match instruction with
  | Take -> [0]
  | Enter n -> [1; n]
  | Push n -> [2; n]
  | Sepcase -> [-3]
  | Let i -> [4; i]
  | Enterglobal s -> [5; (lookup labelEnv s)]
  | Pushglobal s -> [6; (lookup labelEnv s)]
  | Freevars s -> List.length s :: s
  | Case i -> [7; i]
  | Pack (t, a) -> [8; t; a]
  | CstI i -> [9; i]
  | Add    -> [10]
  | Sub    -> [11]
  | Mul    -> [12]
  | Div    -> [13]
  | Lt     -> [14]
  | Gt     -> [15]
  | Le     -> [16]
  | Ge     -> [17]
  | Eq     -> [18]
  | Neg    -> [19]
  | Seplet -> [-20]
;;

(*
instructionCount: offset from beginning of program array to first supercombinator
*)
let rec generateScEnv (compiledScs:compiledSc list) (instructionCount:int) : env =
  match compiledScs with
  | [] -> []
  | (name, args, body)::rest -> 
      (name, instructionCount) :: 
      (generateScEnv rest ((List.fold_left incrementInstructionCount instructionCount body)))
;;

let rec codeGenerationHelper (compiledScs:compiledSc list) (scEnv:env) =
  match compiledScs with
  | [] -> []
  | (name, args, body)::rest ->
      List.flatten (List.map (fun inst -> instructionToCode inst scEnv) body) @
      codeGenerationHelper rest scEnv
;;

let getMainDeBruijn (compiledScs:compiledSc list) : int =
  lookup (List.mapi (fun i (name, _, _) -> (name, i)) compiledScs) "main"
;;

let codeGeneration (compiledScs:compiledSc list) : int list =
  let scsCount = List.length compiledScs in
  let env = generateScEnv compiledScs (scsCount+12) in
  printEnv env;
  (instructionToCode (Pack (1, 1)) env) @ 
  instructionToCode (Pack(1,0)) env @
  instructionToCode (Pack(2,0)) env @
  [scsCount] @ (List.rev (List.map (fun (s, i) -> i) env)) @
  (instructionToCode (Enter (getMainDeBruijn compiledScs))) env @ codeGenerationHelper compiledScs env
;;
