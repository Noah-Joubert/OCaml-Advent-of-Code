(*-------------=+ Part 1 +=-------------*)
let read_file (filename : string) : string list = 
  let lines = ref [] in
  let channel = open_in filename in
  
  try
    while true; do
      lines := (input_line channel)::!lines
    done; !lines
  with End_of_file ->
    close_in channel;
    List.rev !lines ;;

let headSize = 8;;
let numStacks = 9;;
let stacks = Array.init numStacks (fun _ -> ref[]);;

let splitInputToLists (fileInput : string list) = 
  (* Read horizontally *)
  let rec goRight (remainingString: string) (j: int) =
    match remainingString with 
    | "" -> []
    | x ->
      let c = String.get x 0 in
      let s = String.sub x 1 (String.length x - 1) in
      if (j - 1) mod 4 == 0 then c::(goRight s (j + 1)) 
      else goRight s (j + 1)       

  in let rec goDown (remainingStrings: string list) (i: int) =
    match remainingStrings with 
    | [] -> []
    | h::t -> 
      if i == headSize then [] else (goRight h 0) :: goDown t (i + 1)
  in goDown fileInput 0
let transposeListsToArr (listInput : char list list) = 
  let rec append (index : int) (c : char)  =
    let prevArr = !(stacks.(index)) in
    stacks.(index) := if c != ' ' then c::prevArr else prevArr
  in
  for i = 0 to headSize - 1 do
    for j = 0 to numStacks - 1 do
      append j (List.nth (List.nth listInput (headSize - 1 - i)) j)
    done
  done;;
let print_char_list_ref_array array =
  (* Thanks ChatGPT *)
  Array.iteri (fun index list_ref ->
    Printf.printf "List %d: [" (index + 1);
    let list_content = !list_ref in
    List.iter (fun value -> Printf.printf "%c; " value) list_content;
    Printf.printf "]\n"
  ) array
let getCommands (rawInput : string list) = 
  let rec aux remInput count = 
    match remInput with
    | [] -> []
    | h::t -> 
      if count > 9 then h::(aux t (count + 1)) else aux t (count + 1)
  in aux rawInput 0
let executeCommands (commands : string list list) = 
  let executeCommand (command : string list) = 
    let fromIndex = int_of_string(List.nth command 3) - 1 in
    let toIndex = int_of_string(List.nth command 5) -1 in
    let toAdd = ref[] in

    for i = 1 to int_of_string(List.nth command 1) do
      let newElement = List.nth !(stacks.(fromIndex)) 0 in
      let newFromList =
        match !(stacks.(fromIndex)) with 
        | h::t -> t 
        | x -> [] in
      (stacks.(fromIndex)) := newFromList;
      toAdd := newElement::!toAdd
    done;

    for i = 0 to (List.length !toAdd) - 1 do
      (stacks.(toIndex)) := (List.nth !toAdd i)::!(stacks.(toIndex))
    done in

  List.iter (executeCommand) commands;;

let fileInput = read_file "5.txt";;
let stacksAsArr = splitInputToLists fileInput;;
transposeListsToArr stacksAsArr;;
let otherStacks = stacks;;
let rawCommands = getCommands fileInput;;
let commands =  List.map (String.split_on_char ' ') rawCommands;;
let otherStacks = stacks;;
executeCommands commands;;
print_char_list_ref_array stacks
