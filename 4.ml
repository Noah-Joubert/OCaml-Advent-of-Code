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

let listOfString (stringInput: string) : int list list =
  let rec aux (returnList : int list list) (currentList : int list) (currentString : string) (stringRemaining : string) =
    match stringRemaining with 
    | "" -> (int_of_string currentString::currentList)::returnList
    | x -> 
      let stringRemaining' = String.sub stringRemaining 1 (String.length stringRemaining - 1) in
      let c = String.get x 0 in
      match c with 
      | '-' -> aux returnList (int_of_string currentString::currentList) "" stringRemaining'
      | ',' -> aux ((int_of_string currentString::currentList)::returnList) [] "" stringRemaining'
      | c -> aux returnList currentList (String.cat currentString (Char.escaped c)) stringRemaining'
  in aux [] [] "" stringInput;;

let oneInsideOther (intervals : int list list) : int = 
  match intervals with 
  | (a1::a2::_)::(b1::b2::_)::_ -> 
    if 
      (((b2 <= a1) && (a1 <= b1)) && (((b2 <= a2) && (a2 <= b1)))) || 
      (((a2 <= b1) && (b1 <= a1)) && (((a2 <= b2) && (b2 <= a1)))) 
    then 1 else 0
  | _ -> 0
    
  
let fileInput = read_file "4.txt";;
let splitInput = List.map listOfString fileInput;;
let inside = List.map oneInsideOther splitInput;;
List.fold_left (+) 0 inside;;

(*-------------=+ Part 2 +=-------------*)
let overlap (intervals : int list list) : int = 
  match intervals with 
  | (a1::a2::_)::(b1::b2::_)::_ -> 
    if 
      (((b2 <= a1) && (a1 <= b1)) || (((b2 <= a2) && (a2 <= b1)))) || 
      (((a2 <= b1) && (b1 <= a1)) || (((a2 <= b2) && (b2 <= a1)))) 
    then 1 else 0
  | _ -> 0

let overlapping = List.map overlap splitInput;;
List.fold_left (+) 0 overlapping;;