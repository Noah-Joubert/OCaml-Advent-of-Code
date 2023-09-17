(*-------------=+ Part 1 +=-------------*)
let read_file (filename : string) : int list = 
  let lines = ref [] in
  let channel = open_in filename in
  
  try
    while true; do
      lines := 
        (let line = input_line channel
        in if line = "" then 0 else int_of_string line ):: !lines
    done; !lines
  with End_of_file ->
    close_in channel;
    List.rev !lines ;;

let groupElements (lines: int list) : int list list =
  let rec aux groupedLists currentList remaining = 
    match remaining with
    | [] -> currentList::groupedLists
    | 0::t -> 
      let groupedLists' = 
        if currentList = [] then groupedLists 
        else currentList::groupedLists
      in aux groupedLists' [] t
    | h::t -> let currentList' = h::currentList 
        in aux groupedLists currentList' t
  in aux [] [] lines;;

let sum (nums: int list) : int = 
  List.fold_left (fun acc a -> acc + a) 0 nums;;

let maxElement (nums: int list) : int = 
  List.fold_left (fun acc a -> max acc a) 0 nums ;;


let fileInput = read_file "1.txt";;
let groupedElements = groupElements fileInput;;
let calories = List.map sum groupedElements;;
"Part 1: ";;
maxElement calories;;


(*-------------=+ Part 2 +=-------------*)
let rec first_n nums n = 
  let rec aux accumulator numsRemaining m = 
    if m == 0 then accumulator else 
    match numsRemaining with
    | [] -> []
    | h::t -> 
      let accumulator' = if m = 0 then accumulator else h::accumulator
      in aux accumulator' t (m-1)
  in List.rev (aux [] nums n);;

let orderedCalories = List.rev (List.sort compare calories);;
let topThree = first_n orderedCalories 3;;
"Part 2: ";;
sum topThree;;