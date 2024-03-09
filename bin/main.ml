let addition_tests = [
    Big_int.eq (Big_int.add (Big_int.of_int 1) (Big_int.of_int 1)) (Big_int.of_int 2);
    Big_int.eq (Big_int.add [1l; 1l] [1l]) [1l; 2l];
]
let () = List.fold_left (fun acc x -> if x then acc else raise (Failure "addition_tests")) true addition_tests |> ignore
