type bigint = int32 list

let of_int (n: int) : bigint = [Int32.of_int n]
let of_int32 (n: int32) : bigint = [n]
let of_int64 (n: int64) : bigint = [Int64.to_int32 n]
let rec to_string (n: bigint) : string = 
    match n with
    | [] -> ""
    | [n] -> Int32.to_string n
    | n::rst -> (Int32.to_string n) ^ (to_string rst)

let rec eq (x: bigint) (y: bigint) : bool = 
    match x, y with
    | [], [] -> true
    | [], _ -> false
    | _, [] -> false
    | hx::tx, hy::ty -> if hx = hy then eq tx ty else false

let add (x: bigint) (y: bigint) : bigint = 
    let rec add' (x: bigint) (y: bigint) (carry: int32) : bigint =
        match x, y with
        | [], [] -> if carry = 0l then [] else [carry]
        | [], _ -> add' [0l] y carry 
        | _, [] -> add' x [0l] carry
        | hx::tx, hy::ty -> 
            let x64 = Int64.of_int32 hx in 
            let y64 = Int64.of_int32 hy in
            let c64 = Int64.of_int32 carry in
            let sum = Int64.add (Int64.add x64 y64) c64 in
            let new_carry = if sum > Int64.of_int max_int then Int64.sub sum (Int64.of_int max_int) else 0L in
            (Int64.to_int32 sum)::(add' tx ty (Int64.to_int32 new_carry))
    in List.rev (add' x y 0l)
