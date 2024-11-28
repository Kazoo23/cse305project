type stackValue = BOOL of bool | INT of int | ERROR | STRING of string | NAME of string

type command = QUIT | ADD | SUB | MUL | DIV | PUSH of stackValue

let interpreter (input,output) = 
    let ic = open_in input
    in 

    let oc = open_out output
    in 

    let rec loop_read acc = 
        try
            let l = String.trim(input_line ic) in loop_read (l::acc)
        with
        | End_of_file -> List.rev acc
    in 

    let strList = loop_read []
    in

    let str2sv s = 
        match s with
        | "False" -> BOOL false
        | "True" -> BOOL true
        | "Error" -> ERROR
        | _ -> 
            if( String.get s 0 >= '0' || String.get s 0 <= '9') then
                INT (int_of_string s)
            else if (String.get s 0)  == '\"' then
                STRING s
            else
                NAME s
    in

    let str2com s = 
        match s with
        |"Add" -> ADD
        | "Sub" -> SUB
        | "Mul" -> MUL
        | "Div" -> DIV
        | "Quit" -> QUIT
        | _ -> PUSH (str2sv (String.sub s 6 (String.length(s)-6)))
    in 

    let sv2str sv =
        match sv with
        | INT i -> string_of_int(i)
        | STRING s -> s 
        | NAME n -> n
        | ERROR -> "Error"
        | BOOL b -> match b with
            |true -> "True"
            |false -> "False"
    in

    let com2str c =
        match c with
        | ADD -> "Add"
        | SUB -> "Sub"
        | MUL -> "Mul"
        | DIV -> "Div"
        | QUIT -> "Quit"
        | PUSH x -> "Push" ^ sv2str(x)
    in

    let rec map sl = 
        match sl with
        | head::rest -> str2com(head) :: map (rest)
        | [] -> []
        in


    let comList = map strList
in 

    let rec processor cl stack =
        match (cl,stack) with
        | (ADD::restOfCommands,INT(a)::INT(b)::restOfStack) -> processor restOfCommands (INT(a+b)::restOfStack)
        | (ADD::restOfCommands,s) -> processor restOfCommands (ERROR::stack)
        | (SUB::restOfCommands,INT(a)::INT(b)::restOfStack) -> processor restOfCommands (INT(a-b)::restOfStack)
        | (SUB::restOfCommands,s) -> processor restOfCommands (ERROR::stack)
        | (MUL::restOfCommands,INT(a)::INT(b)::restOfStack) -> processor restOfCommands (INT(a*b)::restOfStack)
        | (MUL::restOfCommands,s) -> processor restOfCommands (ERROR::stack)
        | (DIV::restOfCommands,INT(a)::INT(b)::restOfStack) -> processor restOfCommands (INT(a/b)::restOfStack)
        | (DIV::restOfCommands,s) -> processor restOfCommands (ERROR::stack)
        | (PUSH p ::restOfCommands,s) -> processor restOfCommands (p::stack)
        | (QUIT::[],s) -> stack
        | (_,_) -> stack
    in

    let rec outputstr2file list = 
        match list with
        | x :: xs -> output_string oc (sv2str x); outputstr2file xs
        | [] -> ()
    in

    let stackList = processor comList []
    in

    outputstr2file stackList;

    close_in ic;

    close_out oc;

