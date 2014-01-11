namespace BitcoinFs
open System.Text

type Op =
    // Constants
    | Op_False                              //0   0x00    Nothing.    (empty value)   An empty array of bytes is pushed onto the stack. (This is not a no-op: an item is added to the stack.)
    | Op_PushData of IVector * array<byte>   //N/A     1-75    0x01-0x4b   (special)   data    The next opcode bytes is data to be pushed onto the stack
    | Op_PushData1 of IVector * array<byte>  //76  0x4c    (special)   data    The next byte contains the number of bytes to be pushed onto the stack.
    | Op_PushData2 of IVector * array<byte>  //77  0x4d    (special)   data    The next two bytes contain the number of bytes to be pushed onto the stack.
    | Op_PushData4 of IVector * array<byte>  //78  0x4e    (special)   data    The next four bytes contain the number of bytes to be pushed onto the stack.
    | Op_1Negate                            //79  0x4f    Nothing.    -1  The number -1 is pushed onto the stack.
    | OP_True                               //81  0x51    Nothing.    1   The number 1 is pushed onto the stack.
    | OP_PushConst of IVector                  //82-96   0x52-0x60   Nothing.    2-16    The number in the word name (2-16) is pushed onto the stack. 
    //Flow control
    | Op_NOp                                //97  0x61    Nothing     Nothing     Does nothing.
    | Op_RShift                             //99  0x63    <expression> if [statements] [else [statements]]* endif     If the top stack value is not 0, the statements are executed. The top stack value is removed.
    | Op_NotIf                              //100     0x64    <expression> if [statements] [else [statements]]* endif     If the top stack value is 0, the statements are executed. The top stack value is removed.
    | Op_Else                               //103     0x67    <expression> if [statements] [else [statements]]* endif     If the preceding OP_IF or OP_NOTIF or OP_ELSE was not executed then these statements are and if the preceding OP_IF or OP_NOTIF or OP_ELSE was executed then these statements are not.
    | Op_EndIf                              //104     0x68    <expression> if [statements] [else [statements]]* endif     Ends an if/else block.
    | Op_Verify                             //105     0x69    True / false    Nothing / False     Marks transaction as invalid if top stack value is not true. True is removed, but false is not.
    | Op_Return                             //106     0x6a    Nothing     Nothing     Marks transaction as invalid.
    // Stack
    | Op_ToAltStack                         //107     0x6b    x1  (alt)x1     Puts the input onto the top of the alt stack. Removes it from the main stack.
    | Op_FromAltStack                       //108     0x6c    (alt)x1     x1  Puts the input onto the top of the main stack. Removes it from the alt stack.
    | Op_IfDup                              //115     0x73    x   x / x x     If the top stack value is not 0, duplicate it.
    | Op_Depth                              //116     0x74    Nothing     <Stack size>    Puts the number of stack items onto the stack.
    | Op_Drop                               //117     0x75    x   Nothing     Removes the top stack item.
    | Op_Dup                                //118     0x76    x   x x     Duplicates the top stack item.
    | Op_Nip                                //119     0x77    x1 x2   x2  Removes the second-to-top stack item.
    | Op_Over                               //120     0x78    x1 x2   x1 x2 x1    Copies the second-to-top stack item to the top.
    | Op_Pick                               //121     0x79    xn ... x2 x1 x0 <n>     xn ... x2 x1 x0 xn  The item n back in the stack is copied to the top.
    | Op_Roll                               //122     0x7a    xn ... x2 x1 x0 <n>     ... x2 x1 x0 xn     The item n back in the stack is moved to the top.
    | Op_Rot                                //123     0x7b    x1 x2 x3    x2 x3 x1    The top three items on the stack are rotated to the left.
    | Op_Swap                               //124     0x7c    x1 x2   x2 x1   The top two items on the stack are swapped.
    | Op_Tuck                               //125     0x7d    x1 x2   x2 x1 x2    The item at the top of the stack is copied and inserted before the second-to-top item.
    | Op_2Drop                              //109     0x6d    x1 x2   Nothing     Removes the top two stack items.
    | Op_2Dup                               //110     0x6e    x1 x2   x1 x2 x1 x2     Duplicates the top two stack items.
    | Op_3Dup                               //111     0x6f    x1 x2 x3    x1 x2 x3 x1 x2 x3   Duplicates the top three stack items.
    | Op_2Over                              //112     0x70    x1 x2 x3 x4     x1 x2 x3 x4 x1 x2   Copies the pair of items two spaces back in the stack to the front.
    | Op_2Rot                               //113     0x71    x1 x2 x3 x4 x5 x6   x3 x4 x5 x6 x1 x2   The fifth and sixth items back are moved to the top of the stack.
    | Op_2Swap                              //114     0x72    x1 x2 x3 x4     x3 x4 x1 x2     Swaps the top two pairs of items.
    //Splice
    | Op_Cat                                //126     0x7e    x1 x2   out     Concatenates two strings. disabled.
    | Op_SubStr                             //127     0x7f    in begin size   out     Returns a section of a string. disabled.
    | Op_Left                               //128     0x80    in size     out     Keeps only characters left of the specified point in a string. disabled.
    | Op_Right                              //129     0x81    in size     out     Keeps only characters right of the specified point in a string. disabled.
    | Op_Size                               //130     0x82    in  in size     Returns the length of the input string.
    //Bitwise logic
    | Op_Invert                             //131     0x83    in  out     Flips all of the bits in the input. disabled.
    | Op_And                                //132     0x84    x1 x2   out     Boolean and between each bit in the inputs. disabled.
    | Op_Or                                 //133     0x85    x1 x2   out     Boolean or between each bit in the inputs. disabled.
    | Op_Xor                                //134     0x86    x1 x2   out     Boolean exclusive or between each bit in the inputs. disabled.
    | Op_Equal                              //135     0x87    x1 x2   True / false    Returns 1 if the inputs are exactly equal, 0 otherwise.
    | Op_EqualVerify                        //136     0x88    x1 x2   True / false    Same as OP_EQUAL, but runs OP_VERIFY afterward.
    //Arithmetic
    | Op_1Add                               //139     0x8b    in  out     1 is added to the input.
    | Op_1Sub                               //140     0x8c    in  out     1 is subtracted from the input.
    | Op_2Mul                               //141     0x8d    in  out     The input is multiplied by 2. disabled.
    | Op_2Div                               //142     0x8e    in  out     The input is divided by 2. disabled.
    | Op_Negate                             //143     0x8f    in  out     The sign of the input is flipped.
    | Op_Abs                                //144     0x90    in  out     The input is made positive.
    | Op_Not                                //145     0x91    in  out     If the input is 0 or 1, it is flipped. Otherwise the output will be 0.
    | Op_0NotEqual                          //146     0x92    in  out     Returns 0 if the input is 0. 1 otherwise.
    | Op_Add                                //147     0x93    a b     out     a is added to b.
    | Op_Sub                                //148     0x94    a b     out     b is subtracted from a.
    | Op_Mul                                //149     0x95    a b     out     a is multiplied by b. disabled.
    | Op_Div                                //150     0x96    a b     out     a is divided by b. disabled.
    | Op_Mod                                //151     0x97    a b     out     Returns the remainder after dividing a by b. disabled.
    | Op_BitLShift                          //152     0x98    a b     out     Shifts a left b bits, preserving sign. disabled.
    | Op_BitRShift                          //153     0x99    a b     out     Shifts a right b bits, preserving sign. disabled.
    | Op_BoolAnd                            //154     0x9a    a b     out     If both a and b are not 0, the output is 1. Otherwise 0.
    | Op_BoolOr                             //155     0x9b    a b     out     If a or b is not 0, the output is 1. Otherwise 0.
    | Op_NumEqual                           //156     0x9c    a b     out     Returns 1 if the numbers are equal, 0 otherwise.
    | Op_NumEqualVefify                     //157     0x9d    a b     out     Same as OP_NUMEQUAL, but runs OP_VERIFY afterward.
    | Op_NumNotEqual                        //158     0x9e    a b     out     Returns 1 if the numbers are not equal, 0 otherwise.
    | Op_LessThan                           //159     0x9f    a b     out     Returns 1 if a is less than b, 0 otherwise.
    | Op_GreaterThan                        //160     0xa0    a b     out     Returns 1 if a is greater than b, 0 otherwise.
    | Op_LessThanOrEqual                    //161     0xa1    a b     out     Returns 1 if a is less than or equal to b, 0 otherwise.
    | Op_GreaterThanOrEqual                 //162     0xa2    a b     out     Returns 1 if a is greater than or equal to b, 0 otherwise.
    | Op_Min                                //163     0xa3    a b     out     Returns the smaller of a and b.
    | Op_Max                                //164     0xa4    a b     out     Returns the larger of a and b.
    | Op_Within                             //165     0xa5    x min max   out     Returns 1 if x is within the specified range (left-inclusive), 0 otherwise.
    //Crypto
    | Op_RIPEMD160                          //166     0xa6    in  hash    The input is hashed using RIPEMD-160.
    | Op_SHA1                               //167     0xa7    in  hash    The input is hashed using SHA-1.
    | Op_SHA256                             //168     0xa8    in  hash    The input is hashed using SHA-256.
    | Op_HASH160                            //169     0xa9    in  hash    The input is hashed twice: first with SHA-256 and then with RIPEMD-160.
    | Op_HASH256                            //170     0xaa    in  hash    The input is hashed two times with SHA-256.
    | Op_CodeSeparator                      //171     0xab    Nothing     Nothing     All of the signature checking words will only match signatures to the data after the most recently-executed OP_CODESEPARATOR.
    | Op_CheckSig                           //172     0xac    sig pubkey  True / false    The entire transaction's outputs, inputs, and script (from the most recently-executed OP_CODESEPARATOR to the end) are hashed. The signature used by OP_CHECKSIG must be a valid signature for this hash and public key. If it is, 1 is returned, 0 otherwise.
    | Op_CheckSigVerify                     //173     0xad    sig pubkey  True / false    Same as OP_CHECKSIG, but OP_VERIFY is executed afterward.
    | Op_CheckMultisig                      //174     0xae    x sig1 sig2 ... <number of signatures> pub1 pub2 <number of public keys>    True / False    For each signature and public key pair, OP_CHECKSIG is executed. If more public keys than signatures are listed, some key/sig pairs can fail. All signatures need to match a public key. If all signatures are valid, 1 is returned, 0 otherwise. Due to a bug, one extra unused value is removed from the stack.
    | Op_CheckMultisigVerify                //175     0xaf    x sig1 sig2 ... <number of signatures> pub1 pub2 ... <number of public keys>    True / False    Same as OP_CHECKMULTISIG, but OP_VERIFY is executed afterward.
    //Pseudo-words
    | Op_PubkeyHash                         //253     0xfd    Represents a public key hashed with OP_HASH160.
    | Op_Pubkey                             //254     0xfe    Represents a public key compatible with OP_CHECKSIG.
    | Op_InvalidOpcode of byte              //255     0xff    Matches any opcode that is not yet assigned.
    //Reserved words
    | Op_Reserved                           //80  0x50    Transaction is invalid unless occuring in an unexecuted OP_IF branch
    | Op_Ver                                //98  0x62    Transaction is invalid unless occuring in an unexecuted OP_IF branch
    | Op_VerIf                              //101     0x65    Transaction is invalid even when occuring in an unexecuted OP_IF branch
    | Op_VerNofIf                           //102     0x66    Transaction is invalid even when occuring in an unexecuted OP_IF branch
    | Op_Reserved1                          //137     0x89    Transaction is invalid unless occuring in an unexecuted OP_IF branch
    | Op_Reserved2                          //138     0x8a    Transaction is invalid unless occuring in an unexecuted OP_IF branch
    | Op_NOpx                               //176-185    0xb0-0xb9   The word is ignored.

// small subset of ops that appear in canonical scripts
type CanonicalOutputScript =
  | PayToPublicKey of string
  | PayToAddress of string

module ScriptParser =

    let opCodeTable =
        [ (fun () -> Op_False),              0uy
          (fun () -> Op_1Negate),            79uy
          (fun () -> OP_True),               81uy
          //Flow control
          (fun () -> Op_NOp),                97uy
          (fun () -> Op_RShift),             99uy
          (fun () -> Op_NotIf),              100uy
          (fun () -> Op_Else),               103uy
          (fun () -> Op_EndIf),              104uy
          (fun () -> Op_Verify),             105uy
          (fun () -> Op_Return),             106uy
          // Stack
          (fun () -> Op_ToAltStack),         107uy
          (fun () -> Op_FromAltStack),       108uy
          (fun () -> Op_IfDup),              115uy
          (fun () -> Op_Depth),              116uy
          (fun () -> Op_Drop),               117uy
          (fun () -> Op_Dup),                118uy
          (fun () -> Op_Nip),                119uy
          (fun () -> Op_Over),               120uy
          (fun () -> Op_Pick),               121uy
          (fun () -> Op_Roll),               122uy
          (fun () -> Op_Rot),                123uy
          (fun () -> Op_Swap),               124uy
          (fun () -> Op_Tuck),               125uy
          (fun () -> Op_2Drop),              109uy
          (fun () -> Op_2Dup),               110uy
          (fun () -> Op_3Dup),               111uy
          (fun () -> Op_2Over),              112uy
          (fun () -> Op_2Rot),               113uy
          (fun () -> Op_2Swap),              114uy
          //Splice
          (fun () -> Op_Cat),                126uy
          (fun () -> Op_SubStr),             127uy
          (fun () -> Op_Left),               128uy
          (fun () -> Op_Right),              129uy
          (fun () -> Op_Size),               130uy
          //Bitwise logic
          (fun () -> Op_Invert),             131uy
          (fun () -> Op_And),                132uy
          (fun () -> Op_Or),                 133uy
          (fun () -> Op_Xor),                134uy
          (fun () -> Op_Equal),              135uy
          (fun () -> Op_EqualVerify),        136uy
          //Arithmetic
          (fun () -> Op_1Add),               139uy
          (fun () -> Op_1Sub),               140uy
          (fun () -> Op_2Mul),               141uy
          (fun () -> Op_2Div),               142uy
          (fun () -> Op_Negate),             143uy
          (fun () -> Op_Abs),                144uy
          (fun () -> Op_Not),                145uy
          (fun () -> Op_0NotEqual),          146uy
          (fun () -> Op_Add),                147uy
          (fun () -> Op_Sub),                148uy
          (fun () -> Op_Mul),                149uy
          (fun () -> Op_Div),                150uy
          (fun () -> Op_Mod),                151uy
          (fun () -> Op_BitLShift),          152uy
          (fun () -> Op_BitRShift),          153uy
          (fun () -> Op_BoolAnd),            154uy
          (fun () -> Op_BoolOr),             155uy
          (fun () -> Op_NumEqual),           156uy
          (fun () -> Op_NumEqualVefify),     157uy
          (fun () -> Op_NumNotEqual),        158uy
          (fun () -> Op_LessThan),           159uy
          (fun () -> Op_GreaterThan),        160uy
          (fun () -> Op_LessThanOrEqual),    161uy
          (fun () -> Op_GreaterThanOrEqual), 162uy
          (fun () -> Op_Min),                163uy
          (fun () -> Op_Max),                164uy
          (fun () -> Op_Within),             165uy
          //Crypto
          (fun () -> Op_RIPEMD160),          166uy
          (fun () -> Op_SHA1),               167uy
          (fun () -> Op_SHA256),             168uy
          (fun () -> Op_HASH160),            169uy
          (fun () -> Op_HASH256),            170uy
          (fun () -> Op_CodeSeparator),      171uy  
          (fun () -> Op_CheckSig),           172uy  
          (fun () -> Op_CheckSigVerify),     173uy  
          (fun () -> Op_CheckMultisig),      174uy  
          (fun () -> Op_CheckMultisigVerify),175uy  
          //Pseudo-words
          (fun () -> Op_PubkeyHash),         253uy  
          (fun () -> Op_Pubkey),             254uy  
          //Reserved words
          (fun () -> Op_Reserved),           80uy   
          (fun () -> Op_Ver),                98uy   
          (fun () -> Op_VerIf),              101uy  
          (fun () -> Op_VerNofIf),           102uy  
          (fun () -> Op_Reserved1),          137uy  
          (fun () -> Op_Reserved2),          138uy ]
        |> Seq.map (fun (x,y) -> y,x) // realized too late I'd build the table the wrong way round ...
        |> Map.ofSeq


    let parseScript (script: array<byte>)  =
        let makePushDataOp index (v: IVector) opConst =
            let nextIndex = index + 1
            //printfn "script.Length %i nextIndex %i (nextIndex + v.AsInt32 - 1) %i" script.Length nextIndex (nextIndex + v.AsInt32 - 1)
            opConst(v, script.[nextIndex .. nextIndex + v.AsInt32 - 1]), nextIndex + v.AsInt32
        let rec innerParse index acc =
            //printfn "%A" acc
            if index >= script.Length then acc
            else
                //printfn "index %i opcode %i" index script.[index]
                match script.[index] with
                | x when opCodeTable.ContainsKey(x) -> innerParse (index + 1) (opCodeTable.[x]() :: acc)
                | x when 1uy <= x && x <= 75uy -> 
                    let v = Vector(x)
                    let op, nextIndex = makePushDataOp index v (fun (v, data) -> Op_PushData(v, data))
                    innerParse nextIndex (op :: acc)
                | x when x = 76uy -> 
                    let nextIndex = index + 1
                    let bytes = script.[nextIndex]
                    let v = Vector(bytes) :> IVector
                    let op, nextIndex = makePushDataOp nextIndex v (fun (v, data) -> Op_PushData1(v, data))
                    innerParse nextIndex (op :: acc)
                | x when x = 77uy -> 
                    let v = Vector2(script.[index + 1], script.[index + 2]) :> IVector
                    let op, nextIndex = makePushDataOp (index + 2) v (fun (v, data) -> Op_PushData2(v, data))
                    innerParse nextIndex (op :: acc)
                | x when x = 78uy -> 
                    let v = Vector4(script.[index + 1], script.[index + 2], script.[index + 3], script.[index + 4]) :> IVector
                    let op, nextIndex = makePushDataOp (index + 4) v (fun (v, data) -> Op_PushData4(v, data))
                    innerParse nextIndex (op :: acc)
                | x when 82uy <= x && x <= 96uy -> innerParse (index + 1) (OP_PushConst(Vector(x - 80uy) :> IVector) :: acc)
                | x when 176uy <= x && x <= 185uy -> innerParse (index + 1) (Op_NOpx :: acc)
                | x -> innerParse (index + 1) (Op_InvalidOpcode x :: acc) // spec if a little unclear if just 255 is Op_InvalidOpcode, or all other unassigned
        innerParse 0 [] |> List.rev

    let bytesToHexString data =
        let builder = new StringBuilder()
        for b in data do
            builder.Append(sprintf "%x" b) |> ignore
        builder.ToString()

    let (|PushData|_|) op =
        match op with
        | Op_PushData (_, data) -> Some (bytesToHexString data)
        | Op_PushData1 (_, data) -> Some (bytesToHexString data)
        | Op_PushData2 (_, data) -> Some (bytesToHexString data)
        | Op_PushData4 (_, data) -> Some (bytesToHexString data)
        | _ -> None

    let (|PayToPublicKeyPattern|_|) ops =
        match ops with
        | [| op; Op_CheckSig |] ->
            match op with
            | PushData data -> Some (PayToPublicKey data)
            | _ -> None
        | _ -> None

    let (|PayToAddressPattern|_|) ops =
        match ops with
        | [| Op_Dup; Op_HASH160; op; Op_EqualVerify; Op_CheckSig |] ->
            match op with
            | PushData data -> Some (PayToAddress data)
            | _ -> None
        | _ -> None

    let parseStandardOutputScript script =
        match script with
        | PayToPublicKeyPattern script -> Some(script)
        | PayToAddressPattern script -> Some(script)
        | _ -> None