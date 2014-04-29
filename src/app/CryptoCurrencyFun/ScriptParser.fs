namespace CryptoCurrencyFun
open System.Text

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
          (fun () -> Op_Reserved2),          138uy
          (fun () -> Op_InvalidOpcode),      255uy ]
        |> Seq.map (fun (x,y) -> y,x) // realized too late I'd build the table the wrong way round ...
        |> Map.ofSeq


    let private parseScriptImp (script: array<byte>) =
        let makePushDataOp index (v: IVector) opConst =
            let nextIndex = index + 1
            opConst(v, script.[nextIndex .. nextIndex + v.AsInt32 - 1]), nextIndex + v.AsInt32
        let rec innerParse index acc =
            if index >= script.Length then acc
            else
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
                | x when 176uy <= x && x <= 185uy -> innerParse (index + 1) (Op_NOpx :: acc)
                | x -> failwithf "invalid opcode 0x%x" x 
        innerParse 0 [] |> List.rev

    let parseScript (script: array<byte>) =
        try
            let script = parseScriptImp script |> List.toArray
            Some script
        with ex ->
            // TODO would be nice to log the exception somewhere, but not sure where is best
            None

    let (|PushData|_|) op =
        match op with
        | Op_PushData (_, data) -> Some data
        | Op_PushData1 (_, data) -> Some data
        | Op_PushData2 (_, data) -> Some data
        | Op_PushData4 (_, data) -> Some data
        | _ -> None

    let (|PayToPublicKeyPattern|_|) ops =
        match ops with
        | [| op; Op_CheckSig |] ->
            match op with
            | PushData data -> Some (PayToPublicKey (Address.FromPublicKey data))
            | _ -> None
        | _ -> None

    let (|PayToAddressPattern|_|) ops =
        match ops with
        | [| Op_Dup; Op_HASH160; op; Op_EqualVerify; Op_CheckSig |] ->
            match op with
            | PushData data -> Some (PayToAddress (new Address(data)))
            | _ -> None
        | _ -> None

    let parseStandardOutputScript script =
        match script with
        | PayToPublicKeyPattern script -> Some(script)
        | PayToAddressPattern script -> Some(script)
        | _ -> None