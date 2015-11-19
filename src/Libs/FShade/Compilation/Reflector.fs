namespace FShade.Reflection

open System
open System.Reflection
open System.Reflection.Emit
open Mono.Reflection
open Aardvark.Base

[<AutoOpen>]
module Patterns =

    let inline private operandTarget8 (i : Instruction) = i.Operand |> unbox<int8> |> int32
    let inline private operandTarget32 (i : Instruction) = i.Operand |> unbox<int32>

    let inline (|Add|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Add then Some ()
        else None

    let inline (|Add_Ovf|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Add_Ovf then Some ()
        else None

    let inline (|Add_Ovf_Un|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Add_Ovf_Un then Some ()
        else None

    let inline (|And|_|) (i : Instruction) =
        if i.OpCode = OpCodes.And then Some ()
        else None

    let inline (|Arglist|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Arglist then Some ()
        else None

    let inline (|Beq|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Beq then Some (operandTarget32 i)
        elif i.OpCode = OpCodes.Beq_S then Some (operandTarget8 i)
        else None


    let inline (|Bge|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Bge then Some (operandTarget32 i)
        elif i.OpCode = OpCodes.Bge_S then Some (operandTarget8 i)
        else None


    let inline (|Bge_Un|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Bge_Un then Some (operandTarget32 i)
        elif i.OpCode = OpCodes.Bge_Un_S then Some (operandTarget8 i)
        else None


    let inline (|Bgt|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Bgt then Some (operandTarget32 i)
        elif i.OpCode = OpCodes.Bgt_S then Some (operandTarget8 i)
        else None


    let inline (|Bgt_Un|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Bgt_Un then Some (operandTarget32 i)
        elif i.OpCode = OpCodes.Bgt_Un_S then Some (operandTarget8 i)
        else None


    let inline (|Ble|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ble then Some (operandTarget32 i)
        elif i.OpCode = OpCodes.Ble_S then Some (operandTarget8 i)
        else None

    let inline (|Ble_Un|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ble_Un then Some (operandTarget32 i)
        elif i.OpCode = OpCodes.Ble_Un_S then Some (operandTarget8 i)
        else None


    let inline (|Blt|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Blt then Some (operandTarget32 i)
        elif i.OpCode = OpCodes.Blt_S then Some (operandTarget8 i)
        else None


    let inline (|Blt_Un|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Blt_Un then Some (operandTarget32 i)
        elif i.OpCode = OpCodes.Blt_Un_S then Some (operandTarget8 i)
        else None

    let inline (|Box|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Box then Some (unbox<Type> i.Operand)
        else None

    let inline (|Br|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Br then Some (operandTarget32 i)
        elif i.OpCode = OpCodes.Br then Some (operandTarget8 i)
        else None

    let inline (|Break|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Break then Some ()
        else None

    let inline (|Brfalse|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Brfalse then Some (operandTarget32 i)
        elif i.OpCode = OpCodes.Brfalse_S then Some (operandTarget8 i)
        else None


    let inline (|Brtrue|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Brtrue then Some (operandTarget32 i)
        elif i.OpCode = OpCodes.Brtrue_S then Some (operandTarget8 i)
        else None


    let inline (|Call|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Call then Some (unbox<MethodInfo> i.Operand)
        else None

    let inline (|Calli|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Call then Some (i.Operand)
        else None

    let inline (|Callvirt|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Callvirt then Some (unbox<MethodInfo> i.Operand)
        else None

    let inline (|Castclass|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Castclass then Some (unbox<Type> i.Operand)
        else None

    let inline (|Ceq|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ceq then Some ()
        else None

    let inline (|Cgt|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Cgt then Some ()
        else None

    let inline (|Cgt_Un|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Cgt_Un then Some ()
        else None

    let inline (|Ckfinite|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ckfinite then Some ()
        else None

    let inline (|Clt|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Clt then Some ()
        else None

    let inline (|Clt_Un|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Clt_Un then Some ()
        else None

    let inline (|Constrained|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Constrained then Some (i.Operand |> unbox<Type>)
        else None

    let inline (|Conv|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Conv_I then Some (typeof<nativeint>)
        elif i.OpCode = OpCodes.Conv_I1 then Some (typeof<int8>)
        elif i.OpCode = OpCodes.Conv_I2 then Some (typeof<int16>)
        elif i.OpCode = OpCodes.Conv_I4 then Some (typeof<int32>)
        elif i.OpCode = OpCodes.Conv_I8 then Some (typeof<int64>)
        elif i.OpCode = OpCodes.Conv_R4 then Some (typeof<float32>)
        elif i.OpCode = OpCodes.Conv_R8 then Some (typeof<float>)

        elif i.OpCode = OpCodes.Conv_U then Some (typeof<unativeint>)
        elif i.OpCode = OpCodes.Conv_U1 then Some (typeof<uint8>)
        elif i.OpCode = OpCodes.Conv_U2 then Some (typeof<uint16>)
        elif i.OpCode = OpCodes.Conv_U4 then Some (typeof<uint32>)
        elif i.OpCode = OpCodes.Conv_U8 then Some (typeof<uint64>)

        else None



    let inline (|Conv_Ovf|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Conv_Ovf_I then Some (typeof<nativeint>)
        elif i.OpCode = OpCodes.Conv_Ovf_I1 then Some (typeof<int8>)
        elif i.OpCode = OpCodes.Conv_Ovf_I2 then Some (typeof<int16>)
        elif i.OpCode = OpCodes.Conv_Ovf_I4 then Some (typeof<int32>)
        elif i.OpCode = OpCodes.Conv_Ovf_I8 then Some (typeof<int64>)
        elif i.OpCode = OpCodes.Conv_Ovf_U then Some (typeof<unativeint>)
        elif i.OpCode = OpCodes.Conv_Ovf_U1 then Some (typeof<uint8>)
        elif i.OpCode = OpCodes.Conv_Ovf_U2 then Some (typeof<uint16>)
        elif i.OpCode = OpCodes.Conv_Ovf_U4 then Some (typeof<uint32>)
        elif i.OpCode = OpCodes.Conv_Ovf_U8 then Some (typeof<uint64>)
        else None

    let inline (|Conv_Ovf_Un|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Conv_Ovf_I_Un then Some (typeof<nativeint>)
        elif i.OpCode = OpCodes.Conv_Ovf_I1_Un then Some (typeof<int8>)
        elif i.OpCode = OpCodes.Conv_Ovf_I2_Un then Some (typeof<int16>)
        elif i.OpCode = OpCodes.Conv_Ovf_I4_Un then Some (typeof<int32>)
        elif i.OpCode = OpCodes.Conv_Ovf_I8_Un then Some (typeof<int64>)
        elif i.OpCode = OpCodes.Conv_Ovf_U_Un then Some (typeof<unativeint>)
        elif i.OpCode = OpCodes.Conv_Ovf_U1_Un then Some (typeof<uint8>)
        elif i.OpCode = OpCodes.Conv_Ovf_U2_Un then Some (typeof<uint16>)
        elif i.OpCode = OpCodes.Conv_Ovf_U4_Un then Some (typeof<uint32>)
        elif i.OpCode = OpCodes.Conv_Ovf_U8_Un then Some (typeof<uint64>)
        else None

    let inline (|Conv_R_Un|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Conv_R_Un then Some ()
        else None



    let inline (|Cpblk|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Cpblk then Some ()
        else None

    let inline (|Cpobj|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Cpobj then Some (unbox<Type> i.Operand)
        else None

    let inline (|Div|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Div then Some ()
        else None

    let inline (|Div_Un|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Div_Un then Some ()
        else None

    let inline (|Dup|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Dup then Some ()
        else None

    //endfault missing

    let inline (|Endfinally|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Endfinally then Some ()
        else None

    let inline (|Endfilter|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Endfilter then Some ()
        else None

    let inline (|Initblk|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Initblk then Some ()
        else None

    let inline (|Initobj|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Initobj then Some (unbox<Type> i.Operand)
        else None

    let inline (|Isinst|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Isinst then Some (unbox<Type> i.Operand)
        else None

    let inline (|Jmp|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Jmp then Some (unbox<MethodInfo> i.Operand)
        else None

    

    let inline (|Ldarg|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldarg then Some (unbox<int16> i.Operand |> int)
        elif i.OpCode = OpCodes.Ldarg_S then Some (unbox<int8> i.Operand |> int)
        elif i.OpCode = OpCodes.Ldarg_0 then Some 0
        elif i.OpCode = OpCodes.Ldarg_1 then Some 1
        elif i.OpCode = OpCodes.Ldarg_2 then Some 2
        elif i.OpCode = OpCodes.Ldarg_3 then Some 3
        else None

    let inline (|Ldarga|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldarga then Some (unbox<int16> i.Operand |> int)
        elif i.OpCode = OpCodes.Ldarga_S then Some (unbox<int8> i.Operand |> int)
        else None 

    let inline (|LdInt32|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldc_I4 then Some (unbox<int> i.Operand)
        elif i.OpCode = OpCodes.Ldc_I4_S then Some (i.Operand |> unbox<int8> |> int)
        elif i.OpCode = OpCodes.Ldc_I4_0 then Some 0
        elif i.OpCode = OpCodes.Ldc_I4_1 then Some 1
        elif i.OpCode = OpCodes.Ldc_I4_2 then Some 2
        elif i.OpCode = OpCodes.Ldc_I4_3 then Some 3
        elif i.OpCode = OpCodes.Ldc_I4_4 then Some 4
        elif i.OpCode = OpCodes.Ldc_I4_5 then Some 5
        elif i.OpCode = OpCodes.Ldc_I4_6 then Some 6
        elif i.OpCode = OpCodes.Ldc_I4_7 then Some 7
        elif i.OpCode = OpCodes.Ldc_I4_8 then Some 8
        elif i.OpCode = OpCodes.Ldc_I4_M1 then Some -1
        else None

    let inline (|LdInt64|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldc_I8 then Some (unbox<int64> i.Operand)
        else None

    let inline (|LdR32|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldc_R4 then Some (unbox<float32> i.Operand)
        else None

    let inline (|LdR64|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldc_R8 then Some (unbox<float> i.Operand)
        else None

    
    let inline (|Ldelem|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldelem then Some (unbox<Type> i.Operand)
        elif i.OpCode = OpCodes.Ldelem_I then Some (typeof<nativeint>)
        elif i.OpCode = OpCodes.Ldelem_I1 then Some (typeof<int8>)
        elif i.OpCode = OpCodes.Ldelem_I2 then Some (typeof<int16>)
        elif i.OpCode = OpCodes.Ldelem_I4 then Some (typeof<int32>)
        elif i.OpCode = OpCodes.Ldelem_I8 then Some (typeof<int64>)
        elif i.OpCode = OpCodes.Ldelem_R4 then Some (typeof<float32>)
        elif i.OpCode = OpCodes.Ldelem_R8 then Some (typeof<float>)
        elif i.OpCode = OpCodes.Ldelem_Ref then Some (typeof<obj>)
        elif i.OpCode = OpCodes.Ldelem_U1 then Some (typeof<uint8>)
        elif i.OpCode = OpCodes.Ldelem_U2 then Some (typeof<uint16>)
        elif i.OpCode = OpCodes.Ldelem_U4 then Some (typeof<uint32>)
        // missing: Ldelem_U8
        else None


    let inline (|Ldelema|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldelema then Some (unbox<Type> i.Operand)
        else None

    let inline (|Ldfld|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldfld then Some (unbox<FieldInfo> i.Operand)
        else None

    let inline (|Ldflda|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldflda then Some (unbox<FieldInfo> i.Operand)
        else None

    let inline (|Ldftn|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldftn then Some (unbox<MethodInfo> i.Operand)
        else None

    let inline (|Ldind|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldind_I then Some (typeof<nativeint>)
        elif i.OpCode = OpCodes.Ldind_I1 then Some (typeof<int8>)
        elif i.OpCode = OpCodes.Ldind_I2 then Some (typeof<int16>)
        elif i.OpCode = OpCodes.Ldind_I4 then Some (typeof<int32>)
        elif i.OpCode = OpCodes.Ldind_I8 then Some (typeof<int64>)
        elif i.OpCode = OpCodes.Ldind_R4 then Some (typeof<float32>)
        elif i.OpCode = OpCodes.Ldind_R8 then Some (typeof<float>)
        elif i.OpCode = OpCodes.Ldind_Ref then Some (typeof<obj>)
        elif i.OpCode = OpCodes.Ldind_U1 then Some (typeof<uint8>)
        elif i.OpCode = OpCodes.Ldind_U2 then Some (typeof<uint16>)
        elif i.OpCode = OpCodes.Ldind_U4 then Some (typeof<uint32>)
        // missing: Ldind_U8
        else None


    let inline (|Ldlen|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldlen then Some ()
        else None

    let inline (|Ldloc|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldloc then Some (unbox<uint16> i.Operand |> int)
        elif i.OpCode = OpCodes.Ldloc_S then Some (unbox<uint8> i.Operand |> int)
        elif i.OpCode = OpCodes.Ldloc_0 then Some 0
        elif i.OpCode = OpCodes.Ldloc_1 then Some 1
        elif i.OpCode = OpCodes.Ldloc_2 then Some 2
        elif i.OpCode = OpCodes.Ldloc_3 then Some 3
        else None

    let inline (|Ldloca|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldloca then Some (unbox<uint16> i.Operand |> int)
        elif i.OpCode = OpCodes.Ldloca_S then Some (unbox<uint8> i.Operand |> int)
        else None

    let inline (|Ldnull|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldnull then Some ()
        else None

    let inline (|Ldobj|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldnull then Some (unbox<Type> i.Operand)
        else None

    let inline (|Ldsfld|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldsfld then Some (unbox<FieldInfo> i.Operand)
        else None

    let inline (|Ldsflda|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldsflda then Some (unbox<FieldInfo> i.Operand)
        else None

    let inline (|Ldstr|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldstr then Some (unbox<string> i.Operand)
        else None

    let inline (|Ldtoken|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldtoken then Some (i.Operand)
        else None

    let inline (|Ldvirtftn|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ldvirtftn then Some (unbox<MethodInfo> i.Operand)
        else None

    let inline (|Leave|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Leave then Some (unbox<int> i.Operand)
        elif i.OpCode = OpCodes.Leave_S then Some (unbox<int8> i.Operand |> int)
        else None

    let inline (|Localloc|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Localloc then Some ()
        else None

    let inline (|Mkrefany|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Mkrefany then Some (unbox<Type> i.Operand)
        else None

    let inline (|Mul|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Mul then Some ()
        else None

    let inline (|Mul_Ovf|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Mul_Ovf then Some ()
        else None

    let inline (|Mul_Ovf_Un|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Mul_Ovf_Un then Some ()
        else None

    let inline (|Neg|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Neg then Some ()
        else None

    let inline (|Newarr|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Newarr then Some (unbox<Type> i.Operand)
        else None

    let inline (|Newobj|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Newobj then Some (unbox<ConstructorInfo> i.Operand)
        else None

    let inline (|Nop|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Nop then Some ()
        else None

    let inline (|Not|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Not then Some ()
        else None

    let inline (|Or|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Or then Some ()
        else None

    let inline (|Pop|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Pop then Some ()
        else None

    let inline (|Readonly|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Readonly then Some ()
        else None

    let inline (|Refanytype|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Refanytype then Some ()
        else None

    let inline (|Refanyval|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Refanyval then Some (unbox<Type> i.Operand)
        else None

    let inline (|Rem|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Rem then Some ()
        else None

    let inline (|Rem_Un|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Rem_Un then Some ()
        else None

    let inline (|Ret|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Ret then Some ()
        else None

    let inline (|Rethrow|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Rethrow then Some ()
        else None

    let inline (|Shl|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Shl then Some ()
        else None

    let inline (|Shr|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Shr then Some ()
        else None

    let inline (|Shr_Un|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Shr_Un then Some ()
        else None

    let inline (|Sizeof|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Sizeof then Some (unbox<Type> i.Operand)
        else None

    let inline (|Starg|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Starg then Some (unbox<uint16> i.Operand |> int)
        elif i.OpCode = OpCodes.Starg_S then Some (unbox<uint8> i.Operand |> int)
        else None

    let inline (|Stelem|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Stelem then Some (unbox<Type> i.Operand)
        elif i.OpCode = OpCodes.Stelem_I then Some (typeof<nativeint>)
        elif i.OpCode = OpCodes.Stelem_I1 then Some (typeof<int8>)
        elif i.OpCode = OpCodes.Stelem_I2 then Some (typeof<int16>)
        elif i.OpCode = OpCodes.Stelem_I4 then Some (typeof<int32>)
        elif i.OpCode = OpCodes.Stelem_I8 then Some (typeof<int64>)
        elif i.OpCode = OpCodes.Stelem_R4 then Some (typeof<float32>)
        elif i.OpCode = OpCodes.Stelem_R8 then Some (typeof<float>)
        elif i.OpCode = OpCodes.Stelem_Ref then Some (typeof<obj>)
        else None

    let inline (|Stfld|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Stfld then Some (unbox<FieldInfo> i.Operand)
        else None

    let inline (|Stind|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Stind_I then Some (typeof<nativeint>)
        elif i.OpCode = OpCodes.Stind_I1 then Some (typeof<int8>)
        elif i.OpCode = OpCodes.Stind_I2 then Some (typeof<int16>)
        elif i.OpCode = OpCodes.Stind_I4 then Some (typeof<int32>)
        elif i.OpCode = OpCodes.Stind_I8 then Some (typeof<int64>)
        elif i.OpCode = OpCodes.Stind_R4 then Some (typeof<float32>)
        elif i.OpCode = OpCodes.Stind_R8 then Some (typeof<float>)
        elif i.OpCode = OpCodes.Stind_Ref then Some (typeof<obj>)
        else None

    let inline (|Stloc|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Stloc then Some (unbox<int16> i.Operand |> int)
        elif i.OpCode = OpCodes.Stloc_S then Some (unbox<int8> i.Operand |> int)
        elif i.OpCode = OpCodes.Stloc_0 then Some 0
        elif i.OpCode = OpCodes.Stloc_1 then Some 1
        elif i.OpCode = OpCodes.Stloc_2 then Some 2
        elif i.OpCode = OpCodes.Stloc_3 then Some 3
        else None

    let inline (|Stobj|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Stobj then Some (unbox<Type> i.Operand)
        else None

    let inline (|Stsfld|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Stsfld then Some (unbox<FieldInfo> i.Operand)
        else None

    let inline (|Sub|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Sub then Some ()
        else None

    let inline (|Sub_Ovf|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Sub_Ovf then Some ()
        else None

    let inline (|Sub_Ovf_Un|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Sub_Ovf_Un then Some ()
        else None
    
    let inline (|Switch|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Switch then Some (i.Operand)
        else None

    let inline (|Tailcall|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Tailcall then Some ()
        else None

    let inline (|Throw|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Throw then Some ()
        else None

    let inline (|Unaligned|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Unaligned then Some ()
        else None

    let inline (|Unbox|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Unbox then Some (unbox<Type> i.Operand)
        else None

    let inline (|Unbox_Any|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Unbox_Any then Some (unbox<Type> i.Operand)
        else None

    let inline (|Volatile|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Volatile then Some ()
        else None

    let inline (|Xor|_|) (i : Instruction) =
        if i.OpCode = OpCodes.Xor then Some ()
        else None

type Argument =
    | Constant of obj
    | Argument of int
    | Field of Argument * FieldInfo

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Argument =
    let isConstant (a : Argument) =
        match a with
            | Constant _ -> true
            | _ -> false

    let isArgument (a : Argument) =
        match a with
            | Argument _ -> true
            | _ -> false


    let constantValue (a : Argument) =
        match a with
            | Constant v -> v
            | _ -> failwith "cannot get constant value for non-constant Argument"

type Closure = { inner : MethodBase; args : list<Argument> }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Closure =
    let substitute (index : int) (value : Argument) (c : Closure) = 
        if c.inner.IsStatic then
            { inner = c.inner; args = c.args |> List.mapi (fun i a -> if i = index then value else a)}
        else
            { inner = c.inner; args = c.args |> List.mapi (fun i a -> if i = index + 1 then value else a)}
        
module PartialEvaluator =
    let rec private tryDeconstructClosure (args : Argument[]) (stack : list<Argument>) (i : list<Instruction>) =
        match i with
            | Nop::i -> 
                tryDeconstructClosure args stack i

            | Ldarg 0 :: i ->
                tryDeconstructClosure args (args.[0]::stack) i

            | Ldarg arg::i ->
                tryDeconstructClosure args (args.[arg]::stack) i
            
            | Ldfld f :: i ->
                match stack with
                    | Constant o::stack ->
                        let value = Constant (f.GetValue o)
                        tryDeconstructClosure args (value :: stack) i
                    | a::stack ->
                        let value = Field (a, f)
                        tryDeconstructClosure args (value :: stack) i
                    | _ ->
                        failwith "stack imbalance"

            | Ldsfld f :: i ->
                let value = f.GetValue(null)
                tryDeconstructClosure args (Constant value :: stack) i

            | LdInt32 v :: i -> tryDeconstructClosure args (Constant v :: stack) i
            | LdInt64 v :: i -> tryDeconstructClosure args (Constant v :: stack) i
            | LdR32 v :: i -> tryDeconstructClosure args (Constant v :: stack) i
            | LdR64 v :: i -> tryDeconstructClosure args (Constant v :: stack) i
            | Ldstr v :: i -> tryDeconstructClosure args (Constant v :: stack) i


            | Tailcall :: i ->
                tryDeconstructClosure args stack i
            

            | [Callvirt mi; Ret] ->
                let stack = List.rev stack
                match stack with
                    | Constant c :: _ ->
                        let impl = c.GetType().GetMethod(mi.Name, mi.GetParameters() |> Array.map (fun pi -> pi.ParameterType))
                   
                        let args = stack |> List.toArray
                        let instructions = impl.GetInstructions() |> Seq.toList
                        match tryDeconstructClosure args [] instructions with
                            | Some res -> Some res
                            | None -> Some { inner = mi; args = stack }
                    | _ ->
                        Some { inner = mi; args = stack }

            | [Call mi; Ret] ->
                let stack = List.rev stack
                Some { inner = mi; args = stack }


            | Call mi :: i ->
                let parameters = mi.GetParameters()

                let argCount =
                    if mi.IsStatic then parameters.Length
                    else 1 + parameters.Length

                let fArgs = stack |> Seq.take argCount |> Seq.toList
                let stack = stack |> Seq.skip argCount |> Seq.toList

                if fArgs |> List.forall Argument.isConstant then
                    let fArgs = List.rev fArgs |> List.map Argument.constantValue |> List.toArray

                    let res =
                        if mi.IsStatic then mi.Invoke(null, fArgs)
                        else 
                            let self = fArgs.[0]
                            let fArgs = Array.sub fArgs 1 (fArgs.Length-1)
                            mi.Invoke(self, fArgs)

                    tryDeconstructClosure args (Constant res :: stack) i

                else
                    None


            | _ -> None


    let getClosure (f : 'a) =
        let t = f.GetType()
        let invoke = 
            t.GetMethods() 
                |> Array.filter (fun mi -> mi.Name = "Invoke") 
                |> Array.maxBy (fun mi -> mi.GetParameters().Length)

        let instructions = invoke.GetInstructions()
        let args = Array.append [|Constant (f :> obj)|] (invoke.GetParameters() |> Array.mapi (fun i _ -> Argument i))

        let res = tryDeconstructClosure args [] (Seq.toList instructions)
        match res with
            | Some res -> res
            | None -> { inner = invoke; args = Array.toList args }
