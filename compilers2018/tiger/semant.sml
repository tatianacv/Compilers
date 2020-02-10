structure Semant :
     sig val transProg : Absyn.exp -> unit end =
struct
  structure A = Absyn

  structure Translate = struct type exp = unit end

  type expty = {exp: Translate.exp, ty: Types.ty}
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table

  fun checkInt ({ty=Types.INT, exp=_}, pos) = ()
  | checkInt ({ty=_,exp=_},pos) = ErrorMsg.error pos "integer required"

  fun transProg (exp:A.exp) : unit =
    let
      val {ty=_, exp=prog} = transExp (Env.base_venv, Env.base_tenv) exp
    in
      prog
    end
  and transExp(venv:venv,tenv:tenv) : A.exp -> expty =
    let fun trexp (A.OpExp{left, oper= A.PlusOp, right, pos}) =
      (checkInt (trexp left, pos);
       print "sw $a0 -4($sp)\naddiu $sp $sp -4\n";
       checkInt (trexp right, pos);
       print "lw $t1 0($sp)\n";
       print "addiu $sp $sp 4\n";
       print "add $a0 $a0 $t1\n";
       {ty=Types.INT, exp=()})
       | trexp (A.IntExp _) = {ty=Types.INT, exp=(print ("li $a0 " ^ Int.toString n ^ "\n"))}
       | trexp (A.LetExp{decs, body, pos}) = 
          (print "b main\n";
          let 
            val {venv = venv', tenv = tenv'}= transDecs(venv,tenv,decs)
          in 
            print "main:\n";
            transExp(venv',tenv') body
          end
          )
       | trexp (A.StringExp (_,_)) = {ty=Types.STRING, exp=()}
       | trexp (A.VarExp var) = trvar var 
       | trexp (A.IfExp {test, then', else', pos}) = 
          let 
            var {exp, ty} = trexp test 
       | trexp (A.SeqExp exppos) = 
          let 
            fun helper (lastty, []) = lastty
              | helper (lastty, (head,_)::tail) = 
              let 
                val {ty, exp} = trexp head
              in 
                helper (ty, tail)
              end
          in 
            {ty = helper (Types.UNIT, exppos), exp = ()}
          end
       | trexp (A.CallExp {func, args, pos})=
          (print "sw $fp, 0($sp)\n";
          print "addiu $sp, $sp, -4\n";
          map trexp args;
          print "sw $a0, 0($sp)\n";
          print "addiu $sp, $sp, -4\n";
          print ("jal " ^ Symbol.name func ^ ("\n"));
          {exp = (), ty = Types.INT}
          )
       | trexp _ = {ty=Types.UNIT, exp=ErrorMsg.error 0 "Can't typecheck this yet"}
        and  trvar (A.SimpleVar (id, pos)) = 
              (case Symbol.look (venv,id) of SOME(Env.VarEntry{ty}) => {exp=(), ty = actual_ty ty}
               | NONE => (ErrorMsg.error pos "Undefined variable"; 
               {exp=(), ty=Types.NIL}))
            | trvar (A.FieldVar(v, id, pos)) = 
        and actual_ty (Types.NAME(id, _)) =  
            (case Symbol.look (tenv, id) of SOME(ty) =>  actual_ty ty
                  | NONE => (ErrorMsg.error 0 "Undefined variable"; Types.NIL))
            | actual_ty ty = ty
    in
      trexp
    end
  and transDec (venv,tenv, A.VarDec{name, typ=SOME(tyrone), init,...}) =
      let 
        val {exp, ty} = transExp(venv,tenv) init
      in 
        if ty = transTy(tenv, A.NameTy tyrone)  then 
          {tenv=tenv, venv=Symbol.enter(venv, name, Env.VarEntry{ty=ty})}
        else 
          (ErrorMsg.error 0 "Types don't match"; {venv=venv, tenv=tenv})
      end
      | transDec (venv, tenv, A.TypeDec[{name, ty, pos}]) = 
          {venv=venv, tenv=Symbol.enter(tenv, name, transTy(tenv,ty))}
      | transDec (venv,tenv, A.FunDec [fun]) = 
        let 
          val venv' = dofun (venv, tenv, func)
        in 
          {venv=venv', tenv=tenv'}
        end 
  and dofun (venv, tenv, {name, params, result, body, pos}) = 
      (print ((Symbol.name name) ^ ":\n");
       print "addiu $fp $sp 0\n";
       print "sw $ra 0($sp)\n";
       print "addiu $sp, $sp, -4\n";
       transExp (venv, tenv) body;
       print "lw $ra 0($fp)\n";
       print "addiu $sp $sp 4\n";     (*pop return address*)
       print "addiu $sp $sp 4\n";     (*pop return argument*)
       print "lw $fp 4($sp)\n";
       print "addiu $sp $sp 4\n";     (*pop old fp*)
       print "jr $ra\n"
       ) 
  and transDecs (venv, tenv, nil) = {tenv = tenv, venv= venv})
      | transDecs (venv, tenv, (x::xs)) = 
        let 
          val {venv = venv', tenv = tenv'} = transDec(venv, tenv, x)
        in 
          transDecs(venv', tenv', xs)
        end
  and transTy (tenv, A.NameTy (sym, pos)) = 
      case (Symbol.look(tenv,sym)) of SOME (ty) => ty 
      | NONE => (ErrorMsg.error 0 "No hay tipo"; Types.NIL)
end
