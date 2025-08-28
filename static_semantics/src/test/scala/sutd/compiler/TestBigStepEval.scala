package sutd.compiler

import scala.language.adhocExtensions
import org.scalatest.funsuite
import org.scalatest.matchers
import sutd.compiler.SimplyTypedLambdaCalculus.* 
import sutd.compiler.BigStepEval.*
import sutd.compiler.Util.*

class TestBigStepEval extends funsuite.AnyFunSuite {
    import Term.* 
    import Value.*
    import Const.* 
    import Op.*
    import Result.* 
    import Type.*

    val st = StateInfo(0)

    test("const should be evaluated to const") {
        val t = ConstTerm(IntConst(1))
        val expected = ConstValue(IntConst(1)) 

        eval(t).run(st) match {
            case Error(err) => assert(false, err)
            case Ok(_,v) => assert(v == expected)
        }
    }

    test("identity function should be returning the same value") {
        val id = LambdaTerm(Var("x"), IntTy, VarTerm(Var("x"))) // \x:int.x 
        val one = IntConst(1)
        val t = AppTerm(id, ConstTerm(one))
        val expected = ConstValue(one)

        eval(t).run(st) match {
            case Error(err) => assert(false, err)
            case Ok(_,v) => assert(v == expected)
        }
    }

    test("factorial 3 should be 6") {
        val three = IntConst(3)
        val six = IntConst(6) 
        val zero = IntConst(0)
        val one = IntConst(1)
        val varx = Var("x")
        val varf = Var("f")
        val cond = OpTerm(VarTerm(varx), DEqual, ConstTerm(zero)) // x == 0
        val ifelse = IfTerm(cond, ConstTerm(one), OpTerm(VarTerm(varx), Mult, AppTerm(VarTerm(varf), OpTerm(VarTerm(varx), Minus, ConstTerm(one))))) // if x == 0 then 1 else x * (f (x-1))
        val tyx = IntTy
        val tyf = FunTy(IntTy, IntTy)
        val fac = FixTerm(LambdaTerm(varf, tyf, LambdaTerm(varx, tyx, ifelse)))  // fix \f:int->int.\x:int. if x == 0 then 1 else x * (f (x-1))
        val t = AppTerm(fac, ConstTerm(three)) // fac 3
        val expected = ConstValue(six) 

        eval(t).run(st) match {
            case Error(err) => assert(false, err)
            case Ok(_,v) => assert(v == expected)
        }
    }
}
