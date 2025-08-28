package sutd.compiler


import scala.language.adhocExtensions
import org.scalatest.funsuite
import org.scalatest.matchers
import sutd.compiler.SimplyTypedLambdaCalculus.* 
import sutd.compiler.TypeCheck.*
import sutd.compiler.Util.*

class TestTypeCheck extends funsuite.AnyFunSuite {
    import Term.* 
    import Value.*
    import Const.* 
    import Op.*
    import Result.* 
    import Type.*

    val st = StateInfo(0)

    test("const 1 should have type int") {
        val t = ConstTerm(IntConst(1))
        val expected = IntTy
        val g = Map[Var, Type]()
        typeCheck(g, t).run(st) match {
            case Error(err) => assert(false, err)
            case Ok(_,v) => assert(v == expected)
        }
    }

    test("identity function should be have type int -> int") {
        val id = LambdaTerm(Var("x"), IntTy, VarTerm(Var("x"))) // \x:int.x 
        val expected = FunTy(IntTy, IntTy)
        val g = Map[Var, Type]()

        typeCheck(g, id).run(st) match {
            case Error(err) => assert(false, err)
            case Ok(_,v) => assert(v == expected)
        }
    }

    test("factorial should have type int -> int") {
        val three = IntConst(3)
        val zero = IntConst(0)
        val one = IntConst(1)
        val varx = Var("x")
        val varf = Var("f")
        val cond = OpTerm(VarTerm(varx), DEqual, ConstTerm(zero)) // x == 0
        val ifelse = IfTerm(cond, ConstTerm(one), OpTerm(VarTerm(varx), Mult, AppTerm(VarTerm(varf), OpTerm(VarTerm(varx), Minus, ConstTerm(one))))) // if x == 0 then 1 else x * (f (x-1))
        val tyx = IntTy
        val tyf = FunTy(IntTy, IntTy)
        val fac = FixTerm(LambdaTerm(varf, tyf, LambdaTerm(varx, tyx, ifelse)))  // fix \f:int->int.\x:int. if x == 0 then 1 else x * (f (x-1))
        val g = Map[Var, Type]()
        val expected = FunTy(IntTy,IntTy)

        typeCheck(g, fac).run(st) match {
            case Error(err) => assert(false, err)
            case Ok(_,v) => assert(v == expected)
        }
    }
}