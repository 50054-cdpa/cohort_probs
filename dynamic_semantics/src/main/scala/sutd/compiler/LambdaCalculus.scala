package sutd.compiler


object LambdaCalculus {
    /**
      * Lambda Calculus terms
      */
    enum Term {
        case ConstTerm(c:Const)                 // c
        case VarTerm(x:Var)                     // x
        case LambdaTerm(x:Var, body:Term)       // \x.t
        case AppTerm(t1:Term, t2:Term)          // t t
        case IfTerm(t1:Term, t2:Term, t3:Term)  // if t then t else t
        case OpTerm(t1:Term, op:Op, t2:Term)    // t op t
        case LetTerm(x:Var, t1:Term, t2:Term)   // let x = t1 in t2
        case FixTerm(t:Term)                    // fix t
    }

    /**
      * variable
      *
      * @param n name of the variable
      */
    case class Var(n:String)

    /**
      * contants
      */
    enum Const {
        case IntConst(v:Int)        // int const
        case BoolConst(v:Boolean)   // boolean const
    }

    /**
      * binary operators
      */
    enum Op {
        case Plus   // +
        case Minus  // -
        case Mult   // *
        case Div    // \ 
        case DEqual // == 
    }

    /**
      * all the possible values
      */
    enum Value {
        case ConstValue(c:Const) // c
        case LambdaValue(l:Term) // \x.t
    }

    import Term.*

    /**
      * compute the set of free variables in term t
      *
      * @param t - the term
      * @return a set of varables that are free in t
      */
    def fv(t:Term):Set[Var] = t match {
        case VarTerm(y) => Set(y) 
        case LambdaTerm(x, body) => fv(body) - x
        case AppTerm(t1, t2) => fv(t1) union fv(t2)
        case LetTerm(x, t1, t2) => (fv(t1) - x) union fv(t2) 
        case IfTerm(t1, t2, t3) => fv(t1) union fv(t2) union fv(t3) 
        case ConstTerm(c) => Set() 
        case FixTerm(t1) => fv(t1)
        case OpTerm(t1, op, t2) => fv(t1) union fv(t2)
    }


    
}    
