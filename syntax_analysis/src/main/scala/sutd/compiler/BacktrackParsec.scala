package sutd.compiler

import sutd.compiler.Functor.*
import sutd.compiler.Applicative.*
import sutd.compiler.Monad.*

object BacktrackParsec {

    type Error = String
    enum Result[+A] {
        case Ok(value: A)
        case Failed(msg: Error)
    }

    import Result.*

    case class Parser[T, A](p: List[T] => Result[(A, List[T])]) {
        def map[B](f: A => B): Parser[T, B] = this match {
            case Parser(p) =>
                Parser(toks =>
                    p(toks) match {
                        case Failed(err)   => Failed(err)
                        case Ok((a, toks1)) => Ok((f(a), toks1))
                    }
                )
        }
        def flatMap[B](f: A => Parser[T, B]): Parser[T, B] = this match {
            case Parser(p) =>
                Parser(toks =>
                    p(toks) match {
                        case Failed(err) => Failed(err)
                        case Ok((a, toks1)) =>
                            f(a) match {
                                case Parser(pb) => pb(toks1)
                            }
                    }
                )
        }
    }

    def run[T, A](
        parser: Parser[T, A]
    )(toks: List[T]): Result[(A, List[T])] = parser match {
        case Parser(p) => p(toks)
    }

    type ParserM = [T] =>> [A] =>> Parser[T, A]

    given parsecMonadError[T]: MonadError[ParserM[T], Error] =
        new MonadError[ParserM[T], Error] {
            override def pure[A](a: A): Parser[T, A] =
                Parser(cs => Ok((a, cs)))
            override def bind[A, B](
                fa: Parser[T, A]
            )(f: A => Parser[T, B]): Parser[T, B] = fa.flatMap(f)
            override def raiseError[A](e: Error): Parser[T, A] =
                Parser(toks => Failed(e))
            override def handleErrorWith[A](
                fa: Parser[T, A]
            )(f: Error => Parser[T, A]): Parser[T, A] = fa match {
                case Parser(p) =>
                    Parser(toks =>
                        p(toks) match {
                            case Failed(err) => run(f(err))(toks)
                            case Ok(v)       => Ok(v)
                        }
                    )
            }

        }

    def choice[T, A](p: Parser[T, A])(q: Parser[T, A])(using
        m: MonadError[ParserM[T], Error]
    ): Parser[T, A] = m.handleErrorWith(p)(e => q)

    def get[T]: Parser[T, List[T]] =
        Parser(toks => Ok((toks, toks)))

    def set[T](toks: List[T]): Parser[T, Unit] =
        Parser(_ => Ok(((), toks)))

    // unconditionally parse a single item
    def item[T]: Parser[T, T] =
        Parser(toks => {
            toks match {
                case Nil =>
                    Failed(s"item() is called with an empty token stream")
                case (c :: cs) => Ok((c, cs))
            }
        })

    def sat[T](p: T => Boolean, err:String=""): Parser[T, T] = Parser(toks =>
        toks match {
            case Nil => Failed(s"sat() is called with an empty token stream. ${err}")
            case (c :: cs) if p(c) => Ok((c, cs))
            case (c :: cs) =>
                Failed(s"sat() is called with a unsatisfied predicate with ${c}. ${err}")
        }
    )

    // one or more
    def many1[T, A](p: Parser[T, A]): Parser[T, List[A]] = for {
        a <- p
        as <- many(p)
    } yield (a :: as)

    // zero or more
    def many[T, A](p: Parser[T, A]): Parser[T, List[A]] = {
        Parser(toks =>
            run(manyOp(p))(toks) match {
                case Ok((x, toks1)) => Ok((x.reverse, toks1))
                case Failed(err)   => Failed(err)
            }
        )
    }

    def manyOp[T, A](p: Parser[T, A]): Parser[T, List[A]] = {
        def walk(acc: List[A])(toks: List[T])(r: Result[(A, List[T])]): Result[(List[A], List[T])] =
            r match {
                case Failed(err) => Ok((acc, toks)) // we stop
                case Ok((x, toks1)) =>
                    val acc_ = (x :: acc)
                    walk(acc_)(toks1)(run(p)(toks1))
            }
        Parser(toks =>
            run(p)(toks) match {
                case Failed(err) => Ok((Nil, toks))
                case Ok(v) => (walk(Nil)(toks)(Ok(v)))
            }
        )
    }

    // interleave As with B as delimeter
    def interleave[T, A, B](
        pa: Parser[T, A]
    )(pb: Parser[T, B]): Parser[T, List[A]] = {
        lazy val p1 = for {
            a <- pa
            b <- pb
            as <- interleave(pa)(pb)
        } yield (a :: as)
        lazy val p2 = for {
            a <- pa
        } yield (List(a))
        choice(p1)(p2) 
    }

    // optional
    def optional[T, A](pa: Parser[T, A]): Parser[T, Either[Unit, A]] = {
        val p1: Parser[T, Either[Unit, A]] = for (a <- pa) yield (Right(a))
        val p2: Parser[T, Either[Unit, A]] = Parser(toks => Ok((Left(()), toks)))
        choice(p1)(p2)
    }

    // everything until condition
    def everythingUntil[T](p: T => Boolean): Parser[T, List[T]] = {
        def rest(c: T): Parser[T, List[T]] =
            if (!p(c)) { for { cs <- everythingUntil(p) } yield (c :: cs) }
            else { Parser(toks => Ok((Nil, toks))) }
        for {
            t <- item 
            r <- rest(t)
        } yield r
    }

    // apply f to a to extract b, if the result is None, signal failure
    def someOrFail[T, A, B](a:A)( f:A=>Option[B])(err:Error):Parser[T, B] = Parser(
        toks => f(a) match {
            case Some(v) =>  Ok((v, toks))
            case None => Failed(err)
        }
    )


}
