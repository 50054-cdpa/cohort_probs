package sutd.compiler

import sutd.compiler.Functor.*
import sutd.compiler.Applicative.*
import sutd.compiler.Monad.*

object Parsec {
    enum Progress[+A] {
        case Consumed(value: A)
        case Empty(value: A)
    }
    type Error = String
    enum Result[+A] {
        case Ok(value: A)
        case Failed(msg: Error)
    }

    import Progress.*
    import Result.*
    case class Parser[T, A](p: List[T] => Progress[Result[(A, List[T])]]) {
        def map[B](f: A => B): Parser[T, B] = this match {
            case Parser(p) =>
                Parser(toks =>
                    p(toks) match {
                        case Empty(Failed(err))    => Empty(Failed(err))
                        case Empty(Ok((a, toks1))) => Empty(Ok((f(a), toks1)))
                        case Consumed(Failed(err)) => Consumed(Failed(err))
                        case Consumed(Ok((a, toks1))) =>
                            Consumed(Ok((f(a), toks1)))
                    }
                )
        }
        def flatMap[B](f: A => Parser[T, B]): Parser[T, B] = this match {
            case Parser(p) =>
                Parser(toks =>
                    p(toks) match {
                        case Consumed(v) => {
                            lazy val cont = v match {
                                case Failed(err) => Failed(err)
                                case Ok((a, toks1)) =>
                                    f(a) match {
                                        case Parser(pb) =>
                                            pb(toks1) match {
                                                case Consumed(x) => x
                                                case Empty(x)    => x
                                            }
                                    }
                            }
                            Consumed(cont)
                        }
                        case Empty(v) =>
                            v match {
                                case Failed(err) => Empty(Failed(err))
                                case Ok((a, toks1)) =>
                                    f(a) match {
                                        case Parser(pb) => pb(toks1)
                                    }
                            }
                    }
                )
        }
    }

    def run[T, A](
        parser: Parser[T, A]
    )(toks: List[T]): Progress[Result[(A, List[T])]] = parser match {
        case Parser(p) => p(toks)
    }

    type ParserM = [T] =>> [A] =>> Parser[T, A]

    given parsecMonadError[T]: MonadError[ParserM[T], Error] =
        new MonadError[ParserM[T], Error] {
            override def pure[A](a: A): Parser[T, A] =
                Parser(cs => Empty(Ok((a, cs))))
            override def bind[A, B](
                fa: Parser[T, A]
            )(f: A => Parser[T, B]): Parser[T, B] = fa.flatMap(f)

            override def raiseError[A](e: Error): Parser[T, A] =
                Parser(toks => Empty(Failed(e)))
            override def handleErrorWith[A](
                fa: Parser[T, A]
            )(f: Error => Parser[T, A]): Parser[T, A] = fa match {
                case Parser(p) =>
                    Parser(toks =>
                        p(toks) match {
                            case Empty(
                                  Failed(err)
                                ) => // only backtrack when it is empty
                                {
                                    f(err) match {
                                        case Parser(p2) => p2(toks)
                                    }
                                }
                            case Empty(
                                  Ok(v)
                                ) => // LL(1) parser will also attempt to look at f if fa does not consume anything
                                {
                                    f("faked error") match {
                                        case Parser(p2) =>
                                            p2(toks) match {
                                                case Empty(_) =>
                                                    Empty(
                                                      Ok(v)
                                                    ) // if f also fail, we report the error from fa
                                                case consumed => consumed
                                            }
                                    }
                                }
                            case Consumed(v) => Consumed(v)
                        }
                    )
            }

        }

    // explicit try and backtrack if fails
    def attempt[T, A](p: Parser[T, A]): Parser[T, A] =
        Parser(toks =>
            run(p)(toks) match {
                case Consumed(Failed(err)) => Empty(Failed(err))
                case otherwise             => otherwise
            }
        )

    def choice[T, A](p: Parser[T, A])(q: Parser[T, A])(using
        m: MonadError[ParserM[T], Error]
    ): Parser[T, A] = m.handleErrorWith(p)(e => q)

        // unconditionally parse a single item
    def item[T]: Parser[T, T] =
        Parser(toks => {
            toks match {
                case Nil =>
                    Empty(
                      Failed(
                        s"item() is called with an empty token stream"
                      )
                    )
                case (c :: cs) =>
                    Consumed(Ok((c, cs)))
            }
        })

    def sat[T](p: T => Boolean, err:String=""): Parser[T, T] = Parser(
      toks => {
          toks match {
              case Nil =>
                  Empty(
                    Failed(
                      s"sat() is called with an empty token stream. ${err}"
                    )
                  )
              case (c :: cs) if p(c) =>
                  Consumed(Ok((c, cs)))
              case (c :: cs) =>
                  Empty(
                    Failed(
                      s"sat() is called with a unsatisfied predicate with ${c}. ${err}"
                    )
                  )

          }
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
                case Empty(Ok((x, toks1))) => Empty(Ok((x.reverse, toks1)))
                case Empty(Failed(err))   => Empty(Failed(err))
                case Consumed(Ok((x, toks1))) =>
                    Consumed(Ok((x.reverse, toks1)))
                case Consumed(Failed(err)) => Consumed(Failed(err))
            }
        )
    }

    def manyOp[T, A](p: Parser[T, A]): Parser[T, List[A]] = {
        def walk(acc: List[A])(
            toks: List[T]
        )(r: Progress[Result[(A, List[T])]]): Result[(List[A], List[T])] =
            r match {
                case Empty(Failed(err)) => Ok((acc, toks))
                case Empty(Ok(v)) =>
                    Failed(
                      s"manyOp() is applied which yields a possible empty value but nothing is consumed."
                    ) // not making progress
                case Consumed(Failed(err)) => Failed(err)
                case Consumed(Ok((x, toks1))) =>
                    val acc_ = (x :: acc)
                    walk(acc_)(toks1)(run(p)(toks1))
            }
        Parser(toks =>
            run(p)(toks) match {
                case Empty(Failed(err)) => Empty(Ok((Nil, toks)))
                case Empty(Ok(v)) =>
                    Empty(
                      Failed(
                        s"manyOp() is applied a sub parser which yields a possible empty value and nothing is consumed."
                      )
                    )
                case Consumed(x) => Consumed(walk(Nil)(toks)(Consumed(x)))
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
        choice(attempt(p1))(p2) // for comprehesion is not composable			?
    }

    // either one
    // backtrack even when committed
    def either1[A, B, T](
        pa: Parser[T, A]
    )(pb: Parser[T, B]): Parser[T, Either[A, B]] = {
        val p1: Parser[T, Either[A, B]] = for (a <- pa) yield Left(a)
        lazy val p2: Parser[T, Either[A, B]] = for (b <- pb) yield Right(b)
        choice(attempt(p1))(p2)
    }

    // optional
    def optional[T, A](pa: Parser[T, A]): Parser[T, Either[Unit, A]] = {
        val p1: Parser[T, Either[Unit, A]] = for (a <- pa) yield Right(a)
        lazy val p2: Parser[T, Either[Unit, A]] =
            Parser(env => Empty(Ok((Left(()), env))))
        choice(attempt(p1))(p2)
    }

    // everything until condition
    def everythingUntil[T](p: T => Boolean): Parser[T, List[T]] = {
        // need to create some local function to get around the local type inference
        def rest(c: T): Parser[T, List[T]] =
            if (!p(c)) { for { cs <- everythingUntil(p) } yield (c :: cs) }
            else { Parser(env => Empty(Ok((Nil, env)))) }
        for {
            t <- item // a bug here?
            r <- rest(t)
        } yield r
    }

    // get something without consuming the input
    def lookAhead[T, A](p: Parser[T, A]): Parser[T, A] = {
        for {
            toks <- get
            x <- p
            () <- set(toks)
        } yield x
    }

    def get[T]: Parser[T, List[T]] = Parser(toks => Empty(Ok((toks, toks))))


    def set[T](toks:List[T]): Parser[T, Unit] =
        Parser(toks1 => Empty(Ok(((), toks))))



    // apply f to a to extract b, if the result is None, signal failure
    def someOrFail[E, A, B](a:A)( f:A=>Option[B])(err:Error):Parser[E, B] = Parser(
        env => f(a) match {
            case Some(v) => Empty(Ok((v, env)))
            case None => Empty(Failed(err))
        }
    )
    // not consuming anything and return the given value.
    def empty[E, A](a:A):Parser[E,A] = Parser( env => Empty(Ok((a, env))))

}
