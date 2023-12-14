import zio.{Cause, Console, Exit, Random, Scope, Task, ZIO, ZIOAppArgs, ZIOAppDefault}


//Using the appropriate effect constructor, fix the following function so that
//it no longer fails with defects when executed. Make a note of how the
//inferred return type for the function changes.
object Chapter4_1 extends ZIOAppDefault {
  def failWithMessage(string: String): ZIO[Any, Nothing, Nothing] =
    ZIO.succeed(throw new Error(string))

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    failWithMessage("test").catchAllCause(_ => ZIO.succeed())
  }
}

//Using the ZIO#foldCauseZIO operator and the Cause#defects method,
//implement the following function. This function should take the effect,
//inspect defects, and if a suitable defect is found, it should recover from
//the error with the help of the specified function, which generates a new
//success value for such a defect.
object Chapter4_2 extends ZIOAppDefault {

  def recoverFromSomeDefects[R, E, A](zio: ZIO[R, E, A])(
    f: Throwable => Option[A]
  ): ZIO[R, E, A] = {
    zio.foldCauseZIO(
      failure => ZIO.succeed(failure.defects.flatMap(f).head), // WTF ?
      success => ZIO.succeed(success)
    )
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    recoverFromSomeDefects(ZIO.fail(new RuntimeException("Woops")))(_ => Some("Reover"))
  }
}

//Using the ZIO#foldCauseZIO operator and the Cause#prettyPrint
//method, implement an operator that takes an effect, and returns a new
//effect that logs any failures of the original effect (including errors and
//defects), without changing its failure or success value.
object Chapter4_3 extends ZIOAppDefault {
  def logFailures[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = {
    zio.foldCauseZIO(
      failure => ZIO.succeed(failure.prettyPrint) *> ZIO.failCause(failure),
      success => ZIO.succeed(success)
    )
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    logFailures(ZIO.fail(new RuntimeException("Woops")))
  }
}

//Using the ZIO#foldCauseZIO method, which “runs” an effect to an Exit
//value, implement the following function, which will execute the specified
//effect on any failure at all:
object Chapter4_4 extends ZIOAppDefault {
  def onAnyFailure[R, E, A](
                             zio: ZIO[R, E, A],
                             handler: ZIO[R, E, Any]
                           ): ZIO[R, E, A] = {
    zio.exit.flatMap {
      case Exit.Failure(cause) => handler.flatMap(_ => ZIO.failCause(cause))
      case Exit.Success(value) => ZIO.succeed(value)
    }
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    onAnyFailure(ZIO.fail("WOOPS"), ZIO.succeed("HI"))
  }
}

//Using the ZIO#refineOrDie method, implement the ioException function,
//which refines the error channel to only include the IOException
//error.
object Chapter4_5 extends ZIOAppDefault {
  def ioException[R, A](
                         zio: ZIO[R, Throwable, A]
                       ): ZIO[R, java.io.IOException, A] = {
    zio.refineOrDie {
      case e: java.io.IOException => e
    }
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    ioException(ZIO.fail(new RuntimeException("WOOPS")))
  }
}

//Using the ZIO#refineToOrDie method, narrow the error type of the following
//effect to just NumberFormatException.
object Chapter4_6 extends ZIOAppDefault {

  val parseNumber: ZIO[Any, Throwable, Int] =
    ZIO.attempt("foo".toInt)

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    parseNumber.refineToOrDie[NumberFormatException]
  }
}

//Using the ZIO#foldZIO method, implement the following two functions,
//which make working with Either values easier, by shifting the unexpected
//case into the error channel (and reversing this shifting).
object Chapter4_7 extends ZIOAppDefault {

  def left[R, E, A, B](
    zio: ZIO[R, E, Either[A, B]]
  ): ZIO[R, Either[E, B], A] = {
    zio.foldZIO(
      failure => ZIO.fail(Left(failure)),
      success => success match {
        case Left(value) => ZIO.succeed(value)
        case Right(value) => ZIO.fail(Right(value))
      }
    )
  }

  def unleft[R, E, A, B](
    zio: ZIO[R, Either[E, B], A]
  ): ZIO[R, E, Either[A, B]] = {
    zio.foldZIO(
      failure => failure match {
        case Left(value) =>  ZIO.fail(value)
        case Right(value) => ZIO.succeed(Right(value))
      },
      success => ZIO.succeed(Left(success))
    )
  }
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    left(ZIO.succeed(Left("A"))) *> unleft(ZIO.succeed(Left("B")))
  }
}

//Using the ZIO#foldZIO method, implement the following two functions,
//which make working with Either values easier, by shifting the unexpected
//case into the error channel (and reversing this shifting).
object Chapter4_8 extends ZIOAppDefault {

  def right[R, E, A, B](
    zio: ZIO[R, E, Either[A, B]]
  ): ZIO[R, Either[E, A], B] = {
    zio.foldZIO(
      failure => ZIO.fail(Left(failure)),
      _.fold(error => ZIO.fail(Right(error)), success => ZIO.succeed(success))
    )
  }

  def unright[R, E, A, B](
    zio: ZIO[R, Either[E, A], B]
  ): ZIO[R, E, Either[A, B]] = {
    zio.foldZIO(
      _.fold(error => ZIO.fail(error), success => ZIO.succeed(Left(success))),
      success => ZIO.succeed(Right(success))
    )
  }
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    right(ZIO.succeed(Left("A"))) *> unright(ZIO.succeed(Left("B")))
  }
}

//Using the ZIO#sandbox method, implement the following function.
object Chapter4_9 extends ZIOAppDefault {
  def catchAllCause[R, E1, E2, A](
    zio: ZIO[R, E1, A],
    handler: Cause[E1] => ZIO[R, E2, A]
  ): ZIO[R, E2, A] = {
    zio.sandbox.foldZIO(handler, ZIO.succeed)
  }
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    ZIO.succeed(1)
  }
}

//Using the ZIO#foldCauseZIO method, implement the following function.
object Chapter4_10 extends ZIOAppDefault {
  def catchAllCause[R, E1, E2, A](
    zio: ZIO[R, E1, A],
    handler: Cause[E1] => ZIO[R, E2, A]
  ): ZIO[R, E2, A] = {
    zio.foldCauseZIO(handler, ZIO.succeed)
  }
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    ZIO.succeed(1)
  }
}