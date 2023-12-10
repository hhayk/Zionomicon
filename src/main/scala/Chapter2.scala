import Chapter2_1.ZIOToy
import zio.{Console, Random, Scope, Task, ZIO, ZIOAppArgs, ZIOAppDefault}

import java.io.IOException

//Implement a ZIO version of the function readFile by using the
//ZIO.attempt constructor.
object Chapter2_1 extends ZIOAppDefault {
  final case class ZIOToy[-R, +E, +A](run: R => Either[E, A])

  def readFile(file: String): String = {
    val source = scala.io.Source.fromFile(file)
    try source.getLines().mkString
    finally source.close()
  }

  def readFileZIO(file: String): Task[String] = {
    ZIO.attempt(readFile(file))
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    readFileZIO("src/main/resources/readFile.txt")
  }
}

//Implement a ZIO version of the function writeFile by using the
//ZIO.attempt constructor.
object Chapter2_2 extends ZIOAppDefault {
  def writeFile(file: String, text: String): Unit = {
    import java.io._
    val pw = new PrintWriter(new File(file))
    try pw.write(text)
    finally pw.close
  }

  def writeFileZio(file: String, text: String): Task[Unit] = {
    ZIO.attempt(writeFile(file, text))
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    writeFileZio("src/main/resources/writeFile.txt", "Hello World")
  }
}

//Using the flatMap method of ZIO effects, together with the readFileZio
//and writeFileZio functions that you wrote, implement a ZIO version of
//the function copyFile.
object Chapter2_3 extends ZIOAppDefault {
  def copyFile(source: String, dest: String): Unit = {
    val contents = Chapter2_1.readFile(source)
    Chapter2_2.writeFile(dest, contents)
  }

  def copyFileZio(source: String, dest: String): ZIO[Any, Throwable, Unit] = {
    Chapter2_1.readFileZIO(source).flatMap { contents =>
      Chapter2_2.writeFileZio(dest, contents)
    }
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    copyFileZio("src/main/resources/readFile.txt", "src/main/resources/copyWriteFile.txt")
  }
}

//Rewrite the following ZIO code that uses flatMap into a for comprehension.
object Chapter2_4 extends ZIOAppDefault {
  def printLine(line: String): Task[Unit] = ZIO.attempt(println(line))

  val readLine = ZIO.attempt(scala.io.StdIn.readLine())

  val printLineZio = {
    for {
      _ <- printLine("What is your name?")
      name <- readLine
      _ <- printLine(s"Hello, $name!")
    } yield ()
  }


  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    printLineZio
  }
}

//Rewrite the following ZIO code that uses flatMap into a for comprehension.
object Chapter2_5 extends ZIOAppDefault {
  val random = ZIO.attempt(scala.util.Random.nextInt(3) + 1)

  def printLine(line: String) = ZIO.attempt(println(line))

  val readLine = ZIO.attempt(scala.io.StdIn.readLine())

  val randomZio = {
    for {
      int <- random
      _ <- printLine("Guess a number from 1 to 3:")
      num <- readLine
      _ <- if (num == int.toString) printLine("You guessed right!")
      else printLine(s"You guessed wrong, the number was $int!")
    } yield ()
  }


  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    randomZio
  }
}

//Implement the zipWith function in terms of the toy model of a ZIO
//effect. The function should return an effect that sequentially composes
//the specified effects, merging their results with the specified user-defined
//function.
object Chapter2_6 extends ZIOAppDefault {

  def zipWith[R, E, A, B, C](
                              self: ZIOToy[R, E, A],
                              that: ZIOToy[R, E, B]
                            )(f: (A, B) => C): ZIOToy[R, E, C] = {
    ZIOToy { r =>
      for {
        selfRes <- self.run(r)
        thatRes <- that.run(r)
      } yield f(selfRes, thatRes)
    }
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    ZIO.attempt {
      val res = zipWith(
        ZIOToy((_: Any) => Right(1)),
        ZIOToy((_: Any) => Right(2)),
      )((a, b) => a + b).run(())

      println(res)
    }
  }
}

//Implement the collectAll function in terms of the toy model of a ZIO
//effect. The function should return an effect that sequentially collects the
//results of the specified collection of effects.
object Chapter2_7 extends ZIOAppDefault {
  def collectAll[R, E, A](
                           in: Iterable[ZIOToy[R, E, A]]
                         ): ZIOToy[R, E, List[A]] = {
    if (in.isEmpty) ZIOToy(_ => Right(List.empty))
    else Chapter2_6.zipWith(in.head, collectAll(in.tail))((a, b) => a :: b)
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    ZIO.attempt {
      val res = collectAll(
        List(
          ZIOToy((_: Any) => Right(1)),
          ZIOToy((_: Any) => Right(2)),
          ZIOToy((_: Any) => Right(3))
        )
      ).run()

      println(res)
    }
  }
}

//Implement the foreach function in terms of the toy model of a ZIO effect.
//The function should return an effect that sequentially runs the specified
//function on every element of the specified collection.
object Chapter2_8 extends ZIOAppDefault {
  def foreach[R, E, A, B](
                           in: Iterable[A]
                         )(f: A => ZIOToy[R, E, B]): ZIOToy[R, E, List[B]] = {
    Chapter2_7.collectAll(in.map(f))
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    ZIO.attempt {
      val res = foreach(List(1, 2, 3))(a => {
        ZIOToy((_: Any) => Right(a))
      }).run()

      println(res)
    }
  }
}

//Implement the orElse function in terms of the toy model of a ZIO effect.
//The function should return an effect that tries the left hand side, but if
//that effect fails, it will fallback to the effect on the right hand side.
object Chapter2_9 extends ZIOAppDefault {
  def orElse[R, E1, E2, A](
                            self: ZIOToy[R, E1, A],
                            that: ZIOToy[R, E2, A]
                          ): ZIOToy[R, E2, A] = {
    ZIOToy { r =>
      self.run(r) match {
        case Right(a) => Right(a)
        case Left(_) => that.run(r)
      }
    }

  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    ZIO.attempt {
      val res = orElse(
        ZIOToy((_: Any) => Left(new RuntimeException("Exp"))),
        ZIOToy((_: Any) => Right(2))
      ).run()

      println(res)
    }
  }
}

//Using the following code as a foundation, write a ZIO application that
//prints out the contents of whatever files are passed into the program as
//command-line arguments. You should use the function readFileZio that
//you developed in these exercises, as well as ZIO.foreach.
object Chapter2_10 extends ZIOAppDefault {
  def print(commandLineArguments: List[String]) = {
    ZIO.attempt {
      Chapter2_8.foreach(commandLineArguments) { file =>
        ZIOToy((_: Any) => Right {
          Chapter2_1.readFileZIO(file).flatMap(Chapter2_5.printLine)
        })
      }
    }
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    print(List("readFile.txt"))
  }
}

//Using ZIO.fail and ZIO.succeed, implement the following function,
//which converts an Either into a ZIO effect:
object Chapter2_11 extends ZIOAppDefault {
  def eitherToZIO[E, A](either: Either[E, A]): ZIO[Any, E, A] = {
    either.fold(e => ZIO.fail(e), s => ZIO.succeed(s))
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    eitherToZIO(Right(1))
  }
}

//Using ZIO.fail and ZIO.succeed, implement the following function,
//which converts a List into a ZIO effect, by looking at the head element in
//the list and ignoring the rest of the elements.
object Chapter2_12 extends ZIOAppDefault {
  def listToZIO[A](list: List[A]): ZIO[Any, None.type, A] = {
    list match {
      case a :: _ => ZIO.succeed(a)
      case Nil => ZIO.fail(None)
    }
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    listToZIO(List(1, 2, 3))
  }
}

//Using ZIO.succeed, convert the following procedural function into a ZIO
//function:
object Chapter2_13 extends ZIOAppDefault {
  def currentTime(): Long = java.lang.System.currentTimeMillis()

  lazy val currentTimeZIO: ZIO[Any, Nothing, Long] = ZIO.succeed(currentTime())

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    currentTimeZIO
  }
}

//Using ZIO.async, convert the following asynchronous, callback-based function
//into a ZIO function:
object Chapter2_14 extends ZIOAppDefault {
  def getCacheValue(
                     key: String,
                     onSuccess: String => Unit,
                     onFailure: Throwable => Unit
                   ): Unit = ???

  def getCacheValueZio(key: String): ZIO[Any, Throwable, String] =
    ZIO.async { callback =>
      getCacheValue(
        key,
        s => callback(ZIO.succeed(s)),
        e => callback(ZIO.fail(e))
      )
    }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    getCacheValueZio("ASD")
  }
}

//Using ZIO.async, convert the following asynchronous, callback-based function
//into a ZIO function:
object Chapter2_15 extends ZIOAppDefault {
  trait User

  def saveUserRecord(
                      user: User,
                      onSuccess: () => Unit,
                      onFailure: Throwable => Unit
                    ): Unit =
    ???

  def saveUserRecordZio(user: User): ZIO[Any, Throwable, Unit] = {
    ZIO.async { callback =>
      saveUserRecord(
        user,
        () => callback(ZIO.succeed(())),
        e => callback(ZIO.fail(e))
      )
    }
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    saveUserRecordZio(new User {})
  }
}

//Using ZIO.fromFuture, convert the following code to ZIO
object Chapter2_16 extends ZIOAppDefault {

  import scala.concurrent.{ExecutionContext, Future}

  trait Query

  trait Result

  def doQuery(query: Query)(implicit
                            ec: ExecutionContext
  ): Future[Result] =
    ???

  def doQueryZio(query: Query): ZIO[Any, Throwable, Result] =
    ZIO.fromFuture { implicit ec => doQuery(query) }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    doQueryZio(new Query {})
  }
}

//Using the Console, write a little program that asks the user what their
//name is, and then prints it out to them with a greeting.
object Chapter2_17 extends ZIOAppDefault {
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    for {
      _ <- Console.printLine("What is your name")
      name <- Console.readLine
      _ <- Console.printLine(s"Greetings $name")
    } yield ()
  }
}

//Using the Console and Random services in ZIO, write a little program that
//asks the user to guess a randomly chosen number between 1 and 3, and
//prints out if they were correct or not.
object Chapter2_18 extends ZIOAppDefault {
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    for {
      int <- Random.nextIntBounded(2).map(_ + 1)
      _ <- Console.printLine("Guess a number from 1 to 3:")
      num <- Console.readLine
      _ <- if (num == int.toString) Console.printLine("You guessed right!")
      else Console.printLine(s"You guessed wrong, the number was $int!")
    } yield ()
  }
}

//Using the Console service and recursion, write a function that will repeatedly
//read input from the console until the specified user-defined function
//evaluates to true on the input.
object Chapter2_19 extends ZIOAppDefault {
  def rec(cond: String => Boolean): ZIO[Console, IOException, String] = {
    Console.readLine.flatMap { str =>
      if (cond(str)) ZIO.succeed(str)
      else rec(cond)
    }
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    rec(s => s == "A")
  }
}

//Using recursion, write a function that will continue evaluating the specified
//effect, until the specified user-defined function evaluates to true on the
//output of the effect.
object Chapter2_20 extends ZIOAppDefault {
  def rec[R, E, A](effect: ZIO[R, E, A])(cond: A => Boolean): ZIO[R, E, A] = {
    effect.flatMap { a =>
      if (cond(a)) ZIO.succeed(a)
      else rec(effect)(cond)
    }
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    rec(ZIO.succeed(1))(v => v == 1)
  }
}
