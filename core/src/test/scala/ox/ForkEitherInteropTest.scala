package ox

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.compiletime.testing.*

class ForkEitherInteropTest extends AnyFlatSpec with Matchers {

  "forkUnsupervised" should "prevent usage of either.ok() combinator" in {
    assertReceivesForkedScopeError(
      """import ox.*
         import ox.either.*

         val e1: Either[Throwable, Unit] = Left(Exception("uh oh"))
         unsupervised {
          either {
             forkUnsupervised {
               e1.ok()
             }
           }
         }"""
    )
  }

  "forkCancellable" should "prevent usage of either.ok() combinator" in {
    assertReceivesForkedScopeError {
      """import ox.*
         import ox.either.*

         val e1: Either[Throwable, Unit] = Left(Exception("uh oh"))
         unsupervised {
           either {
             forkCancellable {
               e1.ok()
             }
           }
         }"""
    }
  }

  "fork" should "prevent usage of either.ok() combinator" in {
    assertReceivesForkedScopeError {
      """import ox.*
       import ox.either.*

       val e1: Either[Throwable, Unit] = Left(Exception("uh oh"))
       supervised {
         either {
           fork {
             e1.ok()
           }
         }
       }
       """
    }
  }

  "forkUser" should "prevent usage of either.ok() combinator" in {
    assertReceivesForkedScopeError {
      """import ox.*
       import ox.either.*

       val e1: Either[Throwable, Unit] = Left(Exception("uh oh"))
       supervised {
         either {
           forkUser {
             e1.ok()
           }
         }
       }
       """
    }
  }

  "forkError" should "prevent usage of either.ok() combinator" in {
    assertReceivesForkedScopeError {
      """import ox.*
       import ox.either.*

       val e1: Either[Throwable, Unit] = Left(Exception("uh oh"))
       supervisedError(EitherMode[Throwable]) {
         either {
           val f = forkError {
             e1.ok()
             Right(23)
           }

           f.joinEither()
         }
       }
       """
    }
  }

  "forkUserError" should "prevent usage of either.ok() combinator" in {
    assertReceivesForkedScopeError {
      """import ox.*
       import ox.either.*

       val e1: Either[Throwable, Unit] = Left(Exception("uh oh"))
       supervisedError(EitherMode[String]) {
         either {
           val f = forkUserError {
             e1.ok()
             Right(23)
           }

           f.join()
           Left("oops")
         }
       }"""
    }
  }

  "forkAll" should "prevent usage of either.ok() combinator" in {
    assertReceivesForkedScopeError {
      """import ox.*
         import ox.either.*

         val e1: Either[Throwable, Unit] = Left(Exception("uh oh"))
         supervised {
           either {
             forkAll(Vector(() => e1.ok()))
           }
         }"""
    }
  }

  "fork/either interop" should "disallow usage of fork in either blocks" in {
    assertReceivesForkedScopeError {
      """import ox.*
         import ox.either.*

         val e1: Either[Throwable, Unit] = Left(Exception("uh oh"))
         supervised { 
           either { 
             fork { 
               e1.ok() 
             }
           }
         }"""
    }
  }

  // empty line at the beginning of every snippet left to improve error reporting readability, do not remove
  "fork/either interop" should "compile correct combinations" in {
    // first example made illegal by prevent nested either blocks PR:

//    """
//    import ox.*
//    import ox.either.*
//
//    val e1: Either[Throwable, Unit] = Left(Exception("uh oh"))
//    either {
//      supervised {
//        fork {
//          either {
//            e1.ok()
//          }
//        }
//      }
//    }
//    """ should compile

    """
       import ox.*
       import ox.either.*

       supervised {
         either {
           fork {
             1
           }
         }
       }""" should compile

    """
       import ox.*
       import ox.either.*

       either {
         supervised {
           fork {
             Right(1).ok()
           }
         }
       }""" should compile

    """
       import ox.*
       import ox.either.*

       supervised {
         fork {
           either {
             Right(1).ok()
           }
         }
       }""" should compile

    // the new approach with supervised nesting prevention disallows these two examples but fixes last example in subsequent test

//    """
//       import ox.*
//       import ox.either.*
//
//       either {
//         supervised {
//           fork {
//             supervised {
//               Right(1).ok()
//             }
//           }
//         }
//       }""" should compile

//    """
//       import ox.*
//       import ox.either.*
//
//       either {
//         supervised {
//           fork {
//             supervised {
//               fork {
//                 Right(1).ok()
//               }
//             }
//           }
//         }
//       }""" should compile

    """
       import ox.*
       import ox.either.*

       either {
         supervised {
           fork {
             fork {
               Right(1).ok()
             }
           }
         }
       }""" should compile
  }

  "fork/either interop" should "fail to compile invalid examples" in {
    """
       import ox.*
       import ox.either.*

       supervised {
         either {
           fork {
             Right(1).ok()
           }
         }
       }""" shouldNot compile

    """
       import ox.*
       import ox.either.*

       supervised {
         fork {
           either {
             fork {
               Right(1).ok()
             }
           }
         }
       }""" shouldNot compile

    """
       import ox.*
       import ox.either.*

       supervised {
         either {
           fork {
             Right(1).ok()
           }
         }
       }""" shouldNot compile

    """
       import ox.*
       import ox.either.*

       supervised { // Supervised ?=>
         either { // Label ?=>
           fork { // Forked ?=>
             supervised { // Supervised ?=>
               Right(1).ok() // if forked && !supervised then error but it is forked and then supervised
             }
           }
         }
       }""" shouldNot compile
  }

  // made illegal by prevent nested either blocks PR:

//  "fork/either interop" should "work with either scopes nested in forks" in {
//    import ox.*
//    import ox.either.*
//
//    val e1: Either[Throwable, String] = Right("it works!")
//    val outer = either {
//      supervised {
//        val f1 = fork {
//          either {
//            e1.ok()
//          }
//        }
//
//        f1.join() shouldEqual Right("it works!")
//
//        "outer"
//      }
//    }
//
//    outer.shouldEqual(Right("outer"))
//  }

  "fork/either interop" should "allow ok() combinators if supervised block guarantees either block correctness" in {
    import ox.*
    import ox.either.*

    val sth: Either[Throwable, String] = Right("it works!")

    val res = either {
      supervised {
        fork {
          sth.ok()
        }.join()
      }
    }

    res.shouldEqual(Right("it works!"))
  }

  private transparent inline def assertReceivesForkedScopeError(inline code: String): Unit =
    val errs = typeCheckErrors(code)
    if !errs
        .map(_.message)
        .contains(
          "This use of .ok() belongs to either block outside of the fork and is therefore illegal. Use either block inside of the forked block."
        )
    then throw Exception(errs.mkString("\n"))

}
