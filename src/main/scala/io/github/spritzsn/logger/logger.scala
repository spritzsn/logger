package io.github.spritzsn.logger

import io.github.spritzsn.libuv.hrTime
import io.github.spritzsn.spritz.{HandlerResult, Request, RequestHandler, Response, responseTime}

import scala.collection.mutable.ListBuffer

def apply(): RequestHandler =
  (req: Request, res: Response) =>
    val start = hrTime

    res.action { _ =>
      val rt = responseTime(start, 3, true)
    }
    HandlerResult.Next

val tokenRegex = """:(?<name>[a-zA-Z]+)(?:\[(?<arg>[a-zA-Z0-9]+)])?""".r
val tokenMap =
  Map(
    "method" -> Nil,
    "url" -> Nil,
    "date" -> List("iso", "web"),
  )

def parse(format: String): List[Segment] =
  val buf = new ListBuffer[Segment]

  tokenRegex.findAllMatchIn(format) foreach { m =>
    val name = m.group("name")
    val arg = m.group("arg")

    tokenMap get name match
      case Some(Nil)                        => Segment.Token(name, None)
      case Some(args) if arg == null        => Segment.Token(name, Some(args.head))
      case Some(args) if args contains args => Segment.Token(name, Some(arg))
      case Some(args) => sys.error(s"logger: token '$name' doesn't accept argument '$arg' - can be $args")
  }

enum Segment:
  case Token(name: String, arg: Option[String]) extends Segment
  case Literal(s: String) extends Segment
