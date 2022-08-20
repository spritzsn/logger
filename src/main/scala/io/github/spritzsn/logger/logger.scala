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

val tokenRegex = """:(?<name>[a-zA-Z-]+);?(?:\[(?<arg>[a-zA-Z0-9-]+)])?""".r
val tokenMap =
  Map(
    "method" -> Nil,
    "url" -> Nil,
    "status" -> Nil,
    "date" -> List("iso", "web"),
    "req" -> null,
    "res" -> null,
    "response-time" -> List("3", "0", "1", "2"),
    "user-agent" -> Nil,
    "remote-addr" -> Nil,
    "referrer" -> Nil,
    "http-version" -> Nil,
  )

def parse(format: String): List[Segment] =
  val buf = new ListBuffer[Segment]
  var idx = 0

  tokenRegex.findAllMatchIn(format) foreach { m =>
    def add(tok: Segment.Token): Unit =
      if m.start > idx then buf += Segment.Literal(format.substring(idx, m.start))
      buf += tok
      idx = m.end

    val name = m.group("name")
    val arg = m.group("arg")

    tokenMap get name match
      case Some(null) if arg == null => sys.error(s"logger: token '$name' requires an argument")
      case Some(null)                => add(Segment.Token(name, Some(arg)))
      case Some(Nil) if arg != null  => sys.error(s"logger: token '$name' doesn't accept any argument: found $arg")
      case Some(Nil)                 => add(Segment.Token(name, None))
      case Some(args) if arg == null => add(Segment.Token(name, Some(args.head)))
      case Some(args) if args contains arg => add(Segment.Token(name, Some(arg)))
      case Some(args) =>
        sys.error(s"logger: token '$name' doesn't accept argument '$arg': can be one of ${args mkString ","}")
      case None => sys.error(s"logger: token '$name' not recognized")
  }

  if format.length > idx then buf += Segment.Literal(format.substring(idx))
  buf.toList

enum Segment:
  case Token(name: String, arg: Option[String]) extends Segment
  case Literal(s: String) extends Segment
