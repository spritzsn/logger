package io.github.spritzsn.logger

import io.github.spritzsn.libuv.hrTime
import io.github.spritzsn.spritz.{HandlerResult, Request, RequestHandler, Response, responseTime}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

private val predefined =
  Map(
    "dev" -> ":method :url :status[color-coded] :response-time;ms - :res[content-length]",
    "tiny" -> ":method :url :status :res[content-length] - :response-time;ms",
  ).view mapValues parse

def apply(format: String): RequestHandler =
  val parsed =
    parse(format) match
      case List(Segment.Literal(name)) =>
        predefined.getOrElse(name, sys.error(s"pre-defined format '$name' not found"))
      case parsed => parsed

  (req: Request, res: Response) =>
    val start = hrTime

    res.action { _ =>
      val entry =
        parsed map {
          case Segment.Literal(s)                     => s
          case Segment.Token("method", _)             => req.method
          case Segment.Token("usr", _)                => req.originalUrl
          case Segment.Token("status", _)             => res.statusCode.map(_.toString) getOrElse "-"
          case Segment.Token("res", header)           => res.headers getOrElse (header.get, "-")
          case Segment.Token("response-time", digits) => responseTime(start, digits.get.toInt, false)
        } mkString

      println(entry)
    }
    HandlerResult.Next

val tokenRegex = """:(?<name>[a-zA-Z-]+);?(?:\[(?<arg>[a-zA-Z0-9-]+)])?""".r
val tokenMap =
  Map(
    "method" -> Nil,
    "url" -> Nil,
    "status" -> List("plain", "color-coded"),
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
