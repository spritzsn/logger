package io.github.spritzsn.logger

import io.github.spritzsn.spritz.{Request, Response, Server, responseTime}

@main def run(): Unit =
  Server("ExampleServer/1.0") { app =>
    app
      .use(apply("dev"))
      .get("/", (_: Request, res: Response) => res.send("hello"))
      .post("/", (req: Request, res: Response) => res.send(req.body))
    app.listen(3000)
    println("listening")
  }
