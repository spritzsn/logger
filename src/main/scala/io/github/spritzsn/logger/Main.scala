package io.github.spritzsn.logger

import cps.*
import cps.monads.FutureAsyncMonad

import io.github.spritzsn.async.loop
import io.github.spritzsn.fs.*
import io.github.spritzsn.libuv.*

@main def run(): Unit =

//  async {
//    val fd = await(open("asdf", O_WRONLY | O_CREAT, S_IRWXU))
//
//    await(fd.write("one\n"))
//    await(fd.write("two\n"))
//    fd.close()
//  }