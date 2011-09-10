package com.softmemes.napl.testapp

import com.softmemes.napl._

object Main extends App {
  val l = NaplExpression[Unit](Unit, ValueExpression[Unit](StringValue("hello")))
}