package com.clairedl.scala

import scala.collection.mutable.ListBuffer

object Main extends App {
  class Person(val name: String, val father: Option[Person], val mother: Option[Person]) {
    var children =  List[Person]()

    lazy val siblings: List[Person] = {
      ??
    }

    def setChildren(c: List[Person]): Unit = {
      c match {
        case List() => print(s"$name has no children")
        case _ => {
          children = c
          // println(s"$name's children are called:")
          for (child <- c) {
            println(child.name)
          }
        }
      }
    }
  }

  val gerard = new Person("Gerard", None , None)
  val genevieve = new Person("Genevieve", None, None)
  val celine = new Person("Celine", Some(gerard), Some(genevieve))
  val franck = new Person("Franck", None, Some(new Person("Francoise", None, None)))

  celine.setChildren(List(new Person("Mael", Some(franck), Some(celine)), new Person("Loick", Some(franck), Some(celine))))
  println(celine.name)
  println(celine.father.get.name)
  println(celine.mother.get.name)
  for (child <- celine.children) println(child.name)
}
