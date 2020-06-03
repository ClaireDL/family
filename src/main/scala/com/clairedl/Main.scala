package com.clairedl.scala

import scala.collection.mutable.ListBuffer

object Main extends App {
  abstract class FamilyTree(members: List[Person])

  case class Person(name: String) {
    def parents(mother: Person, father: Person): String = {
      s"${mother.name} and ${father.name} are $name's parents."
    }

    var siblings: List[Person] = List()

    var children: List[Person] = List()

    def hasChildren(c: List[Person]): List[Person] = {
      for (child <- c) {
        val childSiblings = c.filter(x => x != child)
        child.siblings = childSiblings
      }
      children = c
      c
    }

    def grandchildren: List[Person] = {
      var g = ListBuffer[Person]()
      for {
        child <- children
        grandchild <- child.children
      } g += grandchild
      g.toList
    }

    // def cousin: List[Person] = {

    // }
  }

  val gerard = Person("Gerard")
  val genevieve = Person("Genevieve")
  val ce = Person("celine")
  val mael = Person("Mael")
  val loick = Person("Loick")
  ce.hasChildren(List(mael, loick))
  val cl = Person("claire")
  val jd = Person("jean-do")
  val olivia = Person("Olivia")
  val andre =  Person("Andre")
  jd.hasChildren(List(olivia, andre))
  gerard.hasChildren(List(ce, cl, jd))

  println(s"List of ${gerard.name}'s' children")
  println(gerard.children.map(_.name))
  println(s"List of ${gerard.name}'s grandchildren")
  println(gerard.grandchildren.map(_.name))
  println(s"List of ${ce.name}'s' siblings")
  println(ce.siblings.map(_.name))
  println(s"List of ${gerard.name}'s' siblings")
  println(gerard.siblings.map(_.name))
  genevieve.siblings = List(Person("Michel"), Person("Francis"), Person("Agnes"))
  println(Person("Agnes").siblings.map(_.name))
}
