package com.clairedl.scala

import scala.collection.mutable.ListBuffer

object Main extends App {


  case class Person(name: String, isMale: Boolean) {
    // Getters are used because the only relationship set by the user is that of a person
    // who has children with another person
    private var _children: ListBuffer[Person] = new ListBuffer[Person]()
    def children: List[Person] = this._children.toList

    private var _siblings: ListBuffer[Person] = new ListBuffer[Person]()
    def siblings: List[Person] = this._siblings.toList

    private var _father: Option[Person] = None
    def father: Person = {
      _father match {
        case None           => Person("Unknown", true)
        case Some(_father)  => this._father.get
      }
    }

    private var _mother: Option[Person] = None
    def mother: Person = {
      _mother match {
        case None           => Person("Unknown", false)
        case Some(_mother)  => this._mother.get
      }
    }

    private var _paternalGrandParents: ListBuffer[Person] = new ListBuffer[Person]()
    def paternalGrandParents: List[Person] = this.paternalGrandParents.toList

    /**
    * Sets relationships based on 2 people and their children
    * returns siblings, father and mother for each children
    */
    def setBasicRelationships(person: Person, otherParent: Person, children: List[Person]): Unit = {
      for ( child <- children ) {
        // Assigns siblings to each child
        val childSiblings = children.filter(x => x != child)
        childSiblings.foreach(child._siblings += _)
        // Assigns children to current person
        _children += child
        // Assigns children to other parent
        otherParent._children += child
        if ( person.isMale ) {
          child._father = Option(person)
          child._mother = Option(otherParent)
        }
        else {
          child._mother = Option(person)
          child._father = Option(otherParent)
        }
      }
    }

    def hasChildren(otherParent: Person, children: List[Person]): Unit = {
      setBasicRelationships(Person(this.name, this.isMale), otherParent, children)
    }
  }

  case class FamilyTree(person: Person) {
    def printRelationships(): Unit = {
      println(findFather(person))
    }

    def findFather(p: Person): Unit = p.father.name match {
      case "Unknown" => p.father.name
      case _         => findFather(p.father.father)
    }

  }

  val gerard = Person("Gerard", true)
  val ge = Person("Genevieve", false)
  val ce = Person("Celine", false)
  val cl = Person("Claire", false)
  val jd = Person("Jean-Do", true)
  println(s"${jd.name}'s father is called: ${jd.father.name}")
  gerard.hasChildren(ge, List(ce, jd, cl))

  val pauline = Person("Pauline", false)
  val olivia = Person("Olivia", false)
  val andre = Person("Andre", true)

  jd.hasChildren(pauline, List(olivia, andre))
  val mael = Person("Mael", true)
  val loick = Person("Loick", true)

  println(s"${jd.name}'s father is called: ${jd.father.name}")
  println(s"${ge.name}'s children are called: ${ge.children.map(x => x.name).mkString(", ")}")
  println(s"${jd.name}'s siblings are called: ${jd.siblings.map(x => x.name).mkString(", ")}")
  println(s"${olivia.name}'s siblings are called: ${olivia.siblings.map(x => x.name).mkString(", ")}")
  println(s"${andre.name}'s father is called: ${andre.father.name}")

  val elders = println(FamilyTree(andre).findFather(andre))
}
