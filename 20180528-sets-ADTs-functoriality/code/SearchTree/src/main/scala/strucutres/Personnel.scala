package strucutres

import strucutres.GenericSet.{Set, Empty, adjoin, contains_?, union, map}

object Personnel {

  /**
    * The base type inherits traits Product and Serializable, which are common to all case classes.
    * This explicit inheritance is helpful to simplify the use of implicit Ordering.
    */
  sealed trait Person extends Product with Serializable

  case class Contractor(name: String) extends Person

  case class Employee(name: String, employeeNumber: Int) extends Person


  /**
    * Ordering for sum type Person that may be either Contractor or Employee
    */
  implicit val ordPerson: Ordering[Person] = Ordering.by({
    case Contractor(name) => name
    case Employee(name, en) => name + "/" + en
  })

  /**
    * Ordering for Contractors.
    */
  implicit val ordContractor: Ordering[Contractor] = Ordering.by(e => e.name)

  /**
    * Ordering for Employees.
    */
  implicit val ordEmployee: Ordering[Employee] = Ordering.by(e => e.employeeNumber)


  def main(args: Array[String]): Unit = {
    println("Let's add some personnel.")

    val contractors = adjoin(Contractor("Tomas"),
                      adjoin(Contractor("Andrew"),
                      adjoin(Contractor("James"), Empty))) // implicitly uses ordContractor

    val employees = adjoin(Employee("Tomas", 100),
                    adjoin(Employee("James", 102),
                    adjoin(Employee("Andrew", 101), Empty))) // implicitly uses ordEmployee

    val all = union(employees, contractors) // implicitly uses ordPerson

    println(contractors)
    println(employees)
    println(all)
  }

}
