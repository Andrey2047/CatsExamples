package com.andrey2047.core

import cats.data.Reader

object ReaderMonadExample {

  case class Course(desc: String, code: String)

  class AuthService {
    def isAuthorised(userName: String): Boolean = userName.startsWith("J")
  }

  class CourseService {
    def register(course: Course, isAuthorised: Boolean, name: String) = {
      if (isAuthorised)
        s"User $name registered for the course: ${course.code}"
      else
        s"User: $name is not authorised to register for course: ${course.code}"
    }
  }

  def isAuthorised = Reader[CourseManager, Boolean]{ courseMgr =>
    courseMgr.authService.isAuthorised(courseMgr.userName)
  }

  def register(isFull: Boolean) = Reader[CourseManager, String] { courseMgr =>
    courseMgr.courseService.register(courseMgr.course,
      isFull,
      courseMgr.userName)
  }

  case class CourseManager(course: Course,
      userName: String,
      authService: AuthService,
      courseService: CourseService)

  def main(args: Array[String]): Unit = {

    val upper = Reader((text: String) => text.toUpperCase)
    val greet = Reader((name: String) => s"Hello $name")

    val comb1 = upper.compose(greet)
    val comb2 = upper.andThen(greet)
    val result = comb2.run("Bob")
    println(result) // prints Hello Bob


    val result2 = for {
      authorised <- isAuthorised
      response <- register(authorised)
    } yield response


    val course = Course("Computer Science", "CS01")
    val report = result2.run(CourseManager(course, "Jon", new AuthService, new CourseService))

    println(report)

  }



}
