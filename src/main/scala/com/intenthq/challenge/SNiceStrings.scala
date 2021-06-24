package com.intenthq.challenge

import scala.annotation.tailrec

object SNiceStrings {

// From http://adventofcode.com/day/5
//  --- Day 5: Doesn't He Have Intern-Elves For This? ---
//
//  Santa needs help figuring out which strings in his text file are naughty or nice.
//
//    A nice string is one with all of the following properties:
//
//    It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
//  It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
//    It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
//    For example:
//
//    ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
//  aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
//    jchzalrnumimnmhp is naughty because it has no double letter.
//    haegwjzuvuyypxyu is naughty because it contains the string xy.
//    dvszwmarrgswjxmb is naughty because it contains only one vowel.
//    How many strings are nice?

  private lazy val badStrings = List("ab", "cd", "pq", "xy")

  def nice(xs: List[String]): Int = {
    xs.foldLeft(0)((count, str) => {
      if (isNice(str)) count + 1
      else count
    })
  }

  private def isNice(str: String): Boolean = {
    @tailrec
    def threeVowels(str: String, count: Int): Boolean = {
      if (count >= 3) true
      else if (str.isEmpty) false
      else str.head match {
        case 'a' | 'e' | 'i' | 'o' | 'u' => threeVowels(str.tail, count + 1)
        case _ => threeVowels(str.tail, count)
      }
    }

    @tailrec
    def doubleLetters(ls: List[String]): Boolean = {
      if (ls.isEmpty) false
      else {
        val str = ls.head
        if (str.charAt(0) == str.charAt(1)) true
        else doubleLetters(ls.tail)
      }
    }

    def noBadStrings(str: String): Boolean = {
      !badStrings.exists(str.contains)
    }

    threeVowels(str, 0) && doubleLetters(str.sliding(2).toList) && noBadStrings(str)
  }
}
