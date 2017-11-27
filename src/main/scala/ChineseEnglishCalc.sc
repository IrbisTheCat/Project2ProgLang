val chinese: List[String] = List("ling", "yi", "er", "san", "si", "wu", "liu", "qi", "ba", "jiu", "shi")
val english: List[String] = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
val num: List[Int]=List(0,1,2,3,4,5,6,7,8,9)


/**Wrapper method for the translate
  * @param all list to be "translated" */
def go(all : List[String]): Unit= {
  val it: List[Int] = List[Int]()



  /**Translates the words list into the number list
    * @param all list to be translated
    * @param acc list in which we are going to accumulate numbers
    * @return list that we put in but now in ints
    * method checks whether index is even valid so all items with invalid index will simply be discarded*/
  def translate(all: List[String], acc: List[Int]): List[Int] = {
    all match {
      case Nil=> return acc
      case _ if (indexFinder(all.head)>(-1)) => {
         translate(all.tail, acc) ::: num(indexFinder(all.head)) :: acc
      }
      case _=>  translate(all.tail, acc)  ::: acc
    }

  }

  output(translate(all, it).reverse)
}


/**Outputs the numbers list and  calculates the sum and product of the numbers
  * @param it list that we will do caluclations on*/
def output(it: List[Int]) : Unit ={
  println("Translation: ")
  println(it.mkString(","))
  print('\n')
  println("Summation: ")
  println(it.mkString(" + "))
  print(" = ")
  print(it.foldLeft(0)(_+_))
  print('\n')
  println("Multiplication: ")
  println(it.mkString(" * "))
  print(" = ")
  print(it.foldLeft(1)(_*_))
  print('\n')



}

/**Finds the index of number in words
  * @param all word version of number
  * @return index of the number in the num list*/
def indexFinder(all:  String): Int={

  def checkChinese(str: String, idx : Int) : Int={
    if(idx==0){
      return -1
    }
    chinese(idx) match {
      case s if(s==str)=> idx
      case _=>checkChinese(str, idx-1)
    }
  }



  def checkEnglish(str: String, idx : Int) : Int={
    if(idx==0){
      return -1
    }
    english(idx) match {
      case s if(s==str)=> idx
      case _=>checkEnglish(str, idx-1)
    }
  }

  checkChinese(all, 9)+checkEnglish(all,9)+1


}





go(List("yi", "six"))
go(List("yi","nine","six","ba"))
go(List("yi", "josh","three", "si"))