// COSC 455 - Programming Languages: Implementation and Design
// Project 2

// NAME: <Anna Shchelokova>


// Test Cases
val pTest1: List[Int] = List (1, 1, 1, 1, 0)
val qTest1: List[Int] = List(1, 0, 1, 1)
val test1ExectedSolution: List[Int] = List(1, 0, 1, 0, 0, 1)

val pTest2: List[Int] = List (1, 0, 0, 1, 1, 0, 1)
val qTest2: List[Int] = List(1, 0, 0, 1, 0)
val test2ExectedSolution: List[Int] = List(1, 0, 1, 1, 1, 1, 1)

val pTest3: List[Int] = List (1, 0, 0, 1, 0, 0, 1)
val qTest3: List[Int] = List(1, 1, 0, 0, 1)
val test3ExectedSolution: List[Int] = List(1, 1, 0, 0, 0, 1, 0)

val pTest4: List[Int] = List (1, 0, 0, 0, 1, 1, 1)
val qTest4: List[Int] = List(1, 0, 1, 1, 0)
val test4ExectedSolution: List[Int] = List(1, 0, 1, 1, 1, 0, 1)

val test5ExectedSolution: List[Int] = List(1, 1, 1, 0, 1, 1)
val test6ExectedSolution: List[Int] = List(1, 1, 0, 0, 0, 1)



def helper( z:List[(Boolean, Boolean)], cb:Boolean, acc: List[Boolean], ignoreCb: Boolean = false): List[Boolean] = {
  z match {
    case hvost::Nil => {
      if(ignoreCb) return acc
      hvost match  {
        case (true, true) if cb => return true::true::acc
        case (false, true) if cb => return true::false::acc
        case (true, false) if cb => return true::false::acc
        case (false, false) if cb => return true::acc
        case (true, true) if !cb => return true::false::acc
        case (false, true) if !cb => return true::acc
        case (true, false) if !cb => return true::acc
        case (false, false) if !cb => return false::acc
      }
    }
    case _ => helper( z.tail, getNextCarryBit(z.head._1,z.head._2,cb), addBits(z.head._1,z.head._2,cb)::acc, ignoreCb )
  }
}


// This function does the binary addition when there are uneven lists and still must
// finish the add with the carry bits.
def finishBinaryAdd(remainingBits: List[Boolean], carryBit: Boolean): List[Boolean] = ???

// This function determines what the next carry bit should be based on current bits.
def getNextCarryBit(pBit: Boolean, qBit: Boolean, carryBit: Boolean): Boolean = {
  carryBit match {
    case true => (pBit || qBit)
    case false => pBit && qBit
  }
}

// This function does the binary addition of two Booleans and a carry bit.
def addBits(pBit: Boolean, qBit: Boolean, carryBit: Boolean): Boolean = {
  (pBit ^ qBit) ^ carryBit
}

// This function does the binary addition of two boolean lists. Note that the lists may not be equal in length.
def doBinaryAddition(pBits: List[Boolean], qBits: List[Boolean], carryBit: Boolean): List[Boolean] = {
  val zip = pBits.zipAll(qBits, false, false)

  val result = helper(zip, carryBit, Nil)

  result
}

// This function converts a binary integer list into its corresponding boolean list.
def convertIntListToBooleanList(intList: List[Int]) ={
  intList.map(x => if(x == 1) true else false)
}

// This function converts a boolean list into its corresponding binary integer list.
def convertBooleanListToIntList(booleanList: List[Boolean]) = {
  booleanList.map(x=> if(x==true) 1 else 0)
}

/* This is the "main" function to do binary addition. This function should:
    1. Convert the input parameter lists from integers to boolean. Use Scala reverse
    2. Reverse the lists (since binary addition is performed right to left). Use Scala reverse.
    3. Perform the binary addition with the doBinaryAddition function.
    4. Reverse the lists (to get back in proper order). Use Scala reverse.
    5. Convert the answer back to binary integer form for output.
  Note that the initial carry bit is assumed to be 0 (i.e., false).
*/
def binaryAddition(pList: List[Int], qList: List[Int]): List[Int] = {
  val aList=convertIntListToBooleanList(pList).reverse
  val bList=convertIntListToBooleanList(qList).reverse

  val tmp = convertBooleanListToIntList(doBinaryAddition(aList, bList, false))
  return tmp
}


def binarySubtraction(pList: List[Int], qList: List[Int]) : List[Int] = {
  val aList=convertIntListToBooleanList(pList).reverse
  val bList=convertIntListToBooleanList(qList).reverse.map(x=> if(x==true) false else true)
  val list = List(true)
  // Set MSB (most significant bit to 1) ((not sure this is correct))
  val tempo=(true::doBinaryAddition(bList, list, false)).reverse

  // Can't just use addition - we have to ignore final carry bit in case of substraction (last param of helper)
  val zip = aList.zipAll(tempo, false, false)
  val result = helper(zip, false, Nil, true)

  return convertBooleanListToIntList(result)
}

// Testing binary addition.
if (binaryAddition(pTest1, qTest1).equals(test1ExectedSolution)) println("Test 1 passes!") else println("Test 1 fails.")
if (binaryAddition(pTest2, qTest2).equals(test2ExectedSolution)) println("Test 2 passes!") else println("Test 2 fails.")
if (binaryAddition(pTest3, qTest3).equals(test3ExectedSolution)) println("Test 3 passes!") else println("Test 3 fails.")
if (binaryAddition(pTest4, qTest4).equals(test4ExectedSolution)) println("Test 4 passes!") else println("Test 4 fails.")

// Testing binary subtraction
if (binarySubtraction(pTest2, qTest2).equals(test5ExectedSolution)) println("Test 5 passes!") else println("Test 5 fails.")
if (binarySubtraction(pTest4, qTest4).equals(test6ExectedSolution)) println("Test 6 passes!") else println("Test 6 fails.")