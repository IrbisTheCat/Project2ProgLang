

def prime(it:Int):Boolean = {

    def test(t:Int):Boolean = {
        if(t==1) return true;
        return (it % t) != 0 && test(t-1)
    }

    test(it-1)
}

prime(7)
prime(8)

def twinprimes (it: Int, it2 : Int)= prime(it) && prime(it2) && (Math.abs(it-it2)==2)

var list: List[Int] = List[Int]()


def twinprimesList(n: Int): List[Int] = {

    val q : List[Int] = (4 to n) toList

    (q.foldLeft(List[Int]()) ( (accum, value) => if( prime(value) && prime(value-2)) value::(value-2)::accum else accum) distinct).reverse

}


twinprimesList(50)

def goldbach(it : Int) : Unit = {
    it match {
        case z if z < 3 => Nil
        case z if !isEven(z) => Nil
        case _ => {
           val (t,b)=helper(it-2, 2)
           println(t+" + "+b+" = "+it+"\n")

        }
    }
}

def helper(it :Int, other: Int) : (Int, Int)={
    if(prime(it) && prime(other)) return (it,other)
    return helper( it-1, other+1)
}

def isEven(it :Int) : Boolean={
    it%2==0
}


goldbach(28)

