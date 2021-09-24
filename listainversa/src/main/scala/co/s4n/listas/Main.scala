package co.s4n.listas

import scala.io.Source
import scala.collection.immutable._

sealed trait Lista
case class Vacia () extends Lista
//case object Vacia extends Lista //Así debió haberse implementado
case class Cons (i:Int, lst:Lista) extends Lista



object Main extends App{
    /*
    def longitud(lst:Lista):Int = lst match{
        case Vacia() => 0
        case Cons(i,lstp) => 1+longitud(lstp)
    }
    */

    def deListALista(lst:List[Int]):Lista=lst match{
        case Nil=>Vacia()
        case (i::lstp)=>Cons(i,deListALista(lstp))
    }
    def leerArchivo(src:String):Lista = deListALista(Source.fromFile(src).getLines().toList.map(_.toInt))
         
    def concatenar(lst1:Lista,lst2:Lista):Lista = {
        // Convirtiendo datos tipo Lista a tipo List[Int]
        def ListaAsListInt(lst:Lista):List[Int] = lst match {
            case Vacia() => List()
            case Cons(i,Vacia()) => List(i)
            case Cons(i, lstp) => (List(i)) ::: ListaAsListInt(lstp)
        }

        val Lista1 = ListaAsListInt(lst1)
        val Lista2 = ListaAsListInt(lst2)
        val ListaFull = Lista1 ::: Lista2
        deListALista(ListaFull)
    }

    
    def imprimirLista(lst:Lista):String = {
        //Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Vacia())))))

        def vacia(lst:Lista) = lst match {
            case Vacia() => true
            case Cons(i,l) => false
        }
        // Extrae entero y va construyendo el string. El [ lo coloco al final
        def extractIntAsString(lst:Lista):String = lst match {
            case Vacia() => ""
            case Cons(i,Vacia()) => s"$i]"
            case Cons(i, lstp) => s"$i,".concat(extractIntAsString(lstp))
        }

        val string = extractIntAsString(lst)
        val string2:String = "[".concat(string)
        if (vacia(lst)) "[]" else string2
    }
    

    def invertirLista(lst:Lista):Lista = {
        // Extrae entero y va construyendo la List[Int].
        def InvertAsListInt(lst:Lista):List[Int] = lst match {
            case Vacia() => List()
            case Cons(i,Vacia()) => List(i)
            case Cons(i, lstp) => InvertAsListInt(lstp) ::: (List(i))
        }

        val ListaInvert = InvertAsListInt(lst)
        deListALista(ListaInvert)
    }

    val lista=leerArchivo(args(0))
    println(lista)

    val readLista = imprimirLista(lista)
    println(readLista)

    val invertLista = invertirLista(lista)
    println(invertLista)
    println(imprimirLista(invertLista))

    val concatLista = concatenar(lista,invertLista)
    println(concatLista)
    println(imprimirLista(concatLista))
}