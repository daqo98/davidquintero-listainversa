package co.s4n.listas

import scala.io.Source
import scala.collection.immutable._

sealed trait Lista
case class Vacia () extends Lista
case class Cons (i:Int, lst:Lista) extends Lista


object Main extends App{
    /*
    def longitud(lst:Lista):Int = lst match{
        case Vacia() => 0
        case Cons(i,lst) => 1+longitud()
    }
    */

    def deListALista(lst:List[Int]):Lista=lst match{
        case Nil=>Vacia()
        case (i::lstp)=>Cons(i,deListALista(lstp))
    }
    def leerArchivo(src:String):Lista = deListALista(Source.fromFile(src).getLines().toList.map(_.toInt))
        Lista 
    //def concatenar(lst1:Lista,lst2:Lista):Lista = lst1 ::: lst2

    
    def imprimirLista(lst:Lista):String = {

        def primero(lst:Lista):Int = lst match {
            case Cons(i,_) => i
        }

        def vacia(lst:Lista) = lst match {
            case Vacia() => true
            case Const(i,l) => false
        }
        
        primero(lst):Lista
    }
    

    //def invertirLista(lst:Lista):Lista = lst.reverse

    val lista=leerArchivo(args(0))
    println(lista)
    val readLista = imprimirLista(lista)
    println(readLista)
}