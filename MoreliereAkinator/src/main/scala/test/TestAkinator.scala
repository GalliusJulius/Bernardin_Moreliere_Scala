
package test

import hello.Main_Moreliere._
import org.scalatest._
import org.scalactic.source.Position.apply
import java.io.FileNotFoundException

class TestAkinator extends FunSuite {
  
  
  val attendu = List("n","o","o")
  val nonAttendu = List("n","o","o","o")
  val arbreApprentiDepart = Question("Est-ce qu'il a des ailes ?"
                              ,Question("Est-ce qu'il a des plumes ?"
                                   ,Animal("pigeon"),Animal("chauve-souris"))
                              ,Animal("chien"))
  val arbreApprentiArrive = Question("Est-ce qu'il a des ailes ?"
                               ,Question("Est-ce qu'il a des plumes ?"
                                    ,Question("Est-ce qu'il a un goitre ?"
                                        ,Animal("Pelican"),Animal("pigeon"))
                                    ,Animal("chauve-souris"))
                                ,Animal("chien"))
                                
  val fichierAnBanimal = Question("q:Est-ce qu’il a des ailes ?",Question("q:Est-ce qu’il a des plumes ?",Animal("Pélican"),Animal("Chauve-souris")),Animal("Chien"))
  val arbreAnimalFichier = Question("Est-ce qu'il a des ailes ?",Question("Est-ce qu'il a des plumes ?",Animal("pigeon"),Animal("chauve-souris")),Animal("chien"))
  
  val arbreJnsp = Question("Est-ce qu'il a des ailes ?",Question("Est-ce qu'il a des plumes ?",Question("Est-ce qu'il a un goitre ?",Animal("Pélican"),Animal("Pigeon")),Question("Est-ce qu'il a des poils ?",Animal("Chauve-souris"),Animal("Ptérodactyle"))),Animal("Chien"))

  
  /**
   * Test si la methode jeuSimple renvoi bien vrai si l'utilisateur entre "o"
   */
  test("jeu simple vrai"){
    assert(jeuSimple(Animal("chien"),"o".lines)==true)
  }
  
  
  /**
   * Test si la methode jeuSimple renvoi bien faux si l'utilisateur entre "n"
   */
  test("jeu simple faux"){
    assert(jeuSimple(Animal("chien"),"n".lines)==false)
  }
  
 
   
  /**
   * Test le resultat de la methode jeuSimple lorsqu'on effectue une succession de réponses
   */
  test("jeu simple question"){
    assert(jeuSimple(Question("Est-ce qu'il a des ailes ?",Animal("pigeon"),Question("Est-ce qu'il ronronne ?",Animal("chat"),Animal("chien"))),"n\no\no\n".lines)==true)
  }
   
  
   /**
   * Test le resultat de jeu log avec une liste attendue
   */
    test("jeu log"){
      assert(jeuLog(Question("Est-ce qu'il a des ailes ?",Animal("pigeon"),Question("Est-ce qu'il ronronne ?",Animal("chat"),Animal("chien"))),"n\no\no\n".lines)==attendu)
    }
  
    
  /**
   * test jeu log et attend une erreur 
   */
  test("jeu log errone"){
       assert(jeuLog(Question("Est-ce qu'il a des ailes ?",Animal("pigeon"),Question("Est-ce qu'il ronronne ?",Animal("chat"),Animal("chien"))),"n\no\no\no\nn".lines)!=nonAttendu)
  }
  
  /**
   * test jeu apprentissage : ajout d'un élément (Pélican oui à la réponse Est-ce qu'il a un goitre)
   */
  test("jeu apprentissage"){
    assert(jeuApprentissage(arbreApprentiDepart,"o\no\nn\nPelican\nEst-ce qu'il a un goitre ?\no".lines)===arbreApprentiArrive)
  }
  
  /*
   * test lire un fichier qui créer l'arbre
   */
  test("fichierToAnBanimal"){
    assert(fichierToAnBanimal("test1")===fichierAnBanimal)
  }
  
  /*
   * test lire un fichier qui n'existe pas 
   */
  test("fichierToAnBanimalFaux"){
    intercept[FileNotFoundException]{
      fichierToAnBanimal("test12343")===fichierAnBanimal
    }
  }
  
  /*
   * test ecrire un arbre dans un fichier
   */
  test("AnBanimalToFichier"){
    abanimalToFichier("test3",arbreAnimalFichier)
    assert(fichierToAnBanimal("test3")===Question("q:Est-ce qu'il a des ailes ?",Question("q:Est-ce qu'il a des plumes ?",Animal("pigeon"),Animal("chauve-souris")),Animal("chien")))
  }
  
  /*
   * test le mode de jeu 'je ne sais pas'
   */
  
   test("je ne sais pas"){
     assert(jeuSimpleJnsp(arbreJnsp,"o\nrand\njnsp\nn\no\no\no\n".lines)==true)
   }
  
}