package hello

import org.scalatest._
import Main_Moreliere._

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
  
}
