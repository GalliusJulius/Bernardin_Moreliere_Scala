package hello
import scala.io.Source
import java.io._

object Main_Morelie {
   /**
     * Type de n'importe quel élément de l'arbre
    */
    trait ABanimal
    /**
     * Feuille dans l'arbre qui représentent un animal
     */
    case class Animal(nom:String) extends ABanimal
    /**
    	* Noeuds dans l'arbre qui représentent des questions
     */
    case class Question(q:String, o:ABanimal, n :ABanimal) extends ABanimal
  
  /**
   * function éxécutant le jeu 
   */
  def jeuSimple(a:ABanimal, it : Iterator[String]) : Boolean = {
    a match {
      case Animal(nom:String) => {
        println("Est-ce que c'est : "+nom)
        if(it.next() == "o") true
        else false
      }
      case Question(q : String, o : ABanimal,n:ABanimal) => {
        println(q)
        if(it.next()=="o") jeuSimple(o,it)
        else jeuSimple(n,it)
      }
    }
  }
    
    /**
     * Permet d'éxécuter le jeu tout en gardant les logs des réponses du joueur dans une liste
     */    
    def jeuLog (a:ABanimal, it :Iterator[String]) : List[String] ={
      a match {
      case Animal(nom:String) => {
        if(it.next() == "o") "o"::Nil
        else "n"::Nil
      }
      case Question(q : String, o : ABanimal,n:ABanimal) => {
        if(it.next()=="o") "o"::jeuLog(o,it)
        else "n"::jeuLog(n,it)
      }
    }
    }
    
    /**
     * Permet de faire un jeu qui va ajouter les éléments qu'il ne trouve pas dans son arbre
     */
    def jeuApprentissage(a : ABanimal, it : Iterator[String]) : ABanimal = {
      a match {
      case Animal(nom:String) => {
        if(it.next() == "o") a
        else {
          println("J'ai perdu - quelle est la bonne réponse?")
          //PAS BON (TROUVER UN MOYEN) 
          val rep = it.next()
          println("Quelle question permet de différencier "+ rep +" de "+nom)
          val ques = it.next()
          println("Quelle est la réponse à cette question pour "+rep)
          if(it.next()=="y") Question(ques,Animal(rep),a)
          else Question(ques,a,Animal(rep))
        }
      }
      case Question(q : String, o : ABanimal,n:ABanimal) => {
        if(it.next()=="o") Question(q,jeuApprentissage(o,it),n)
        else Question(q,o,jeuApprentissage(n,it))
      }
    }
   }
   
      /**
     * Fonction permettant de créer un arbre par rapport à un fichier
     * TODO
     */
    def fichierToAnBanimal(nomf:String) : ABanimal = {
      def analyseListe(contenu : Iterator[String]) :ABanimal = {
        val temp = contenu.next()
        if(temp.startsWith("q:")){
          Question(temp,analyseListe(contenu),analyseListe(contenu))
        }
        else{
          Animal(temp)
        }
      }
      analyseListe(Source.fromFile(nomf).getLines)
    }
    
    def abanimalToFichier(nomf :String, a :ABanimal) : Unit ={
        def generationTexte(ani : ABanimal) :String = {
            ani match {
              case Animal(nom :String) =>{
                (nom+"\n")
              }
              case Question(q : String, oui :ABanimal,non : ABanimal) =>{
                ("q:"+q+"\n"+generationTexte(oui)+""+generationTexte(non))
              }
            }
        }
        val writer = new FileWriter(new File(""+nomf))
        writer.write(generationTexte(a))
        writer.close()
    }
    
  def main(args: Array[String]) {
    println("TestJeuSimple")
    val a = Question("Est-ce qu'il a des ailes ?",
              Question("Est-ce qu'il a des plumes ?",
                  Question("Est-ce qu'il a un goitre ?",
                    Animal("Pélican"),Animal("Pigeon")),
                  Question("Est-ce qu'il a des poils ?",
                    Animal("Chauve-souris"),Animal("Ptérodactyle"))),
            Question("Est-ce qu'il ronronne ?",
              Animal("Chat"),Animal("Chien"))) 
    //Lion
    val it = Iterator("n","o","n")
    println(jeuSimple(a,it))
    //Chien
    val it2 = Iterator("n","n","o")
    println(jeuSimple(a,it2))
    val it3 = Iterator("o","o","o","o")
    println(jeuSimple(a,it3))
    
    println("TestJeuLog")
    val it4 = Iterator("n","n","n")
    println(jeuLog(a,it4))
    println("Test apprentissage")
    println(a);
    val it5 = Iterator("n","o","n","Lion","Est-ce qu'il a une crinière?","o")
    println(jeuApprentissage(a,it5)); 
    println("Test de la création d'un fichier")
    abanimalToFichier("test2",a)
    println("Test de la création par rapport à un fichier")
    println(fichierToAnBanimal("test1"))
    
  }
}