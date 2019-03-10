package hello
import scala.io.Source
import java.io._

/**
 * Programme principale du scala
 */
object Main_Moreliere {
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
   * Question 1 : function éxécutant le jeu de manière simple
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
     * Question 2 : Permet d'éxécuter le jeu tout en gardant les logs des réponses du joueur dans une liste
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
     * Question 3 : Permet de faire un jeu qui va ajouter les éléments qu'il ne trouve pas dans son arbre quand il ne trouve pas
     */
    def jeuApprentissage(a : ABanimal, it : Iterator[String]) : ABanimal = {
      a match {
      case Animal(nom:String) => {
        println("Est-ce que c'est "+nom+"?")
        if(it.next() == "o") a
        else {
          println("J'ai perdu - quelle est la bonne réponse?") 
          val rep = it.next()
          println("Quelle question permet de différencier "+ rep +" de "+nom)
          val ques = it.next()
          println("Quelle est la réponse à cette question pour "+rep)
          if(it.next()=="o") Question(ques,Animal(rep),a)
          else Question(ques,a,Animal(rep))
        }
      }
      case Question(q : String, o : ABanimal,n:ABanimal) => {
        println(q)
        if(it.next()=="o") Question(q,jeuApprentissage(o,it),n)
        else Question(q,o,jeuApprentissage(n,it))
      }
    }
   }
   
     /**
     * Question 4 : Fonction permettant de créer un arbre par rapport à un fichier
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
    
    /**
     * Question 5 : Fonction qui va générer un fichier nommé nomf à partir d'un arbre d'animal
     */
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
    
    
    /**
     * Question 6 : Fonction implémentant la fonction je ne sais pas 
     */
    def jeuSimpleJnsp(a : ABanimal, it :Iterator[String]) :Boolean = {
      def generationSousArbres(ls :List[ABanimal], it :Iterator[String]) : Boolean ={
        ls match {
          case Question(q : String,o:ABanimal,n:ABanimal)::_ => {
            println(q)
            val res = it.next()
            if(res=="o") generationSousArbres(o::Nil,it)
            else if(res=="n")  generationSousArbres(n::Nil,it)
            else generationSousArbres(List(o,n),it)
          }
          case Animal(nom:String)::n =>{
            println("Pensez-vous à :"+nom+" ?")
            if(it.next=="o"){
               printf("J'ai gagné")
               true
            }
            else{
               generationSousArbres(n,it)
            }
          }
          case Nil =>{
            println("J'ai perdu!")
            false
          }
        }
      }
      generationSousArbres(a::Nil,it)
    }
   
   /**
    * Fonction permettant de lancer le jeu en boucle avec un mode défini 
    */
   def jouerJeu(a: ABanimal, it:Iterator[String],mode : String) : Unit = {
        println("Début de la partie")
        //on cherche le mode actuel
        mode match{
          case "1" =>{ val arbre = jeuApprentissage(a,Source.stdin.getLines)}
          case "2" =>{ val trouve = jeuSimple(a,it)
                       if(trouve) println("J'ai trouvé :D")
                       else println("Oh nan je n'ai pas trouvé!")
                       }       
          case "3" =>{ 
                 val trouve = jeuSimpleJnsp(a,it)
                       if(trouve) println("J'ai trouvé :D")
                       else println("Oh nan je n'ai pas trouvé!")
            }
        }
        //fin d'éxécution
        println("Voulez-vous continuer?")
        if(it.next()=="o"){
            println("Voulez vous changer de mode?")
            if(it.next()=="o") jouerJeu(a,Source.stdin.getLines,changerMode(it))
            else jouerJeu(a,Source.stdin.getLines,""+mode)
        }
        else println("Merci d'avoir joué!")
    }
   
   /**
    * Méthode permettant de changer le mode
    */
   def changerMode(it:Iterator[String]) : String ={
     println("Quel mode voulez-vous jouer? Apprentissage, simple, je sais pas (1,2,3)")
     it.next()
   }
    
   /**
    * Main qui lance le jeu tant que l'utilisateur veut jouer
    */
   def main(args: Array[String]) {
     println("Bienvenue dans le jeu akinator")
     val arbre = fichierToAnBanimal("test2")
     jouerJeu(arbre,Source.stdin.getLines,changerMode(Source.stdin.getLines))
  }
}