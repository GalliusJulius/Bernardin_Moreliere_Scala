package hello
import Main_Moreliere._

object FeuilleTravail {
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
    println(fichierToAnBanimal("test2"))
    val it6 = Iterator("n","x","n","n")
    println("Test pour la génération du je sais pas")
    println(jeuSimpleJnsp(a,it6))
  
}