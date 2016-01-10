package controllers


import org.joda.time.DateTime
import play.api._
import play.api.mvc._
import scala.io.Source
import play.api.Play.current
import play.twirl.api.Html
import java.io.File

object Application extends Controller {

  val chemin_contenu = ""
  val chemin_appli = Play.application.path.toString

  def liste_realisations : Seq[String] = {
    val place_realisations = "contenu/realisations"

    val real_dir = new File(chemin_appli +"/" + place_realisations)
//    val realisations_dir = Play.getFile("public/" + place_realisations)

    val dirs_real = real_dir.listFiles().toSet.filter(_.isDirectory).toSeq.sorted
//    val dirs_realisations = realisations_dir.listFiles().toSet.filter(_.isDirectory).toSeq.sorted

    val reals = dirs_real.map(x => {
      val liste_images = x.listFiles().toSet.filter(_.getName.endsWith(".jpg")).toSeq.sorted
      if (liste_images.isEmpty) "" else place_realisations + "/" + x.getName + "/" + liste_images(0).getName
    }).filter(_ != "")
//    val realisations = dirs_realisations.map(x => {
//      val liste_images = x.listFiles().toSet.filter(_.getName.endsWith(".jpg")).toSeq.sorted
//      if (liste_images.isEmpty) "" else place_realisations + "/" + x.getName + "/" + liste_images(0).getName
//    }).filter(_ != "")

    return reals
//    return realisations
  }

  def construit_carousel(place_carousel: String) : Seq[(String, Object)] = {
//    val carousel_dir = Play.getFile("public/" + place_carousel)
    val car_dir = new File(chemin_appli + "/" + place_carousel)

//    val images_carousel =  carousel_dir.listFiles().toSet.filter(_.getName.endsWith(".jpg")).toSeq.sorted
    val images_car = car_dir.listFiles.toSet.filter(_.getName.endsWith(".jpg")).toSeq.sorted

//    val carousel = images_carousel.map(x => {
//      val nom_image = place_carousel + "/" + x.getName
//      val fic_texte = Play.getExistingFile("public/" + nom_image.replaceAllLiterally("jpg", "html"))
//      if (fic_texte == None)
//        (nom_image, x.getName)
//      else
//        (nom_image, Html(Source.fromFile(fic_texte.get).mkString))
//    })

    val car = images_car.map(x => {
      val nom_image = place_carousel + "/" + x.getName
      val fic_texte = new File(chemin_appli + "/" + nom_image.replaceAllLiterally("jpg", "html"))
      if (fic_texte.exists)
        (nom_image, Html(Source.fromFile(fic_texte).mkString))
      else
        (nom_image, x.getName)
    })

    return car
//    return carousel
  }

  def index = Action { implicit request =>

    val place_panneaux = "contenu/accueil/panneaux"

//    val panneaux_dir = Play.getFile("public/" + place_panneaux)
    val pan_dir = new File(chemin_appli + "/" + place_panneaux)

//    val fic_panneaux = panneaux_dir.listFiles().toSet.filter(_.getName.endsWith(".html")).toSeq.sorted
    val fic_pan = pan_dir.listFiles.toSet.filter(_.getName.endsWith(".html")).toSeq.sorted

//    val panneaux = fic_panneaux.map(x => {
//      val nom_panneau = place_panneaux + "/" + x.getName
//      val fic_titre = Play.getExistingFile("public/" + nom_panneau.replaceAllLiterally("html","txt"))
//      if (fic_titre == None)
//        (Html(Source.fromFile(x).mkString), "Titre absent")
//      else
//        (Html(Source.fromFile(x).mkString), Source.fromFile(fic_titre.get).mkString)
//    })

    val pan = fic_pan.map(x => {
      val nom_panneau = place_panneaux + "/" + x.getName
      val fic_titre = new File(chemin_appli + "/" + nom_panneau.replaceAllLiterally("html","txt"))
      if (fic_titre.exists)
        (Html(Source.fromFile(x).mkString), Source.fromFile(fic_titre).mkString)
      else
        (Html(Source.fromFile(x).mkString), "Titre absent")
    })

    Ok(views.html.index(
      construit_carousel("contenu/accueil/carousel"),
//      panneaux,
      pan,
      liste_realisations))
  }

  def about = Action { implicit request =>
    Ok(views.html.about(chemin_contenu, chemin_appli))
  }

  def services = Action { implicit request =>
    Ok(views.html.services())
  }

  def portefeuille = Action {implicit request =>
    Ok(views.html.portefeuille())
  }

  def realisation(real: String) = Action { implicit request =>

    val fic_desc = Play.getExistingFile("public/contenu/realisations/" + real + "/description.html")
    val description = {
      if (fic_desc == None)
        Html("<h2>Description manquante</h2>")
      else
        Html(Source.fromFile(fic_desc.get).mkString)
    }

    val fic_liees = Play.getExistingFile("public/contenu/realisations/" + real + "/realisations_liees.txt")
    val filtre_real = {
      if (fic_liees == None)
        "aucune"
      else
        ".*" + Source.fromFile(fic_liees.get).getLines.mkString(".*|.*") +".*"
    }

    Ok(views.html.realisation(
      construit_carousel("contenu/realisations/" + real),
      description,
      liste_realisations.filter(_.matches(filtre_real))))
  }

}