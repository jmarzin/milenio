package controllers

import play.api._
import play.api.mvc._
import scala.io.Source
import play.api.Play.current
import play.twirl.api.Html
import java.io.File

object Application extends Controller {

  val chemin_contenu = ""
  val chemin_appli = Play.application.path.toString

  def liste_realisations(place: String) : Seq[(String, String, String, Html)] = {
    val place_realisations = s"contenu/$place"

    val real_dir = new File(chemin_appli +"/" + place_realisations)

    val dirs_real = real_dir.listFiles().toSet.filter(_.isDirectory).toSeq.sorted

    val reals = dirs_real.map(x => {
      val liste_images = x.listFiles().toSet.filter(_.getName.endsWith(".jpg")).toSeq.sorted
      val fic_titre = new File(x + "/titre.txt")
      val titre = if (fic_titre.exists) Source.fromFile(fic_titre).mkString else "Titre non disponible"
      val fic_resume = new File(x + "/resume.txt")
      val resume = if (fic_resume.exists) Source.fromFile(fic_resume).mkString else "Résumé non disponible"
      val fic_desc = new File(x + "/description.html")
      val description = if (fic_desc.exists) Html(Source.fromFile(fic_desc).mkString) else Html("")
      if (liste_images.isEmpty) ("",titre,resume,description) else (place_realisations + "/" + x.getName + "/" + liste_images.head.getName, titre, resume, description)
    }).filter(_._1 != "")

    reals
  }

  def construit_carousel(place_carousel: String) : Seq[(String, Html, String)] = {
    val car_dir = new File(chemin_appli + "/" + place_carousel)

    val images_car = car_dir.listFiles.toSet.filter(_.getName.endsWith(".jpg")).toSeq.sorted

    val car = images_car.map(x => {
      val nom_image = place_carousel + "/" + x.getName
      val fic_texte = new File(chemin_appli + "/" + nom_image.replaceAllLiterally("jpg", "html"))
      var texte_html = Html(x.getName)
      if (fic_texte.exists)
        texte_html = Html(Source.fromFile(fic_texte).mkString)
      val nom_panorama = place_carousel + "/" + x.getName.replaceAllLiterally(".jpg",".pan")
      if (new File(chemin_appli + "/" + nom_panorama).exists)
        (nom_image, texte_html, nom_panorama)
      else
        (nom_image, texte_html, "")
    })


    car
  }

  def index = Action { implicit request =>

    val place_panneaux = "contenu/accueil/panneaux"

    val pan_dir = new File(chemin_appli + "/" + place_panneaux)

    val fic_pan = pan_dir.listFiles.toSet.filter(_.getName.endsWith(".html")).toSeq.sorted

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
      pan,
      liste_realisations("realisations").map(_._1).filter(x => {
        val nom = x.split('/').drop(2)(0)
        nom.toUpperCase == nom
      })))
  }

  def about = Action { implicit request =>
    val fic_a_propos = new File(s"$chemin_appli/contenu/a_propos/a_propos.html")
    val a_propos = if (fic_a_propos.exists) Html(Source.fromFile(fic_a_propos).mkString) else Html("")
    Ok(views.html.about(a_propos))
  }

  def services = Action { implicit request =>
    Ok(views.html.services(
      "contenu/services/services.jpg",
      liste_realisations("services").map(x => (x._1,x._2,x._4))
    ))
  }

  def portefeuille(num_page: Int) = Action {implicit request =>
    val liste_real = liste_realisations("realisations")
    val max_page = (liste_real.size - 1) / 9 + 1
    Ok(views.html.portefeuille(
      num_page,
      max_page,
      liste_real.map(x => {(x._1, x._2, x._3)}).drop((num_page - 1) * 9).take(9)
    ))
  }

  def realisation(real: String) = Action { implicit request =>

    val fic_desc = new File(chemin_appli + "/contenu/realisations/" + real + "/description.html")
    val description = {
      if (fic_desc.exists)
        Html(Source.fromFile(fic_desc).mkString)
      else
        Html("<h2>Description manquante</h2>")
    }

    val fic_liees = new File(chemin_appli + "/contenu/realisations/" + real + "/realisations_liees.txt")
    val filtre_real = {
      if (fic_liees.exists)
        ".*" + Source.fromFile(fic_liees).getLines.mkString(".*|.*") +".*"
      else
        "aucune"
    }

    Ok(views.html.realisation(
      construit_carousel("contenu/realisations/" + real),
      description,
      liste_realisations("realisations").map(_._1).filter(_.matches(filtre_real))))
  }
}