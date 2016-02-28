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

  def singulier(titre: String): String = titre match {
    case "services/realisations" => "Réalisation"
    case "services/cuisines" => "Cuisine"
    case "services/salles_de_bain" => "Salle de bain"
    case "services/rangements" => "Rangement"
    case "realisations" => "Réalisation"
    case _ => titre
  }

  def pluriel(titre: String): String = titre match {
    case "services/realisations" => "Réalisations"
    case "services/cuisines" => "Cuisines"
    case "services/salles_de_bain" => "Salles de bain"
    case "services/rangements" => "Rangements"
    case "realisations" => "Réalisations"
    case _ => titre
  }

  def liste_items(place: String) : Seq[(String, String, String, Html)] = {
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
      liste_items("realisations").map(_._1).filter(x => {
        val nom = x.split('/').drop(2)(0)
        nom.toUpperCase == nom
      })))
  }

  def about = Action { implicit request =>
    val fic_a_propos = new File(s"$chemin_appli/contenu/a_propos/a_propos.html")
    val a_propos = if (fic_a_propos.exists) Html(Source.fromFile(fic_a_propos).mkString) else Html("")
    Ok(views.html.about(
      a_propos,
      construit_carousel("contenu/a_propos")))
  }

  def portefeuille(type_port: String, num_page: Int) = Action {implicit request =>
    val liste = liste_items(type_port)
    val max_page = (liste.size - 1) / 9 + 1
    Ok(views.html.portfolio(
      pluriel(type_port),
      type_port,
      num_page,
      max_page,
      liste.map(x => {
        (x._1, x._2, x._3)
      }).slice((num_page - 1) * 9, (num_page - 1) * 9 + 9)
    ))
  }

  def item_portefeuille(type_port: String, real: String) = Action { implicit request =>

    val fic_desc = new File(chemin_appli + s"/contenu/$type_port/" + real + "/description.html")
    val description = {
      if (fic_desc.exists)
        Html(Source.fromFile(fic_desc).mkString)
      else
        Html("<h2>Description manquante</h2>")
    }

    val fic_liees = new File(chemin_appli + s"/contenu/$type_port/" + real + "/realisations_liees.txt")
    val filtre_real = {
      if (fic_liees.exists)
        ".*" + Source.fromFile(fic_liees).getLines.mkString(".*|.*") +".*"
      else
        "aucune"
    }

    Ok(views.html.item_portfolio(
      singulier(type_port),
      type_port,
      construit_carousel(s"contenu/$type_port/" + real),
      description,
      liste_items(type_port).map(_._1).filter(_.matches(filtre_real))))
  }
}