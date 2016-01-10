package controllers

import controllers.Application._
import play.api.Play.current
import play.api._
import play.api.mvc._
import play.twirl.api.Html
import play.api.libs.json._
import play.api.libs.json
import com.github.nscala_time.time.Imports._
import org.joda.time.Instant

import play.api.Play.current
import play.api.libs.ws._
import play.api.libs.ws.ning.NingAsyncHttpClientConfigBuilder
import scala.concurrent.Future

import play.api.mvc._
import scala.io.Source
import scala.concurrent.Await
import akka.util.Timeout
import scala.util.Try
import play.api.data._
import play.api.data.Forms._
import scala.util.{Try,Success,Failure}
import play.api.libs.mailer._



object Contact extends Controller {

  implicit val context = play.api.libs.concurrent.Execution.Implicits.defaultContext
  val duree_rdv_mn = Play.current.configuration.getInt("rendez-vous.duree").getOrElse(120)
  val coord = (Play.current.configuration.getString("telephone").getOrElse(""),
    Play.current.configuration.getString("email").getOrElse(""),
    Play.current.configuration.getString("horaires").getOrElse(""))

  case class Creneau(date_debut: DateTime){
    def toTexte: String = date_debut.toString
    def toLibelle: String = {
      " " + Seq("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi","Dimanche")(date_debut.dayOfWeek.get - 1) + " " +
        date_debut.dayOfMonth.get+" " +
        Seq("janvier", "février", "mars", "avril", "mai", "juin", "juillet", "août", "septembre", "octobre", "novembre", "décembre")(date_debut.monthOfYear.get - 1) + " " +
        date_debut.year.get + " à " +
        date_debut.hourOfDay.get+" heures " +
        (if(date_debut.minuteOfHour.get.toString != "0") date_debut.minuteOfHour.get else "")
    }
  }

  object AgendaTonio {
    def plages_libres: Array[Interval] = {
      val maintenant = DateTime now
      val debut = maintenant plusHours 2  withMinute 0 withSecond 0 withMillisOfSecond 0
      val fin = debut plusMonths(2)
      val json_request: JsValue = Json parse(
        "{\"items\": [{\"id\": \"9ua7u1mtcenp7ha665e7kpktmo@group.calendar.google.com\"}],\"timeMin\": \""
          + debut +
          "\",\"timeMax\": \""
          + fin +
          "\",\"timeZone\": \"UTC+01:00\"}")
      var tableau_occupe = new Array[Interval](0)
      val futureResponse: Future[Seq[JsValue]] = WS.url("https://www.googleapis.com/calendar/v3/freeBusy?fields=calendars%2Cgroups%2Ckind%2CtimeMax%2CtimeMin&key=AIzaSyCvvurPk_6StASYk_eNeXj3sWYOhx6pYm4")
        .post(json_request).map(response => (response.json \ "calendars" \\ "busy"))
      Try(Await.result(futureResponse, scala.concurrent.duration.Duration(5, "seconds")))
      if (futureResponse.isCompleted) {
        val reponse: Seq[JsValue] = futureResponse.value.get.get
        val liste_occupe = reponse.map(_.as[Array[Map[String, String]]])
        tableau_occupe = liste_occupe(0).map(x => new Interval(new DateTime(x("start")), new DateTime(x("end"))))
      } else {
        tableau_occupe = Array(new Interval(debut, fin))}
      tableau_occupe +:= new Interval(debut, debut)
      tableau_occupe :+= new Interval(fin, fin)

      var tableau_libre = new Array[Interval] (0)
      for (i <- tableau_occupe.indices.dropRight(1)) {
        if (!tableau_occupe(i).overlaps(tableau_occupe(i + 1)) &&
          !tableau_occupe(i).abuts(tableau_occupe(i + 1)) &&
          tableau_occupe(i).gap(tableau_occupe(i +1)).toDurationMillis >= duree_rdv_mn*60000) {
          tableau_libre :+= tableau_occupe(i).gap(tableau_occupe(i + 1))
        }
      }
     tableau_libre
    }
  }

  def message = Action { implicit request =>
    val nom = request.body.asInstanceOf[AnyContentAsFormUrlEncoded].data.get("name").get.head
    val telephone = request.body.asInstanceOf[AnyContentAsFormUrlEncoded].data.get("phone").get.head
    val email = request.body.asInstanceOf[AnyContentAsFormUrlEncoded].data.get("email").get.head
    val message = request.body.asInstanceOf[AnyContentAsFormUrlEncoded].data.get("message").get.head
    val rdv = request.body.asInstanceOf[AnyContentAsFormUrlEncoded].data.get("rdv").get.head
    var message_complet = nom + "\nTéléphone: " + telephone + "\nMail: " + email + "\nRendez-vous demandé:"
    if (rdv == "") message_complet += " aucun" else message_complet += Creneau(new DateTime(rdv)).toLibelle
    message_complet += "\n\n" + message

    Try(MailerPlugin.send(Email(
      "Prise de contact",
      nom + "<" + email +">",
      Seq("Milenioconcept <contact@milenioconcept.fr>"),
      bodyText = Some(message_complet)
    ))) match {

      case Success(s) => Ok("Votre message a bien été envoyé.")
      case Failure(f) => {BadRequest("")}
    }
  }

  def edit(focus_message: Boolean) = Action { implicit request =>

    var creneaux = new Array[(String, String)](0)
    var heures = new Array[DateTime](0)
    AgendaTonio.plages_libres.foreach(x => {
      var intervalle = x
      while (intervalle.toDurationMillis >= duree_rdv_mn*60000) {
        val creneau = Creneau(intervalle.getStart)
        creneaux :+= (creneau.toTexte, creneau.toLibelle)
        heures :+= intervalle.getStart
        intervalle = intervalle.withStart(intervalle.getStart + duree_rdv_mn.minutes)
      }
    })
    Ok(views.html.contact(coord, creneaux, focus_message))
  }
}