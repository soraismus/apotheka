package users.controllers

import commons.services.ActionRunner
import authentication.api._
import authentication.models.{JwtToken, SecurityUserId, SecurityUserIdProfile}
import commons.controllers.RealWorldAbstractController
import users.models._
import users.services.{UserRegistrationService, UserService}
import play.api.libs.json._
import play.api.mvc._

class UserController(authenticatedAction: AuthenticatedActionBuilder,
                     dbioRunner: ActionRunner,
                     userRegistrationService: UserRegistrationService,
                     userService: UserService,
                     jwtAuthenticator: TokenGenerator[SecurityUserIdProfile, JwtToken],
                     components: ControllerComponents)
  extends RealWorldAbstractController(components) {

  def update: Action[UpdateUserWrapper] =
    authenticatedAction.async(validateJson[UpdateUserWrapper]) {
      request =>
        val email = request.user.email
        val user = request.body.user
        val dbio = userService.update(email, user)
        dbioRunner
          .runTransactionally(dbio)
          .map(userDetails => UserDetailsWithToken(userDetails, request.user.token))
          .map(UserDetailsWithTokenWrapper(_))
          .map(Json.toJson(_))
          .map(Ok(_))
          .recover(handleFailedValidation)
    }

  def getCurrentUser: Action[AnyContent] =
    authenticatedAction.async {
      request =>
        val email = request.user.email
        val dbio = userService.getUserDetails(email)
        dbioRunner
          .runTransactionally(dbio)
          .map(userDetails => UserDetailsWithToken(userDetails, request.user.token))
          .map(UserDetailsWithTokenWrapper(_))
          .map(Json.toJson(_))
          .map(Ok(_))
    }

  def register: Action[UserRegistrationWrapper] =
    Action.async(validateJson[UserRegistrationWrapper]) {
      request =>
        val user = request.body.user
        val dbio = userRegistrationService.register(user)
        dbioRunner
          .runTransactionally(dbio)
          .map(userAndSecurityUserId => {
            val (user, securityUserId) = userAndSecurityUserId
            val jwtToken: JwtToken = generateToken(securityUserId)
            UserDetailsWithToken(
              user.email,
              user.username,
              user.createdAt,
              user.updatedAt,
              user.bio,
              user.image,
              jwtToken.token)
          })
          .map(UserDetailsWithTokenWrapper(_))
          .map(Json.toJson(_))
          .map(Ok(_))
          .recover(handleFailedValidation)
    }

  private def generateToken(securityUserId: SecurityUserId) = {
    val profile = SecurityUserIdProfile(securityUserId)
    val jwtToken = jwtAuthenticator.generate(profile)
    jwtToken
  }

}

/*
package authentication.api
trait Profile
package authentication.api
trait TokenGenerator[T0 <: Profile, T1] { // type TokenGenerator a b = Profile a => a -> b
  def generate(profile: T0): T1
}
package authentication.pac4j.services
private[authentication] class JwtTokenGenerator
  ( dateTimeProvider: DateTimeProvider
  , jwtGenerator: JwtGenerator[CommonProfile]
  )
  extends TokenGenerator[SecurityUserIdProfile, JwtToken] {

  override def generate(profile: SecurityUserIdProfile): JwtToken = {
    var expiredAt = dateTimeProvide.now.pus(tokenDuration)
    val rawToken = jwtGenerator.generate(
      buildProfile(
        profile.securityUserId.value,
        expiredAt))
    JwtToken(rawToken, expiredAt)
  }
}
? Action
? DBIO
class ActionRunner(dbConfigHelper: commons.respositories.DbConfigHelper) {
  import dbConfigHelper.driver._ // Everything in the dbConfig profile.
  def run[A](action: DBIO[A]): scala.concurrent.Future[A] =
    dbConfigHelper.db.run(action)
  def runTransactionally[A](action: DBIO[A]): scala.concurrent.Future[A] =
    run(action.transactionally)
}
package commons.repositories
class DbConfigHelper(dbConfigProvider: play.api.db.slick.DatabaseConfigProvider) {
  private val dbConfig: DatabaseConfig[JdbcProfile] =
    dbConfigProvider.get[JdbcProfile]
  val db: JdbcBackend#DatabaseDef = dbConfig.db
  val driver: JdbcProfile = dbConfig.profile
*/
