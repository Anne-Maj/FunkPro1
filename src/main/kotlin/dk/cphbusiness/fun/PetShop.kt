package dk.cphbusiness.`fun`

import io.ktor.application.*
import io.ktor.features.*
import io.ktor.gson.*
import io.ktor.http.*
import io.ktor.request.*
import io.ktor.response.*
import io.ktor.routing.*
import io.ktor.server.engine.*
import io.ktor.server.netty.*

data class Address(
    val street: String,
    val city: String
    )

data class Greeter(
    val name: String,
    val greeting: String,
    val age: Int,
    val address: Address
    )

fun main() {
    embeddedServer(Netty, port = 4711) {
        install(ContentNegotiation) {
          gson()
          }
        install(CORS) {
            method(HttpMethod.Get)
            method(HttpMethod.Post)
            anyHost()
            allowNonSimpleContentTypes = true
            }
        routing {
            get("/") {
                println("GET: / called")
                Thread.sleep(500)
                call.respondText("""{ "greeting": "Hello ${System.nanoTime()}", "name": "Anders", "age": 61, "address": { "street": "Bygaden 7", "city": "Paris" } }""")
                }
            get("/greeter") {
                println("GET: /greeter called")
                call.respond(
                     Greeter(
                        "Kurt",
                        "Hello ${System.currentTimeMillis()}",
                        34,
                        Address("Rue de Paix", "Liege")
                        )
                    )
                }
            post("/saveGreeting") {
              println("POST: /saveGreeting called")
              val greeter = call.receive<Greeter>()
              println(greeter)
              call.respond("Also ${greeter.greeting} to you ${greeter.name}")
              }
            }
        }.start(wait = true)
    }