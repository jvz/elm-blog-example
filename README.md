This sample Elm app uses the microservice developed [here][service].

To run:

1. Clone the [microservice][service].
2. Run `mvn spring-boot:run` in that repository. This starts up the service on port 8080.
3. Run `elm reactor` in this repository. This starts up the UI on port 8000.
4. Navigate to [http://localhost:8000/src/Main.elm](http://localhost:8000/src/Main.elm).

[service]: https://github.com/jvz/reactor-spring5-example
