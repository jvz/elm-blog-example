import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Http
import Json.Decode exposing (Decoder, map3, field, string)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias BlogPost =
  { id : String
  , title : String
  , author : String
  , body : String
  }

-- in our REST API, we don't return the ID in the response body, so we add
-- this function for convenience
postContent : String -> String -> String -> BlogPost
postContent =
  BlogPost ""

init : (BlogPost, Cmd Msg)
init =
  (BlogPost "" "" "" "", Cmd.none)


-- UPDATE

type Msg
  = GetPost
  | LoadPost (Result Http.Error BlogPost)
  | ChangeId String
--| AddPost
--| UpdatePost

update : Msg -> BlogPost -> (BlogPost, Cmd Msg)
update msg post =
  case Debug.log "message" msg of
    GetPost ->
      (post, getBlogPost post.id)
    LoadPost (Ok content) ->
      ({ content | id = post.id }, Cmd.none)
    LoadPost (Err error) ->
      (post, Cmd.none)
    ChangeId id ->
      ({ post | id = id }, Cmd.none)

getBlogPost : String -> Cmd Msg
getBlogPost id =
  let
      url = "http://localhost:8080/api/blog/" ++ id
      request = Http.get url blogPostDecoder
  in
     Http.send LoadPost request

blogPostDecoder : Decoder BlogPost
blogPostDecoder =
  map3 postContent
    (field "title" string)
    (field "author" string)
    (field "body" string)


-- VIEW

view : BlogPost -> Html Msg
view post =
  div []
    [ div []
        [ input [ type_ "text", onInput ChangeId, placeholder "blog id" ] []
        , button [ onClick GetPost ] [ text "get" ]
        ]
    , div []
        [ h1 [] [ text post.title ]
        , h2 [] [ text <| "By " ++ post.author ]
        , div [] [ text post.body ]
        ]
    ]


-- SUBSCRIPTIONS

subscriptions : BlogPost -> Sub Msg
subscriptions model =
  Sub.none
