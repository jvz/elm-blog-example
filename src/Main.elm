import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Lazy exposing (lazy)
import Markdown exposing (toHtml)
import Http
import Json.Decode exposing (Decoder, map3, field, string)
import Json.Encode as Encode exposing (Value)


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

init : (BlogPost, Cmd Msg)
init =
  (BlogPost "" "" "" "", Cmd.none)


-- UPDATE

type Msg
  = ChangeId String
  | GetPost
  | LoadPost (Result Http.Error BlogPost)
  | ChangeTitle String
  | ChangeAuthor String
  | ChangeBody String
  | AddPost
  | LoadNewPost (Result Http.Error String)
--| UpdatePost

update : Msg -> BlogPost -> (BlogPost, Cmd Msg)
update msg post =
  case Debug.log "message" msg of
    ChangeId id ->
      ({ post | id = id }, Cmd.none)
    GetPost ->
      (post, getBlogPost post.id)
    LoadPost (Ok post) ->
      ({ post | id = post.id }, Cmd.none)
    LoadPost (Err error) ->
      (post, Cmd.none)
    ChangeTitle title ->
      ({ post | title = title }, Cmd.none)
    ChangeAuthor author ->
      ({ post | author = author }, Cmd.none)
    ChangeBody body ->
      ({ post | body = body }, Cmd.none)
    AddPost ->
      (post, addBlogPost post)
    LoadNewPost (Ok id) ->
      ({ post | id = id }, getBlogPost id)
    LoadNewPost (Err error) ->
      (post, Cmd.none)

getBlogPost : String -> Cmd Msg
getBlogPost id =
  let
      url = "http://localhost:8080/api/blog/" ++ id
      request = Http.get url (blogPostDecoder id)
  in
     Http.send LoadPost request

blogPostDecoder : String -> Decoder BlogPost
blogPostDecoder id =
  map3 (BlogPost id)
    (field "title" string)
    (field "author" string)
    (field "body" string)

addBlogPost : BlogPost -> Cmd Msg
addBlogPost post =
  let
      url = "http://localhost:8080/api/blog/"
      body = Http.jsonBody <| encodeBlogPost post
      request = Http.post url body string
  in
     Http.send LoadNewPost request

encodeBlogPost : BlogPost -> Value
encodeBlogPost post =
  -- this really looks redundant compared to an automatic json serializer
  Encode.object
    [ ("title", Encode.string post.title)
    , ("author", Encode.string post.author)
    , ("body", Encode.string post.body)
    ]

-- VIEW

view : BlogPost -> Html Msg
view post =
  div []
    [ div [ class "get-post" ]
        [ div [] [ input [ type_ "text", onInput ChangeId, value post.id, placeholder "id" ] [] ]
        , submitButton "get" GetPost
        ]
    , hr [] []
    , div [ class "view-post" ]
        [ h1 [] [ text post.title ]
        , h2 [] [ text <| "By " ++ post.author ]
        , article [] [ lazy parseMarkdown post.body ]
        ]
    , hr [] []
    , div [ class "add-post" ]
        [ h2 [] [ text "Add New Post" ]
        , div [] [ input [ type_ "text", onInput ChangeTitle, value post.title, placeholder "title" ] [] ]
        , div [] [ input [ type_ "text", onInput ChangeAuthor, value post.author, placeholder "author" ] [] ]
        , textarea [ rows 20, cols 80, onInput ChangeBody ] [ text post.body ]
        , submitButton "add post" AddPost
        ]
    ]

textInput : String -> (String -> Msg) -> Html Msg
textInput placeholderText msg =
  div []
    [ input [ type_ "text", onInput msg, placeholder placeholderText ] []
    ]

submitButton : String -> Msg -> Html Msg
submitButton buttonText msg =
  div []
    [ button [ onClick msg ] [ text buttonText ]
    ]

parseMarkdown : String -> Html Msg
parseMarkdown =
  toHtml []


-- SUBSCRIPTIONS

subscriptions : BlogPost -> Sub Msg
subscriptions model =
  Sub.none
