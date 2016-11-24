{-
   Copyright 2016 Matt Sicker

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onSubmit)
import Html.Lazy exposing (lazy)
import Markdown exposing (toHtml)
import Http
import Json.Decode exposing (Decoder, map3, field, string)
import Json.Encode as Encode exposing (Value)
import CDN exposing (bootstrap)


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
  | UpdatePost
  | LoadUpdatedPost (Result Http.Error String)

update : Msg -> BlogPost -> (BlogPost, Cmd Msg)
update msg post =
  case Debug.log "message" msg of
    ChangeId id ->
      ({ post | id = id }, Cmd.none)
    GetPost ->
      (post, getBlogPost post.id)
    LoadPost (Ok loadedPost) ->
      ({ loadedPost | id = post.id }, Cmd.none)
    LoadPost (Err _) ->
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
    LoadNewPost (Err _) ->
      (post, Cmd.none)
    UpdatePost ->
      (post, updateBlogPost post)
    LoadUpdatedPost (Ok _) ->
      (post, getBlogPost post.id)
    LoadUpdatedPost (Err _) ->
      (post, Cmd.none)

-- this corresponds to the base URL for the blog microservice depending on which
-- implementation you use (the Reactor one is on port 8080, the Lagom one is on
-- port 9000)
baseUrl : String
baseUrl = "http://localhost:8080/api/blog/"

getBlogPost : String -> Cmd Msg
getBlogPost id =
  let
      url = baseUrl ++ id
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
      url = baseUrl
      body = encodeBlogPost post
      request = Http.post url body string
  in
     Http.send LoadNewPost request

updateBlogPost : BlogPost -> Cmd Msg
updateBlogPost post =
  let
      url = baseUrl ++ post.id
      body = encodeBlogPost post
      request = httpPut url body Http.expectString
  in
     Http.send LoadUpdatedPost request

encodeBlogPost : BlogPost -> Http.Body
encodeBlogPost post =
  Http.jsonBody
    <| Encode.object
        [ ("title", Encode.string post.title)
        , ("author", Encode.string post.author)
        , ("body", Encode.string post.body)
        ]

-- it'd be nice if there was an Http.put function
httpPut : String -> Http.Body -> Http.Expect a -> Http.Request a
httpPut url body expect =
  Http.request
    { method = "PUT"
    , headers = []
    , url = url
    , body = body
    , expect = expect
    , timeout = Nothing
    , withCredentials = False
    }

-- VIEW

view : BlogPost -> Html Msg
view post =
  div [ class "container" ]
    [ bootstrap.css
    , bootstrap.theme
    , div [ class "row" ]
        [ section [ class "col-md-6" ]
            [ article [ class "view-post" ]
                [ header
                    [ class "page-header"
                    , style [ ("min-height", "60px") ]
                    ]
                    [ h1 [] (formatTitle post)
                    ]
                , lazy parseMarkdown post.body
                ]
            , Html.form [ class "get-post", onSubmit GetPost ]
                [ div [ class "input-group" ]
                    [ label [ class "input-group-addon", for "blog-id" ] [ text "id" ]
                    , input
                        [ class "form-control"
                        , id "blog-id"
                        , type_ "text"
                        , onInput ChangeId
                        , value post.id
                        , placeholder "12345678-1234-1234-1234-1234567890ab"
                        ] []
                    , span [ class "input-group-btn" ]
                        [ button [ class "btn btn-default", onClick GetPost ] [ text "get post" ] ]
                    ]
                ]
            ]
        , section [ class "edit-post col-md-6" ]
            [ h2 [] [ text "Edit Post" ]
            , input [ class "form-control", type_ "text", onInput ChangeTitle, value post.title, placeholder "title" ] []
            , input [ class "form-control", type_ "text", onInput ChangeAuthor, value post.author, placeholder "author" ] []
            , textarea [ rows 20, cols 76, onInput ChangeBody ] [ text post.body ]
            , div [ class "btn-group" ]
                [ button [ class "btn btn-default", onClick AddPost ] [ text "add post" ]
                , button [ class "btn btn-default", onClick UpdatePost ] [ text "update post" ]
                ]
            ]
        ]
    ]

formatTitle : BlogPost -> List (Html Msg)
formatTitle post =
  if String.isEmpty post.title then
    [ text "" ]
  else
    [ text <| post.title ++ " "
    , small []
        [ text <| "by " ++ post.author
        ]
    ]

parseMarkdown : String -> Html Msg
parseMarkdown =
  toHtml []


-- SUBSCRIPTIONS

subscriptions : BlogPost -> Sub Msg
subscriptions model =
  Sub.none
