# Sugina

_Sugina is the open-source project built for our bright futures._

## Build and Run

**Prerequisites**

* [Stack](https://www.haskellstack.org/)
* make

**Build (Once)**

* Setup `.secret.json` file correctly (See section _Secret_)
* `make bootstrap`

**Build (Many Times)**

* `stack build`

**Run**

* `stack exec -- sugina`

## APIs

| URI | Method | Path Parameter / Request Body | Return Value | Defined in |
| :- | :- | :- | :- | :- |
| `/api/username` | **GET** | / | json, a nullable string | `src/Handler/UserName.hs` |
| `/api/isadmin` | **GET** | / | json, `true` if is administrator, `null` if not logged in | `src/Handler/IsAdmin.hs` |
| `/api/dictum` | **GET** | / | text | `src/Handler/Dictum.hs` |
| `/api/kunyomi` | **GET** | the query word | an array of all the yomikata | `src/Handler/Kunyomi.hs` |
| `/api/board/message` | **GET** | / | all messages sent by current user | `src/Handler/Board.hs` |
| `/api/board/message` | **POST** | json String | json, `true` | `src/Handler/Board.hs` |
| `/api/board/manage` | **GET** | / | all previous messages | `src/Handler/Board.hs` |
| `/api/board/manage` | **POST** | `{ boardId: Int, reply: String }` | json, `true` | `src/Handler/Board.hs` |
| `/font/AlexBrush-Regular.ttf` | **GET** | / | Alex Brush font (under SIL Open Font License) | `pubsta/font/AlexBrush-Regular.ttf` |
| `/pubdyn/transform.css` | **GET** | / | global style sheet | `pubdyn/transform.css` |

## Secret

The `.secret.json` file is used for the server to set password and other stuff that could not be open to the public. The whole file should be a JSON object, and the format should be identical with the format defined in `lib/Secret.hs`.
