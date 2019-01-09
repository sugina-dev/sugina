# Sugina

_Sugina is the open-source project built for our bright futures_

## Build and Run

**Prerequisites**

* [Stack](https://www.haskellstack.org/)
* make

**Build (Once)**

* Setup `.secret.json` file correctly (See section _Secret_)

``` bash
$ git submodule init
$ git submodule update
$ make bootstrap
```

**Build (Many Times)**

* `stack build`

**Run**

* `stack exec -- sugina`

## APIs

| URI | Method | Path Parameter / Request Body | Return Value | Defined in | Auth |
| :- | :- | :- | :- | :- | :- |
| `/api/username` | **GET** | / | json, a nullable string | `src/Handler/UserName.hs` | / |
| `/api/isadmin` | **GET** | / | json, `true` if is administrator, `null` if not logged in | `src/Handler/IsAdmin.hs` | / |
| `/api/users` | **GET** | / | json array | `src/Handler/Users.hs` | Admin |
| `/api/dictum` | **GET** | / | text | `src/Handler/Dictum.hs` | / |
| `/api/kunyomi` | **GET** | the query word | json, an array of all the yomikata | `src/Handler/Kunyomi.hs` | / |
| `/api/board/message` | **GET** | / | all messages sent by current user | `src/Handler/Board.hs` | User |
| `/api/board/message` | **POST** | json String | json, `true` | `src/Handler/Board.hs` | User |
| `/api/board/manage` | **GET** | / | all previous messages | `src/Handler/Board.hs` | Admin |
| `/api/board/manage` | **POST** | `{ boardId: Int, reply: String }` | json, `true` | `src/Handler/Board.hs` | Admin |
| `/font/AlexBrush-Regular.ttf` | **GET** | / | Alex Brush font (under SIL Open Font License) | `pubsta/font/AlexBrush-Regular.ttf` | / |
| `/pubdyn/transform.css` | **GET** | / | global style sheet | `pubdyn/transform.css` | / |
| `/pridyn/kakitsubata/index.csv` | **GET** | / | administrator posts, (a) html file name, (b) title | `~/path/to/pridyn/kakitsubata/Makefile` | Admin |
| `/pubdyn/offprint/index.csv` | **GET** | / | public posts, (a) html file name, (b) title | `pubdyn/offprint/Makefile` | / |

## Secret

Secret is a self-defined data structure to store private server data. Its format is defined in `lib/Secret.hs`. The file `.secret.json` should present to start the server. The whole file should be a JSON object.
