# Sugina

_Sugina is the main back end for the Sugina Development Group website, and an open-source project built for our bright futures_

## Configuration

`.secret.json` is the configuration file. Its format is defined in `lib/Secret.hs`.

## Build and Run

* **Prerequisite**: [Haskell Stack](https://www.haskellstack.org/)
* **Build (Once)**: setup `.secret.json` correctly, then `make bootstrap`
* **Build (Many Times)**: `stack build`
* **Run**: `stack exec -- sugina`

## Project Structure

* `lib/`
* `src/`
* `utils/`: scripts for bootstrap

## APIs

| URI | Method | Path Parameter / Request Body | Return Value | Defined in | Auth |
| :- | :- | :- | :- | :- | :- |
| `/username` | **GET** | / | json, a nullable string | `src/Handler/UserName.hs` | / |
| `/isadmin` | **GET** | / | json, `true` if is administrator, `null` if not logged in | `src/Handler/IsAdmin.hs` | / |
| `/users` | **GET** | / | json array | `src/Handler/Users.hs` | Admin |
| `/dictum` | **GET** | / | text | `src/Handler/Dictum.hs` | / |
| `/kunyomi` | **GET** | the query word | json, an array of all the yomikata | `src/Handler/Kunyomi.hs` | / |
| `/board/message` | **GET** | / | all messages sent by current user | `src/Handler/Board.hs` | User |
| `/board/message` | **POST** | json String | json, `true` | `src/Handler/Board.hs` | User |
| `/board/manage` | **GET** | / | all previous messages | `src/Handler/Board.hs` | Admin |
| `/board/manage` | **POST** | `{ boardId: Int, reply: String }` | json, `true` | `src/Handler/Board.hs` | Admin |
| `/pridyn/kakitsubata/index.csv` | **GET** | / | administrator posts, (a) html file name, (b) title | `~/path/to/pridyn/kakitsubata/Makefile` | Admin |
