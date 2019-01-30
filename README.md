# Sugina

_Sugina is the main back end for the Sugina Development Group website, and an open-source project built for our bright futures_

## Configuration

`.secret.json` is the configuration file. Its detailed format is defined in `lib/Secret.hs`.

### Protogenic Users

To ensure that there are some users that exists before the premiere of the server program, their user names and passwords should be set in `getHardcodedUsers`.

### GitLab Client

A GitLab application is needed to support GitHub login. `getGitLabClientId` should be the application ID, while `getGitLabClientSecret` should be the secret of the application.

Callback URL of the application should be `<App Root>/auth/page/gitlab/callback`.

### App Root

For `getApproot`, without the trailing `/`.

## Build and Run

* **Prerequisite**: [Haskell Stack](https://www.haskellstack.org/)
* **Build** (Once): setup `.secret.json` correctly, then `stack build && stack runhaskell -- -Wall -Werror utils/emplacedb.hs`
* **Build** (Many Times): `stack build`
* **Run**: `stack exec -- sugina`

## Project Structure

* `lib/`: modules which are used by other modules
* `src/`: source codes to compiled into binaries
* `utils/`: scripts for bootstrap

## APIs

| URI | Method | Path Parameter / Request Body | Return Value | Defined in | Auth |
| :- | :- | :- | :- | :- | :- |
| `/isuser` | **GET** | / | json, the user name | `src/Handler/UserName.hs` | User |
| `/isadmin` | **GET** | / | json, `true` | `src/Handler/IsAdmin.hs` | Admin |
| `/users` | **GET** | / | json array | `src/Handler/Users.hs` | Admin |
| `/dictum` | **GET** | / | text | `src/Handler/Dictum.hs` | / |
| `/kunyomi` | **GET** | the query word | json, an array of all the yomikata | `src/Handler/Kunyomi.hs` | / |
| `/board/message` | **GET** | / | all messages sent by current user | `src/Handler/Board.hs` | User |
| `/board/message` | **POST** | json String | json, `true` | `src/Handler/Board.hs` | User |
| `/board/manage` | **GET** | / | all previous messages | `src/Handler/Board.hs` | Admin |
| `/board/manage` | **POST** | `{ boardId: Int, reply: String }` | json, `true` | `src/Handler/Board.hs` | Admin |

Unauthorized users will receive 403.
